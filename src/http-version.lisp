(in-package #:http-protocol)

;;; HTTP version preference & H2 message policy.
;;;
;;; Layering (CLOS split):
;;;   http-protocol  — preference keywords, ALPN offer/map (RFC 7301),
;;;                    HTTP/2 header field rules (RFC 9113 §8.2–8.3),
;;;                    capability query on BACKEND.
;;;   backends       — TLS ALPN, RFC 9113 framing/HPACK (or OS stack),
;;;                    fill RESPONSE-HTTP-VERSION with what was negotiated.
;;;
;;; Wire framing, stream IDs, flow control, HPACK tables → backends only.
;;; Push promise (RFC 9113 §8.4) and h2c prior-knowledge → P2.

(defparameter *valid-http-versions*
  '(:auto :http/1.1 :http/2)
  "Preference / negotiated keywords. :auto = prefer 2 when backend can (httpx).")

(defparameter *http2-connection-specific-headers*
  '("connection" "keep-alive" "proxy-connection" "transfer-encoding"
    "upgrade" "http2-settings" "host" "te")
  "RFC 9113 §8.2.2 — MUST NOT appear as HTTP/2 fields (Host → :authority).
   TE is stripped here for wave-1; RFC allows TE: trailers only.")

(defun normalize-http-version (value &key (default :auto))
  "Coerce VALUE to :auto | :http/1.1 | :http/2.

   Accepts NIL (→ DEFAULT), keywords, ALPN tokens (\"h2\"), and
   HTTP-version strings (\"HTTP/1.1\", \"2\")."
  (cond
    ((null value) default)
    ((member value *valid-http-versions* :test #'eq) value)
    ((stringp value)
     (let ((s (string-downcase (string-trim '(#\Space #\Tab) value))))
       (cond
         ((or (string= s "http/1.1") (string= s "1.1")
              (string= s "http/1.0") (string= s "1.0")
              (string= s "http/1"))
          :http/1.1)
         ((or (string= s "http/2") (string= s "http/2.0")
              (string= s "2") (string= s "2.0") (string= s "h2"))
          :http/2)
         ((or (string= s "auto") (string= s ""))
          :auto)
         (t (error 'http-protocol-error
                   :message (format nil "Unknown HTTP version ~S" value))))))
    ((realp value)
     (cond ((or (= value 1) (= value 1.1)) :http/1.1)
           ((or (= value 2) (= value 2.0)) :http/2)
           (t (error 'http-protocol-error
                     :message (format nil "Unknown HTTP version ~S" value)))))
    (t (error 'http-protocol-error
              :message (format nil "Unknown HTTP version ~S" value)))))

(defun http-version-preference-p (value)
  (ignore-errors
    (member (normalize-http-version value) *valid-http-versions* :test #'eq)))

(defun effective-http-version (client request)
  "Request :http-version override, else client, else :auto."
  (normalize-http-version
   (or (and request (http-request-http-version request))
       (and client (http-client-http-version client))
       :auto)))

;;; --- RFC 7301 ALPN (protocol policy; backends apply on TLS) ---------------

(defun alpn-protocols-for-version (version)
  "ALPN protocol list to offer (RFC 7301 §3).

   :auto / :http/2 → (\"h2\" \"http/1.1\") — prefer HTTP/2
   :http/1.1       → (\"http/1.1\")
   Cleartext h2c is not ALPN; backends reject forced :http/2 on http:// until P2."
  (ecase (normalize-http-version version)
    ((:auto :http/2) '("h2" "http/1.1"))
    (:http/1.1 '("http/1.1"))))

(defun http-version-from-alpn (alpn)
  "Map selected ALPN identity → :http/2 | :http/1.1 | NIL (RFC 7301 / 9113)."
  (when alpn
    (let ((s (string-downcase alpn)))
      (cond ((string= s "h2") :http/2)
            ((or (string= s "http/1.1") (string= s "http/1.0")) :http/1.1)
            (t nil)))))

(defun ensure-http-version-available (preference negotiated &key backend-name)
  "Enforce PREFERENCE against NEGOTIATED result.

   :auto     — always ok (fallback to 1.1 is fine)
   :http/1.1 — always ok
   :http/2   — requires NEGOTIATED = :http/2 else HTTP-VERSION-NOT-AVAILABLE"
  (let* ((pref (normalize-http-version preference))
         (got (when negotiated
                (normalize-http-version negotiated :default :http/1.1))))
    (when (and (eq pref :http/2) (not (eq got :http/2)))
      (error 'http-version-not-available
             :operation 'http-version
             :requested pref
             :negotiated got
             :message (format nil
                              "HTTP/2 required~@[ (~A)~] but negotiated ~A"
                              backend-name (or got :http/1.1))))
    got))

;;; --- CLOS: what can this backend speak? -----------------------------------

(defgeneric backend-http-versions (backend)
  (:documentation
   "List of HTTP version keywords BACKEND can negotiate (:http/1.1 and/or :http/2).

    Default: (:http/1.1) only. Async/WinHTTP specialize to include :http/2.
    :auto is never listed — it is a preference, not a wire version.")
  (:method ((backend http-backend))
    (declare (ignore backend))
    '(:http/1.1)))

(defgeneric backend-supports-http-version-p (backend version)
  (:documentation
   "True if BACKEND can satisfy VERSION preference.

    :auto → T if backend supports any version.
    :http/1.1 / :http/2 → membership in BACKEND-HTTP-VERSIONS.")
  (:method ((backend http-backend) version)
    (let ((v (normalize-http-version version)))
      (if (eq v :auto)
          (not (null (backend-http-versions backend)))
          (member v (backend-http-versions backend) :test #'eq)))))

;;; --- RFC 9113 §8.2–8.3 header field policy (shared by H2 backends) -------

(defun http2-connection-specific-header-p (name)
  "True if NAME is forbidden on HTTP/2 connections (RFC 9113 §8.2.2).
   HOST is treated as connection-specific here — use :authority instead (§8.3.1)."
  (member (string-downcase (string name))
          *http2-connection-specific-headers*
          :test #'string=))

(defun filter-headers-for-http-version (headers version)
  "Return HEADERS alist suitable for VERSION.

   For :http/2, drop connection-specific fields (RFC 9113 §8.2.2).
   For :http/1.1 / :auto, return HEADERS unchanged (caller still owns Host)."
  (let ((v (normalize-http-version version :default :http/1.1)))
    (if (eq v :http/2)
        (remove-if (lambda (pair)
                     (http2-connection-specific-header-p (car pair)))
                   headers)
        headers)))

(defun http2-authority (host port scheme)
  "RFC 9113 §8.3.1 :authority — host[:port] omitting default ports."
  (let* ((h (string host))
         (default (if (string-equal scheme "https") 443 80)))
    (if (or (null port) (= port default))
        h
        (format nil "~A:~A" h port))))

(defun http2-path (uri)
  "RFC 9113 §8.3.1 :path — absolute path + optional ?query; \"*\" for OPTIONS *."
  (let* ((path (or (quri:uri-path uri) "/"))
         (query (quri:uri-query uri)))
    (if query
        (format nil "~A?~A" path query)
        path)))

(defun make-http2-request-headers (method uri headers &key scheme)
  "Build HTTP/2 request header field list (RFC 9113 §8.3).

   Returns a list of (name . value) where pseudo-headers are keyword names
   in conventional order (:method :scheme :path :authority) — all before
   regular fields (RFC 9113 §8.3); order among pseudos is not mandated —
   followed by lowercase regular fields (connection-specific stripped).

   Backends encode this list with HPACK; protocol does not touch the wire."
  (let* ((scheme* (string-downcase
                   (or scheme (quri:uri-scheme uri) "https")))
         (host (or (quri:uri-host uri)
                   (error 'http-protocol-error
                          :message "HTTP/2 request URL missing host")))
         (port (or (quri:uri-port uri)
                   (if (string-equal scheme* "https") 443 80)))
         (method* (string-upcase (string method)))
         (regular (filter-headers-for-http-version headers :http/2)))
    (append
     (list (cons :method method*)
           (cons :scheme scheme*)
           (cons :path (http2-path uri))
           (cons :authority (http2-authority host port scheme*)))
     (mapcar (lambda (pair)
               (cons (string-downcase (string (car pair)))
                     (princ-to-string (cdr pair))))
             regular))))
