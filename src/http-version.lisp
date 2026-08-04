(in-package #:http-protocol)

;;; HTTP version preference / negotiation (RFC 9113 + RFC 7301 ALPN).
;;; Keywords: :auto | :http/1.1 | :http/2
;;; Response reports the negotiated version the same way.

(defparameter *valid-http-versions*
  '(:auto :http/1.1 :http/2)
  "Allowed preference keywords for CLIENT/REQUEST :http-version.")

(defun normalize-http-version (value &key (default :auto))
  "Coerce VALUE to a preference/negotiated keyword.

   Accepts NIL (→ DEFAULT), keywords, and common strings:
   \"HTTP/1.1\", \"1.1\", \"h2\", \"HTTP/2\"."
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
     (cond ((= value 1) :http/1.1)
           ((= value 1.1) :http/1.1)
           ((= value 2) :http/2)
           ((= value 2.0) :http/2)
           (t (error 'http-protocol-error
                     :message (format nil "Unknown HTTP version ~S" value)))))
    (t (error 'http-protocol-error
              :message (format nil "Unknown HTTP version ~S" value)))))

(defun http-version-preference-p (value)
  "True when VALUE is a recognized preference keyword (after normalize)."
  (ignore-errors
    (member (normalize-http-version value) *valid-http-versions* :test #'eq)))

(defun effective-http-version (client request)
  "Request override, else client default, else :auto."
  (normalize-http-version
   (or (and request (http-request-http-version request))
       (and client (http-client-http-version client))
       :auto)))

(defun alpn-protocols-for-version (version)
  "ALPN offer list for VERSION preference (RFC 7301).

   :auto / :http/2 → (\"h2\" \"http/1.1\")
   :http/1.1 → (\"http/1.1\")"
  (ecase (normalize-http-version version)
    ((:auto :http/2) '("h2" "http/1.1"))
    (:http/1.1 '("http/1.1"))))

(defun http-version-from-alpn (alpn)
  "Map negotiated ALPN protocol string → :http/1.1 | :http/2 | NIL."
  (when alpn
    (let ((s (string-downcase alpn)))
      (cond ((string= s "h2") :http/2)
            ((or (string= s "http/1.1") (string= s "http/1.0")) :http/1.1)
            (t nil)))))

(defun ensure-http-version-available (preference negotiated &key backend-name)
  "Signal HTTP-VERSION-NOT-AVAILABLE when PREFERENCE cannot be satisfied.

   :auto always ok. :http/1.1 always ok (backend may still speak 1.1).
   :http/2 requires NEGOTIATED = :http/2."
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
