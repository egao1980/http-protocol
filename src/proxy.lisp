(in-package #:http-protocol)

;;; Proxy configuration (CLOS). Logic ported from egao1980/dexador
;;; feature/proxy-env-support (PR fukamachi/dexador#202): env seeding,
;;; scheme/host alist, NO_PROXY with IPv4/IPv6/CIDR + hostname suffix.

(defun getenv-nonempty (&rest names)
  (loop for name in names
        thereis (let ((v (uiop:getenv name)))
                  (and v (plusp (length v)) v))))

(defun make-environment-proxy-alist (https http all)
  (unless all
    (cond
      ((and https (null http)) (setf http https))
      ((and http (null https)) (setf https http))))
  (remove nil (list (and https (cons "https" https))
                    (and http (cons "http" http))
                    (and all (cons "*" all)))))

(defun environment-proxy-alist ()
  (make-environment-proxy-alist
   (getenv-nonempty "https_proxy" "HTTPS_PROXY")
   (getenv-nonempty "http_proxy" "HTTP_PROXY")
   (getenv-nonempty "all_proxy" "ALL_PROXY")))

(defun environment-no-proxy ()
  (getenv-nonempty "no_proxy" "NO_PROXY"))

(defclass http-proxy-config ()
  ((proxy :initarg :proxy :accessor proxy-config-proxy
          :initform nil
          :documentation "NIL | URL string | alist of scheme / scheme://host / \"*\" → URL.")
   (no-proxy :initarg :no-proxy :accessor proxy-config-no-proxy
             :initform nil
             :documentation "Comma/space string or list of NO_PROXY patterns.")
   (use-system-proxy :initarg :use-system-proxy :accessor proxy-config-use-system-proxy
                     :initform t
                     :documentation "Hint for backends that can discover OS proxy (WinHTTP)."))
  (:documentation "Proxy resolution policy. Backends call RESOLVE-PROXY."))

(defun http-proxy-config-p (x) (typep x 'http-proxy-config))

(defun make-http-proxy-config (&key (proxy nil proxy-p)
                                 (no-proxy nil no-proxy-p)
                                 (use-system-proxy t)
                                 (from-environment t))
  "Build HTTP-PROXY-CONFIG. When FROM-ENVIRONMENT and slots omitted, seed from env."
  (make-instance 'http-proxy-config
                 :proxy (cond (proxy-p proxy)
                              (from-environment (environment-proxy-alist))
                              (t nil))
                 :no-proxy (cond (no-proxy-p no-proxy)
                                 (from-environment (environment-no-proxy))
                                 (t nil))
                 :use-system-proxy use-system-proxy))

(defvar *default-proxy-config*
  (make-http-proxy-config :from-environment t)
  "Default proxy config (env-seeded). Bind or SETF per process.")

(defun strip-ipv6-brackets (host)
  "Strip RFC 2732 brackets: \"[::1]\" → \"::1\"."
  (if (and (stringp host) (plusp (length host)) (char= (char host 0) #\[))
      (let ((close (position #\] host)))
        (if close (subseq host 1 close) host))
      host))

(defun format-host-port (host port)
  "HOST:PORT with IPv6 hosts in RFC 2732 brackets."
  (let ((host (strip-ipv6-brackets host)))
    (if (find #\: host)
        (format nil "[~A]:~A" host port)
        (format nil "~A:~A" host port))))

(defun %parse-ipv6-bytes (string)
  "Expand IPv6 literal → 16-byte vector, or NIL."
  (let* ((s (strip-ipv6-brackets string))
         (dbl (search "::" s)))
    (labels ((hextets (part)
               (if (zerop (length part))
                   nil
                   (mapcar (lambda (h)
                             (and (<= 1 (length h) 4)
                                  (every (lambda (c)
                                           (digit-char-p c 16))
                                         h)
                                  (parse-integer h :radix 16)))
                           (uiop:split-string part :separator ":"))))
             (ok (list) (and list (every #'integerp list))))
      (multiple-value-bind (left right)
          (if dbl
              (values (hextets (subseq s 0 dbl))
                      (hextets (subseq s (+ dbl 2))))
              (values (hextets s) nil))
        (when (and (or (null left) (ok left))
                   (or (null dbl) (null right) (ok right)))
          (let* ((left (or left '()))
                 (right (or right '()))
                 (n (+ (length left) (length right)))
                 (fill (- 8 n)))
            (when (and (<= 0 fill) (or dbl (zerop fill)) (<= n 8))
              (let ((out (make-array 16 :element-type '(unsigned-byte 8)))
                    (pos 0))
                (flet ((put (v)
                         (setf (aref out pos) (ldb (byte 8 8) v)
                               (aref out (1+ pos)) (ldb (byte 8 0) v))
                         (incf pos 2)))
                  (dolist (v left) (put v))
                  (dotimes (_ fill) (put 0))
                  (dolist (v right) (put v)))
                out))))))))

(defun parse-ip-address (string)
  "Parse IPv4/IPv6 literal → (values address-integer total-bits) or NIL."
  (let ((string (strip-ipv6-brackets string)))
    (if (find #\: string)
        (let ((bytes (%parse-ipv6-bytes string)))
          (when bytes
            (values (reduce (lambda (acc byte) (logior (ash acc 8) byte))
                            bytes :initial-value 0)
                    128)))
        (let ((parts (uiop:split-string string :separator ".")))
          (when (= (length parts) 4)
            (loop with address = 0
                  for part in parts
                  for byte = (and (<= 1 (length part) 3)
                                  (every #'digit-char-p part)
                                  (parse-integer part))
                  unless (and byte (<= byte 255))
                    do (return nil)
                  do (setf address (logior (ash address 8) byte))
                  finally (return (values address 32))))))))

(defun ip-in-network-p (address total-bits pattern)
  (let ((slash (position #\/ pattern)))
    (when slash
      (multiple-value-bind (network network-bits)
          (parse-ip-address (subseq pattern 0 slash))
        (let ((prefix (ignore-errors (parse-integer (subseq pattern (1+ slash))))))
          (and network
               (eql total-bits network-bits)
               (integerp prefix)
               (<= 0 prefix network-bits)
               (let ((shift (- total-bits prefix)))
                 (= (ash address (- shift))
                    (ash network (- shift))))))))))

(defun ip-matches-pattern-p (address total-bits pattern)
  (if (find #\/ pattern)
      (ip-in-network-p address total-bits pattern)
      (multiple-value-bind (pattern-address pattern-bits)
          (parse-ip-address
           (let ((colon (position #\: pattern)))
             ;; Single colon → IPv4:port; multiple → leave as IPv6.
             (if (and colon (not (find #\: pattern :start (1+ colon))))
                 (subseq pattern 0 colon)
                 pattern)))
        (and pattern-address
             (eql total-bits pattern-bits)
             (= address pattern-address)))))

(defun hostname-matches-pattern-p (host pattern)
  (let* ((dotless (string-left-trim "." pattern))
         (pattern (subseq dotless 0 (or (position #\: dotless) (length dotless)))))
    (or (string-equal host pattern)
        (let ((pl (length pattern))
              (hl (length host)))
          (and (plusp pl)
               (> hl pl)
               (char= (char host (- hl pl 1)) #\.)
               (string-equal pattern (subseq host (- hl pl))))))))

(defun host-bypassed-p (host no-proxy)
  "True if HOST matches NO-PROXY patterns (NO_PROXY semantics)."
  (when (and host no-proxy)
    (let ((host (strip-ipv6-brackets host))
          (patterns (if (listp no-proxy)
                        no-proxy
                        (remove "" (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                                           (uiop:split-string no-proxy :separator ", "))
                                :test #'string=))))
      (multiple-value-bind (address total-bits) (parse-ip-address host)
        (some (lambda (pattern)
                (and (plusp (length pattern))
                     (or (string= pattern "*")
                         (if address
                             (ip-matches-pattern-p address total-bits pattern)
                             (hostname-matches-pattern-p host pattern)))))
              patterns)))))

(defun normalize-proxy (proxy)
  "Normalize PROXY to alist form: URL string → ((\"*\" . url))."
  (etypecase proxy
    (null nil)
    (string (list (cons "*" proxy)))
    (list proxy)))

(defun proxy-for-uri (uri proxy-alist)
  "Most specific key wins: scheme://host, scheme, then \"*\"/\"all\"."
  (let* ((u (if (typep uri 'quri:uri) uri (quri:uri uri)))
         (scheme (quri:uri-scheme u))
         (host (and (quri:uri-host u) (strip-ipv6-brackets (quri:uri-host u))))
         (host-key (and scheme host (format nil "~A://~A" scheme host)))
         (bracketed-key (and scheme host (find #\: host)
                             (format nil "~A://[~A]" scheme host))))
    (cdr (or (and host-key (assoc host-key proxy-alist :test #'string-equal))
             (and bracketed-key (assoc bracketed-key proxy-alist :test #'string-equal))
             (and scheme (assoc scheme proxy-alist :test #'string-equal))
             (assoc "*" proxy-alist :test #'string-equal)
             (assoc "all" proxy-alist :test #'string-equal)))))

(defgeneric coerce-proxy-config (x)
  (:documentation "Normalize X → HTTP-PROXY-CONFIG.")
  (:method ((x http-proxy-config)) x)
  (:method ((x null))
    (make-http-proxy-config :proxy nil :no-proxy nil :from-environment nil))
  (:method ((x string))
    (make-http-proxy-config :proxy x :no-proxy (proxy-config-no-proxy *default-proxy-config*)
                            :from-environment nil))
  (:method ((x list))
    (make-http-proxy-config :proxy x :no-proxy (proxy-config-no-proxy *default-proxy-config*)
                            :from-environment nil)))

(defgeneric resolve-proxy (config uri)
  (:documentation "Effective proxy URL string for URI, or NIL (direct).")
  (:method ((config http-proxy-config) uri)
    (let ((u (if (typep uri 'quri:uri) uri (quri:uri uri))))
      (unless (host-bypassed-p (quri:uri-host u) (proxy-config-no-proxy config))
        (proxy-for-uri u (normalize-proxy (proxy-config-proxy config))))))
  (:method ((proxy string) uri)
    (resolve-proxy (coerce-proxy-config proxy) uri))
  (:method ((proxy list) uri)
    (resolve-proxy (coerce-proxy-config proxy) uri))
  (:method ((proxy null) uri)
    (declare (ignore uri))
    nil))

(defun effective-proxy-config (request client)
  "Request :PROXY overrides client; else *DEFAULT-PROXY-CONFIG*."
  (coerce-proxy-config
   (or (and request (http-request-proxy request))
       (and client (http-client-proxy client))
       *default-proxy-config*)))

(defun parse-proxy-uri (proxy-url)
  "Return (values scheme host port user password) for a proxy URL string."
  (let* ((u (quri:uri proxy-url))
         (scheme (or (quri:uri-scheme u) "http"))
         (host (strip-ipv6-brackets (quri:uri-host u)))
         (port (or (quri:uri-port u)
                   (cond ((string-equal scheme "https") 443)
                         ((string-equal scheme "socks5") 1080)
                         (t 80))))
         (user (quri:uri-userinfo u))
         (pass nil))
    (when user
      (let ((colon (position #\: user)))
        (if colon
            (setf pass (subseq user (1+ colon))
                  user (subseq user 0 colon))
            (setf pass nil))))
    (values scheme host port user pass)))
