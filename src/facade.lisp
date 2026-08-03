(in-package #:http)

;;; Thin httpx-shaped helpers (API.md facade layer).
;;; Bodies are streams / octets / strings — no filesystem.

(defun request (method url &rest keys
                &key (backend http-protocol:*http-backend*)
                  (client nil clientp)
                  headers content data files params timeout max-redirects cookies
                  auth range
                  accept-encoding content-encoding
                  (decompress t) (force-binary t) want-stream
                  raise-for-status
                &allow-other-keys)
  "Sync HTTP request. Uses *HTTP-BACKEND* / *HTTP-CLIENT* when not supplied.
   :CONTENT stream|octets|string; :DATA/:FILES → multipart (stream-backed parts)."
  (declare (ignore headers content data files params timeout max-redirects cookies
                   auth range
                   accept-encoding content-encoding decompress force-binary
                   want-stream raise-for-status))
  (let* ((backend (or backend
                      (or http-protocol:*http-backend*
                          (error 'http-error :message "*http-backend* is not bound"))))
         (client (if clientp
                     client
                     (or http-protocol:*http-client*
                         (make-http-client backend))))
         (req (apply #'make-http-request :method method :url url
                     (loop for (k v) on keys by #'cddr
                           unless (member k '(:backend :client))
                             collect k and collect v)))
         (res (send backend client req)))
    (when (http-request-raise-for-status req)
      (raise-for-status res))
    res))

(defun get (url &rest keys &key &allow-other-keys)
  (apply #'request :get url keys))

(defun head (url &rest keys &key &allow-other-keys)
  (apply #'request :head url keys))

(defun options (url &rest keys &key &allow-other-keys)
  (apply #'request :options url keys))

(defun post (url &rest keys &key &allow-other-keys)
  (apply #'request :post url keys))

(defun put (url &rest keys &key &allow-other-keys)
  (apply #'request :put url keys))

(defun patch (url &rest keys &key &allow-other-keys)
  (apply #'request :patch url keys))

(defun delete (url &rest keys &key &allow-other-keys)
  (apply #'request :delete url keys))

(defun request-async (method url &rest keys
                      &key (backend http-protocol:*http-backend*)
                        (client nil clientp)
                        headers content data files params timeout max-redirects cookies
                        auth range
                        accept-encoding content-encoding
                        (decompress t) (force-binary t) want-stream
                        raise-for-status
                      &allow-other-keys)
  "Async HTTP request → Blackbird promise of HTTP-RESPONSE."
  (declare (ignore headers content data files params timeout max-redirects cookies
                   auth range
                   accept-encoding content-encoding decompress force-binary
                   want-stream raise-for-status))
  (let* ((backend (or backend
                      (or http-protocol:*http-backend*
                          (error 'http-error :message "*http-backend* is not bound"))))
         (client (if clientp
                     client
                     (or http-protocol:*http-client*
                         (make-http-client backend))))
         (req (apply #'make-http-request :method method :url url
                     (loop for (k v) on keys by #'cddr
                           unless (member k '(:backend :client))
                             collect k and collect v))))
    (blackbird:with-promise (resolve reject)
      (send-async
       backend client req
       :callback
       (lambda (res)
         (if (http-request-raise-for-status req)
             (handler-case
                 (progn
                   (raise-for-status res)
                   (resolve res))
               (error (e) (reject e)))
             (resolve res)))
       :error-callback (lambda (e) (reject e))))))

(defun get-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :get url keys))

(defun head-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :head url keys))

(defun options-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :options url keys))

(defun post-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :post url keys))

(defun put-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :put url keys))

(defun patch-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :patch url keys))

(defun delete-async (url &rest keys &key &allow-other-keys)
  (apply #'request-async :delete url keys))

(defun trace (url &rest keys &key &allow-other-keys)
  "TRACE request. Servers often disable TRACE; use only when allowed."
  (apply #'request :trace url keys))

(defun connect (url &rest keys &key &allow-other-keys)
  "CONNECT tunnel request (expert / proxy). Backend may signal unsupported-operation."
  (apply #'request :connect url keys))

(defun stream (method url &rest keys &key &allow-other-keys)
  "Sync request with :WANT-STREAM T. Body is a buffered binary input stream."
  (apply #'request method url :want-stream t keys))

(defun stream-async (method url &rest keys &key &allow-other-keys)
  "Async request with :WANT-STREAM T → promise of HTTP-RESPONSE.
   RESPONSE-BODY is a binary input stream; keep the event loop running while reading
   (do not block the loop thread on stream reads — copy on a worker if needed)."
  (apply #'request-async method url :want-stream t keys))

(defmacro with-client ((var &rest client-keys) &body body)
  "Bind VAR (and *HTTP-CLIENT*) to a fresh client for *HTTP-BACKEND*."
  `(let* ((,var (make-http-client
                 (or http-protocol:*http-backend*
                     (error 'http-error :message "*http-backend* is not bound"))
                 ,@client-keys))
          (http-protocol:*http-client* ,var))
     ,@body))
