(in-package #:http-protocol)

;;; :auth / :range → wire headers (brief § Facade). Digest = P2.

(defun effective-auth (client request)
  "Per-request :auth wins over client default."
  (or (http-request-auth request)
      (and client (http-client-auth client))))

(defun authorization-header-value (auth)
  "Return Authorization header value, or NIL.
   AUTH: NIL | string | (:basic user password) | (:bearer token).
   (:digest …) signals UNSUPPORTED-OPERATION (P2)."
  (cond
    ((null auth) nil)
    ((stringp auth) auth)
    ((not (consp auth))
     (error 'http-protocol-error
            :message (format nil "Invalid :auth ~S" auth)))
    (t
     (ecase (first auth)
       (:basic
        (let ((user (second auth))
              (password (third auth)))
          (unless (and user password)
            (error 'http-protocol-error
                   :message ":auth (:basic user password) needs two args"))
          (format nil "Basic ~A"
                  (cl-base64:string-to-base64-string
                   (format nil "~A:~A" user password)))))
       (:bearer
        (let ((token (second auth)))
          (unless token
            (error 'http-protocol-error
                   :message ":auth (:bearer token) needs a token"))
          (format nil "Bearer ~A" token)))
       (:digest
        (error 'unsupported-operation
               :operation :auth-digest
               :message "Digest auth is P2"))))))

(defun range-header-value (range)
  "RANGE → Range header value. Accepts string, (start end), or (start)."
  (cond
    ((null range) nil)
    ((stringp range) range)
    ((and (consp range) (integerp (first range)))
     (let ((start (first range))
           (end (second range)))
       (if end
           (format nil "bytes=~D-~D" start end)
           (format nil "bytes=~D-" start))))
    (t
     (error 'http-protocol-error
            :message (format nil "Invalid :range ~S" range)))))

(defun inject-auth-range-headers (headers &key auth range)
  "Alist HEADERS with Authorization / Range applied. Existing keys win."
  (let ((out headers))
    (unless (assoc "authorization" out :test #'string-equal)
      (let ((v (authorization-header-value auth)))
        (when v
          (setf out (acons "authorization" v out)))))
    (unless (assoc "range" out :test #'string-equal)
      (let ((v (range-header-value range)))
        (when v
          (setf out (acons "range" v out)))))
    out))
