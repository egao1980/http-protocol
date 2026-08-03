(in-package #:http-protocol)

;;; Structured timeouts (urllib3 Timeout shape). Part of the client protocol;
;;; backends interpret CONNECT/READ/TOTAL when performing I/O.

(defclass http-timeout ()
  ((connect :initarg :connect :accessor timeout-connect :initform nil
            :documentation "Seconds to establish TCP/TLS (NIL = unset).")
   (read :initarg :read :accessor timeout-read :initform nil
         :documentation "Seconds waiting for socket data between reads.")
   (total :initarg :total :accessor timeout-total :initform nil
          :documentation "Wall-clock budget for the whole attempt (NIL = unset)."))
  (:documentation "Per-phase request timeouts. Numbers coerce via COERCE-TIMEOUT."))

(defun http-timeout-p (x) (typep x 'http-timeout))

(defun make-http-timeout (&key connect read total)
  (make-instance 'http-timeout :connect connect :read read :total total))

(defun coerce-timeout (x &key (default-total 30.0))
  "Normalize X → HTTP-TIMEOUT.
   NIL → DEFAULT-TOTAL as :TOTAL; number → :TOTAL; plist (:CONNECT/:READ/:TOTAL);
   HTTP-TIMEOUT → itself."
  (cond
    ((http-timeout-p x) x)
    ((null x) (make-http-timeout :total default-total))
    ((numberp x) (make-http-timeout :total (float x 1.0d0)))
    ((and (listp x) (evenp (length x)))
     (make-http-timeout :connect (getf x :connect)
                        :read (getf x :read)
                        :total (getf x :total)))
    (t (error 'http-protocol-error
              :message (format nil "cannot coerce timeout: ~S" x)))))

(defun effective-timeout (request client &key (default-total 30.0))
  "Request timeout overrides client; both may be NIL/number/plist/HTTP-TIMEOUT."
  (coerce-timeout (or (and request (http-request-timeout request))
                      (and client (http-client-timeout client)))
                  :default-total default-total))

(defun timeout-connect-seconds (timeout &optional (fallback 10.0))
  (or (timeout-connect timeout)
      (timeout-total timeout)
      fallback))

(defun timeout-read-seconds (timeout &optional (fallback 10.0))
  (or (timeout-read timeout)
      (timeout-total timeout)
      fallback))

(defun timeout-total-seconds (timeout &optional (fallback 30.0))
  (or (timeout-total timeout)
      (timeout-read timeout)
      (timeout-connect timeout)
      fallback))
