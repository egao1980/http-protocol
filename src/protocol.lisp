(in-package #:http-protocol)

;;; Client protocol generics. Backends specialize SEND / MAKE-HTTP-CLIENT.

(defgeneric make-http-client (backend &key base-url headers timeout
                                      max-redirects proxy verify
                                      &allow-other-keys)
  (:documentation "Create an HTTP-CLIENT for BACKEND.")
  (:method ((backend http-backend) &rest keys &key &allow-other-keys)
    (apply #'make-instance 'http-client :backend backend keys)))

(defgeneric send (backend client request &key)
  (:documentation "Perform REQUEST on CLIENT via BACKEND. Blocking → HTTP-RESPONSE.")
  (:method ((backend http-backend) client request &key)
    (declare (ignore client request))
    (error 'unsupported-operation :operation 'send
           :message (format nil "backend ~A does not implement SEND" (backend-name backend)))))

(defgeneric send-async (backend client request &key)
  (:documentation "Async SEND → promise/handle. Wave-1 sync backends signal unsupported.")
  (:method ((backend http-backend) client request &key)
    (declare (ignore client request))
    (error 'unsupported-operation :operation 'send-async
           :message (format nil "backend ~A does not implement SEND-ASYNC" (backend-name backend)))))

(defun raise-for-status (response)
  "Signal http-client-error / http-server-error when status is 4xx/5xx."
  (check-type response http-response)
  (let ((status (response-status response)))
    (cond ((<= 400 status 499)
           (error 'http-client-error :status status :response response
                  :message (format nil "HTTP ~D" status)))
          ((<= 500 status 599)
           (error 'http-server-error :status status :response response
                  :message (format nil "HTTP ~D" status)))
          (t response))))
