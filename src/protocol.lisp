(in-package #:http-protocol)

;;; Client protocol generics. Backends specialize SEND / MAKE-HTTP-CLIENT.

(defgeneric make-http-client (backend &key base-url headers cookie-jar timeout
                                      max-redirects proxy verify
                                      &allow-other-keys)
  (:documentation "Create an HTTP-CLIENT for BACKEND (requests Session shape).
   COOKIE-JAR defaults to a fresh empty jar when omitted.")
  (:method ((backend http-backend) &rest keys
            &key (cookie-jar nil cookie-jar-p) &allow-other-keys)
    (let ((keys* (loop for (k v) on keys by #'cddr
                       unless (eq k :cookie-jar)
                         collect k and collect v)))
      (apply #'make-instance 'http-client
             :backend backend
             :cookie-jar (if cookie-jar-p
                             cookie-jar
                             (cl-cookie:make-cookie-jar))
             keys*))))

(defgeneric send (backend client request &key)
  (:documentation "Perform REQUEST on CLIENT via BACKEND. Blocking → HTTP-RESPONSE.")
  (:method ((backend http-backend) client request &key)
    (declare (ignore client request))
    (error 'unsupported-operation :operation 'send
           :message (format nil "backend ~A does not implement SEND" (backend-name backend)))))

(defgeneric send-async (backend client request &key callback error-callback)
  (:documentation
   "Start REQUEST asynchronously on CLIENT.

    Protocol primitive (callback + cancel token). CALLBACK is called with an
    HTTP-RESPONSE on success; ERROR-CALLBACK with a condition on failure.
    Returns an opaque handle for CANCEL-REQUEST.

    Facade layers wrap this as a Blackbird-shaped promise — backends must not
    hard-depend on Blackbird.")
  (:method ((backend http-backend) client request &key callback error-callback)
    (declare (ignore client request callback error-callback))
    (error 'unsupported-operation :operation 'send-async
           :message (format nil "backend ~A does not implement SEND-ASYNC" (backend-name backend)))))

(defgeneric cancel-request (backend handle)
  (:documentation "Cancel an in-flight SEND-ASYNC handle.")
  (:method ((backend http-backend) handle)
    (declare (ignore handle))
    (error 'unsupported-operation :operation 'cancel-request
           :message (format nil "backend ~A does not implement CANCEL-REQUEST"
                            (backend-name backend)))))

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
