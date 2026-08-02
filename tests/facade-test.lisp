(in-package #:http-protocol/tests)

(defclass mock-backend (http-backend)
  ()
  (:default-initargs :name "mock"))

(defmethod send ((backend mock-backend) client request &key)
  (declare (ignore client))
  (make-instance 'http-response
                 :status 200
                 :headers (let ((ht (make-hash-table :test #'equal)))
                            (setf (gethash "content-type" ht) "text/plain")
                            ht)
                 :body (coerce-to-octets "ok")
                 :url (http-request-url request)
                 :request request))

(deftest facade-get
  (let ((*http-backend* (make-instance 'mock-backend)))
    (let ((res (get "https://example.com/x")))
      (ok (= 200 (response-status res)))
      (ok (equalp (coerce-to-octets "ok") (response-body res)))
      (ok (string= "text/plain" (response-header res :content-type))))))

(deftest raise-for-status-opt-in
  (let ((res (make-instance 'http-response
                            :status 404
                            :headers (make-hash-table :test #'equal)
                            :body #())))
    (ok (signals (raise-for-status res) 'http-client-error))))

(defclass mock-async-backend (http-backend)
  ()
  (:default-initargs :name "mock-async"))

(defmethod send-async ((backend mock-async-backend) client request
                       &key callback error-callback)
  (declare (ignore client error-callback))
  (when callback
    (funcall callback
             (make-instance 'http-response
                            :status 200
                            :headers (make-hash-table :test #'equal)
                            :body (coerce-to-octets "async-ok")
                            :url (http-request-url request)
                            :request request)))
  :mock-handle)

(deftest facade-get-async
  (let ((*http-backend* (make-instance 'mock-async-backend))
        (seen nil))
    (blackbird:attach
     (get-async "https://example.com/a")
     (lambda (res) (setf seen res)))
    (ok (not (null seen)))
    (ok (= 200 (response-status seen)))
    (ok (equalp (coerce-to-octets "async-ok") (response-body seen)))))
