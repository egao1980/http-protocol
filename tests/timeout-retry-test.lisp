(in-package #:http-protocol/tests)

(deftest coerce-timeout-shapes
  (let ((t1 (coerce-timeout 5)))
    (ok (http-timeout-p t1))
    (ok (= 5.0d0 (timeout-total t1))))
  (let ((t2 (coerce-timeout '(:connect 1 :read 2 :total 3))))
    (ok (= 1 (timeout-connect t2)))
    (ok (= 2 (timeout-read t2)))
    (ok (= 3 (timeout-total t2))))
  (ok (= 30.0d0 (timeout-total-seconds (coerce-timeout nil)))))

(deftest effective-timeout-request-wins
  (let* ((client (make-instance 'http-client
                                :backend (make-instance 'http-backend)
                                :timeout 9))
         (req (make-http-request :url "http://x" :timeout '(:total 2 :connect 1))))
    (let ((t* (effective-timeout req client)))
      (ok (= 2 (timeout-total t*)))
      (ok (= 1 (timeout-connect-seconds t*))))))

(deftest coerce-retry-and-policy
  (ok (zerop (retry-total (coerce-retry nil))))
  (ok (= 3 (retry-total (coerce-retry 3))))
  (let ((r (make-http-retry :total 2 :backoff-factor 1 :backoff-max 10)))
    (ok (retry-should-retry-p r 1 :get :condition
                              (make-condition 'http-connection-error :message "x")))
    (ok (not (retry-should-retry-p r 3 :get :condition
                                   (make-condition 'http-connection-error :message "x"))))
    (ok (not (retry-should-retry-p r 1 :post :condition
                                   (make-condition 'http-connection-error :message "x"))))
    (ok (retry-should-retry-p r 1 :get :status 503))
    (ok (= 1.0d0 (retry-delay-seconds r 1)))
    (ok (= 2.0d0 (retry-delay-seconds r 2)))))

(deftest retry-after-header
  (let* ((r (make-http-retry :total 2 :backoff-factor 0.01))
         (ht (make-hash-table :test #'equal))
         (res (make-instance 'http-response :status 503 :headers ht :body #())))
    (setf (gethash "retry-after" ht) "7")
    (ok (= 7.0d0 (retry-delay-seconds r 1 :response res)))))
