(in-package #:http-protocol/tests)

;;; Protocol-level pool tests use a tiny mock — concrete LRU lives in backends.

(defclass mock-conn ()
  ((closed :initform nil :accessor mock-conn-closed)))

(defclass mock-pool (http-connection-pool)
  ((store :initform (make-hash-table :test #'equal) :reader mock-pool-store)))

(defmethod pool-acquire ((pool mock-pool) key)
  (let ((q (gethash key (mock-pool-store pool))))
    (when q
      (let ((c (car q)))
        (setf (gethash key (mock-pool-store pool)) (cdr q))
        c))))

(defmethod pool-release ((pool mock-pool) key connection &key on-evict)
  (declare (ignore on-evict))
  (push connection (gethash key (mock-pool-store pool)))
  t)

(defmethod pool-discard ((pool mock-pool) (c mock-conn))
  (setf (mock-conn-closed c) t))

(defmethod pool-clear ((pool mock-pool))
  (clrhash (mock-pool-store pool)))

(deftest mock-pool-acquire-release
  (let* ((pool (make-instance 'mock-pool))
         (k (pool-key "http" "example.com" 80))
         (a (make-instance 'mock-conn)))
    (ok (null (pool-acquire pool k)))
    (pool-release pool k a)
    (ok (eq a (pool-acquire pool k)))
    (ok (null (pool-acquire pool k)))))

(deftest pooled-body-stream-releases
  (let* ((pool (make-instance 'mock-pool))
         (k (pool-key "https" "example.com" 443))
         (conn (make-instance 'mock-conn))
         (src (make-octet-input-stream #(1 2 3)))
         (stream (make-pooled-body-stream src :pool pool :pool-key k
                                          :connection conn)))
    (ok (equalp #(1 2 3) (slurp-octets stream)))
    (ok (eq conn (pool-acquire pool k)))
    (ok (not (mock-conn-closed conn)))))

(deftest pooled-body-stream-discard-on-abort
  (let* ((pool (make-instance 'mock-pool))
         (k (pool-key "http" "x" 80))
         (conn (make-instance 'mock-conn))
         (src (make-octet-input-stream #(9)))
         (stream (make-pooled-body-stream src :pool pool :pool-key k
                                          :connection conn)))
    (close stream :abort t)
    (ok (null (pool-acquire pool k)))
    (ok (mock-conn-closed conn))))

(deftest pool-key-with-proxy
  (ok (string= "http://proxy:8080|https://[::1]:443"
               (pool-key "https" "::1" 443 :proxy "http://proxy:8080"))))

(deftest protocol-pool-unsupported-without-backend
  (ok (signals (pool-acquire (make-instance 'http-connection-pool) "k")
               'unsupported-operation))
  (let ((*connection-pool-constructor* nil)
        (*default-connection-pool* nil))
    (ok (signals (make-connection-pool) 'unsupported-operation))
    (ok (null (ensure-default-connection-pool)))))
