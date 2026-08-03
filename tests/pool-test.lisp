(in-package #:http-protocol/tests)

(defclass fake-conn ()
  ((alive :initarg :alive :accessor fake-conn-alive :initform t)
   (closed :initform nil :accessor fake-conn-closed)))

(defmethod connection-alive-p ((c fake-conn))
  (fake-conn-alive c))

(defmethod pool-discard ((pool lru-connection-pool) (c fake-conn))
  (setf (fake-conn-closed c) t))

(deftest lru-pool-acquire-release
  (let* ((pool (make-lru-connection-pool :max-size 2))
         (k (pool-key "http" "example.com" 80))
         (a (make-instance 'fake-conn))
         (b (make-instance 'fake-conn))
         (c (make-instance 'fake-conn))
         (evicted nil))
    (pool-release pool k a :on-evict (lambda (x) (setf evicted x)))
    (ok (eq a (pool-acquire pool k)))
    (ok (null (pool-acquire pool k)))
    (pool-release pool k a :on-evict (lambda (x) (setf evicted x)))
    (pool-release pool k b)
    (pool-release pool k c)
    ;; max-size 2 → evict LRU (a); its on-evict closes it
    (ok (eq a evicted))
    (ok (fake-conn-closed evicted))
    (pool-clear pool)))

(deftest pooled-body-stream-releases
  (let* ((pool (make-lru-connection-pool :max-size 4))
         (k (pool-key "https" "example.com" 443))
         (conn (make-instance 'fake-conn))
         (src (make-octet-input-stream #(1 2 3)))
         (stream (make-pooled-body-stream src :pool pool :pool-key k
                                          :connection conn)))
    (ok (equalp #(1 2 3) (slurp-octets stream)))
    (ok (eq conn (pool-acquire pool k)))
    (ok (not (fake-conn-closed conn)))))

(deftest pooled-body-stream-discard-on-abort
  (let* ((pool (make-lru-connection-pool :max-size 4))
         (k (pool-key "http" "x" 80))
         (conn (make-instance 'fake-conn))
         (src (make-octet-input-stream #(9)))
         (stream (make-pooled-body-stream src :pool pool :pool-key k
                                          :connection conn)))
    (close stream :abort t)
    (ok (null (pool-acquire pool k)))
    (ok (fake-conn-closed conn))))

(deftest pool-key-with-proxy
  (ok (string= "http://proxy:8080|https://[::1]:443"
               (pool-key "https" "::1" 443 :proxy "http://proxy:8080"))))
