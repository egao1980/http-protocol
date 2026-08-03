(in-package #:http-protocol/tests)

(deftest buffered-stream-roundtrip
  (let* ((raw (make-array 200000 :element-type '(unsigned-byte 8)
                          :initial-element 7))
         (buf-size 4096))
    (with-open-stream (src (make-octet-input-stream raw))
      (with-open-stream (in (make-buffered-binary-input-stream
                             src :buffer-size buf-size))
        (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0))
              (chunk (make-array buf-size :element-type '(unsigned-byte 8))))
          (loop for n = (read-sequence chunk in)
                while (plusp n)
                do (loop for i below n do (vector-push-extend (aref chunk i) out)))
          (ok (= (length raw) (length out)))
          (ok (equalp raw (coerce out '(simple-array (unsigned-byte 8) (*))))))))))

(deftest buffered-stream-read-byte
  (with-open-stream (src (make-octet-input-stream #(1 2 3)))
    (with-open-stream (in (make-buffered-binary-input-stream src :buffer-size 2))
      (ok (= 1 (read-byte in)))
      (ok (= 2 (read-byte in)))
      (ok (= 3 (read-byte in)))
      (ok (eq :eof (read-byte in nil :eof))))))

(deftest prepare-request-content-stream-no-coding
  (with-open-stream (src (make-octet-input-stream #(9 8 7)))
    (multiple-value-bind (wire ce)
        (prepare-request-content src)
      (ok (null ce))
      (ok (typep wire 'buffered-binary-input-stream))
      (ok (equalp #(9 8 7) (slurp-octets wire))))))

(deftest prepare-request-content-vector-stays-vector
  (let ((v (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))))
    (multiple-value-bind (wire ce)
        (prepare-request-content v)
      (ok (null ce))
      (ok (equalp v wire)))))

(deftest wrap-response-identity
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "content-type" ht) "application/octet-stream")
    (with-open-stream (src (make-octet-input-stream #(4 5 6)))
      (multiple-value-bind (app headers*)
          (wrap-response-body-stream src ht :decompress t :buffer-size 2)
        (ok (typep app 'buffered-binary-input-stream))
        (ok (string= "application/octet-stream"
                     (gethash "content-type" headers*)))
        (ok (equalp #(4 5 6) (slurp-octets app)))))))

(deftest body-stream-from-octets
  (let ((res (make-instance 'http-response
                            :status 200
                            :headers (make-hash-table :test #'equal)
                            :body #(10 11))))
    (with-open-stream (in (body-stream res))
      (ok (equalp #(10 11) (slurp-octets in))))))

(deftest copy-stream-counts
  (uiop:with-temporary-file (:stream out :pathname path
                             :element-type '(unsigned-byte 8)
                             :direction :io)
    (declare (ignore path))
    (with-open-stream (src (make-octet-input-stream #(1 2 3 4 5)))
      (ok (= 5 (copy-stream src out :buffer-size 2))))
    (file-position out 0)
    (let ((got (make-array 5 :element-type '(unsigned-byte 8))))
      (ok (= 5 (read-sequence got out)))
      (ok (equalp #(1 2 3 4 5) got)))))
