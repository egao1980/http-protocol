(in-package #:http-protocol/conformance)

(defun %bytes (s)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

(defun %read-all (stream &optional (chunk 4096))
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0))
        (buf (make-array chunk :element-type '(unsigned-byte 8))))
    (loop for n = (read-sequence buf stream)
          while (plusp n)
          do (loop for i below n do (vector-push-extend (aref buf i) out)))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(deftest buffer-round-trips
  (dolist (coding *test-codings*)
    (testing (format nil "buffer ~S" coding)
      (let* ((raw (%bytes (format nil "hello ~A content-encoding" coding)))
             (enc (encode-content-coding coding raw))
             (dec (decode-content-coding coding enc)))
        (ok (plusp (length enc)))
        (ok (not (equalp raw enc)))
        (ok (equalp raw dec))))))

(deftest buffer-empty
  (dolist (coding *test-codings*)
    (testing (format nil "empty ~S" coding)
      (let* ((raw (make-array 0 :element-type '(unsigned-byte 8)))
             (enc (encode-content-coding coding raw))
             (dec (decode-content-coding coding enc)))
        (ok (equalp raw dec))))))

(deftest stream-decode-round-trip
  (dolist (coding *test-codings*)
    (testing (format nil "stream-decode ~S" coding)
      (let* ((raw (%bytes (format nil "stream decode ~A" coding)))
             (enc (encode-content-coding coding raw)))
        (with-open-stream (src (make-octet-input-stream enc))
          (with-open-stream (in (make-decoding-stream coding src))
            (ok (streamp in))
            (ok (equalp raw (%read-all in)))))))))

(deftest stream-encode-then-buffer-decode
  (dolist (coding *test-codings*)
    (testing (format nil "stream-encode ~S" coding)
      (let ((raw (%bytes (format nil "stream encode ~A" coding))))
        (with-open-stream (src (make-octet-input-stream raw))
          (with-open-stream (cin (make-encoding-stream coding src))
            (let ((enc (%read-all cin)))
              (ok (plusp (length enc)))
              (ok (equalp raw (decode-content-coding coding enc))))))))))

(deftest stream-to-stream
  (dolist (coding *test-codings*)
    (testing (format nil "stream→stream ~S" coding)
      (let ((raw (%bytes (format nil "pull ~A round trip" coding))))
        (with-open-stream (plain (make-octet-input-stream raw))
          (with-open-stream (cin (make-encoding-stream coding plain))
            (with-open-stream (din (make-decoding-stream coding cin))
              (ok (equalp raw (%read-all din))))))))))
