(in-package #:http-protocol)

;;; Tiny binary vector → input stream (encode stream fallbacks, tests).

(defclass octet-input-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((data :initarg :data :reader octet-input-stream-data)
   (pos :initform 0 :accessor octet-input-stream-pos)))

(defun make-octet-input-stream (octets)
  "Binary input stream over OCTETS (copied to a simple-array)."
  (make-instance 'octet-input-stream
                 :data (coerce octets '(simple-array (unsigned-byte 8) (*)))))

(defmethod trivial-gray-streams:stream-read-byte ((s octet-input-stream))
  (let ((pos (octet-input-stream-pos s))
        (data (octet-input-stream-data s)))
    (if (>= pos (length data))
        :eof
        (prog1 (aref data pos)
          (incf (octet-input-stream-pos s))))))

(defmethod trivial-gray-streams:stream-read-sequence
    ((s octet-input-stream) seq start end &key)
  (let* ((data (octet-input-stream-data s))
         (pos (octet-input-stream-pos s))
         (n (min (- end start) (- (length data) pos))))
    (replace seq data :start1 start :end1 (+ start n) :start2 pos)
    (incf (octet-input-stream-pos s) n)
    (+ start n)))

(defun slurp-octets (stream)
  "Read STREAM to EOF into a simple octet vector."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0))
        (buf (make-array 4096 :element-type '(unsigned-byte 8))))
    (loop for n = (read-sequence buf stream)
          while (plusp n)
          do (loop for i below n do (vector-push-extend (aref buf i) out)))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))
