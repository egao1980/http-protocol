(in-package #:http-protocol)

;;; Fixed-size buffered binary input — memory-bounded, high throughput.
;;; Peak extra memory ≈ BUFFER-SIZE (not body size).

(defvar *http-stream-buffer-size* 65536
  "Default buffer size (octets) for HTTP body Gray streams.")

(defclass buffered-binary-input-stream
    (trivial-gray-streams:fundamental-binary-input-stream)
  ((source :initarg :source :reader buffered-stream-source
           :documentation "Underlying binary input stream.")
   (buffer :initarg :buffer :reader buffered-stream-buffer)
   (start :initform 0 :accessor buffered-stream-start)
   (end :initform 0 :accessor buffered-stream-end)
   (eof :initform nil :accessor buffered-stream-eof-p)
   (close-source-p :initarg :close-source-p :initform t
                   :reader buffered-stream-close-source-p)))

(defun make-buffered-binary-input-stream
    (source &key (buffer-size *http-stream-buffer-size*) (close-source-p t))
  "Wrap SOURCE in a binary input stream that refills a fixed BUFFER-SIZE buffer.
   SOURCE must be a binary input stream. Does not copy the full body into memory."
  (check-type source stream)
  (assert (plusp buffer-size) (buffer-size) "buffer-size must be positive")
  (make-instance 'buffered-binary-input-stream
                 :source source
                 :buffer (make-array buffer-size :element-type '(unsigned-byte 8))
                 :close-source-p close-source-p))

(defun %buffered-refill (s)
  (when (buffered-stream-eof-p s)
    (return-from %buffered-refill 0))
  (let* ((buf (buffered-stream-buffer s))
         (n (read-sequence buf (buffered-stream-source s))))
    (setf (buffered-stream-start s) 0
          (buffered-stream-end s) n)
    (when (zerop n)
      (setf (buffered-stream-eof-p s) t))
    n))

(defmethod trivial-gray-streams:stream-read-byte ((s buffered-binary-input-stream))
  (when (>= (buffered-stream-start s) (buffered-stream-end s))
    (when (zerop (%buffered-refill s))
      (return-from trivial-gray-streams:stream-read-byte :eof)))
  (let ((i (buffered-stream-start s)))
    (prog1 (aref (buffered-stream-buffer s) i)
      (setf (buffered-stream-start s) (1+ i)))))

(defmethod trivial-gray-streams:stream-read-sequence
    ((s buffered-binary-input-stream) seq start end &key)
  (let ((pos start))
    (loop while (< pos end)
          do (when (>= (buffered-stream-start s) (buffered-stream-end s))
               (when (zerop (%buffered-refill s))
                 (return)))
             (let* ((avail (- (buffered-stream-end s) (buffered-stream-start s)))
                    (need (- end pos))
                    (n (min avail need))
                    (src-start (buffered-stream-start s)))
               (replace seq (buffered-stream-buffer s)
                        :start1 pos :end1 (+ pos n)
                        :start2 src-start :end2 (+ src-start n))
               (incf pos n)
               (incf (buffered-stream-start s) n)))
    pos))

(defmethod close ((s buffered-binary-input-stream) &key abort)
  (declare (ignore abort))
  (when (open-stream-p s)
    (when (buffered-stream-close-source-p s)
      (ignore-errors (close (buffered-stream-source s))))
    (call-next-method)))

(defun copy-stream (input output &key (buffer-size *http-stream-buffer-size*))
  "Copy INPUT to OUTPUT using a fixed BUFFER-SIZE scratch vector. Returns octet count."
  (let ((buf (make-array buffer-size :element-type '(unsigned-byte 8)))
        (total 0))
    (loop for n = (read-sequence buf input)
          while (plusp n)
          do (write-sequence buf output :end n)
             (incf total n))
    total))
