(in-package #:http-protocol)

;;; Duplex request-body pipe: the application writes; the backend reads.
;;; Close the write side (CLOSE-BODY-PIPE / CLOSE) to send END_STREAM.
;;; ON-DATA wakes the event loop (other thread → loop thread).

(defclass http-body-pipe
    (trivial-gray-streams:fundamental-binary-input-stream
     trivial-gray-streams:fundamental-binary-output-stream)
  ((lock :initform (bt:make-lock "http-body-pipe") :reader body-pipe-lock)
   (cv :initform (bt:make-condition-variable :name "http-body-pipe")
       :reader body-pipe-cv)
   (chunks :initform (list) :accessor body-pipe-chunks)
   (chunk-pos :initform 0 :accessor body-pipe-chunk-pos)
   (buffered :initform 0 :accessor body-pipe-buffered)
   (write-open-p :initform t :accessor body-pipe-write-open-p)
   (read-open-p :initform t :accessor body-pipe-read-open-p)
   (on-data :initarg :on-data :initform nil :accessor body-pipe-on-data)))

(defun make-http-body-pipe (&key on-data)
  "Octet pipe for a streaming / duplex request body."
  (make-instance 'http-body-pipe :on-data on-data))

(defun http-body-pipe-p (x)
  (typep x 'http-body-pipe))

(defun http-body-pipe-eof-p (s)
  (bt:with-lock-held ((body-pipe-lock s))
    (and (not (body-pipe-write-open-p s))
         (null (body-pipe-chunks s)))))

(defun http-body-pipe-listen (s)
  "T when a non-blocking read would return data or EOF."
  (bt:with-lock-held ((body-pipe-lock s))
    (or (body-pipe-chunks s)
        (not (body-pipe-write-open-p s)))))

(defun %body-pipe-signal (s)
  (let ((fn (body-pipe-on-data s)))
    (when fn
      (ignore-errors (funcall fn)))))

(defun write-body-pipe (s octets &key (start 0) (end (length octets)))
  "Append OCTETS[START:END]. Signals if the write side is closed."
  (when (>= start end)
    (return-from write-body-pipe s))
  (let ((piece (subseq octets start end)))
    (bt:with-lock-held ((body-pipe-lock s))
      (unless (body-pipe-write-open-p s)
        (error 'http-protocol-error :message "write to closed http-body-pipe"))
      (unless (body-pipe-read-open-p s)
        (error 'http-protocol-error :message "http-body-pipe reader closed"))
      (setf (body-pipe-chunks s) (nconc (body-pipe-chunks s) (list piece)))
      (incf (body-pipe-buffered s) (length piece))
      (bt:condition-notify (body-pipe-cv s)))
    (%body-pipe-signal s)
    s))

(defun close-body-pipe (s)
  "Half-close the write side (H2 END_STREAM). Further writes error."
  (bt:with-lock-held ((body-pipe-lock s))
    (setf (body-pipe-write-open-p s) nil)
    (bt:condition-notify (body-pipe-cv s)))
  (%body-pipe-signal s)
  s)

(defun http-body-pipe-read-available (s seq start end)
  "Non-blocking read into SEQ. → octets copied (0 if none yet)."
  (let ((pos start))
    (bt:with-lock-held ((body-pipe-lock s))
      (unless (body-pipe-read-open-p s)
        (return-from http-body-pipe-read-available start))
      (loop while (< pos end)
            for chunks = (body-pipe-chunks s)
            do (unless chunks (return))
               (let* ((chunk (car chunks))
                      (cpos (body-pipe-chunk-pos s))
                      (avail (- (length chunk) cpos))
                      (n (min avail (- end pos))))
                 (replace seq chunk :start1 pos :end1 (+ pos n)
                          :start2 cpos :end2 (+ cpos n))
                 (incf pos n)
                 (decf (body-pipe-buffered s) n)
                 (let ((cpos* (+ cpos n)))
                   (if (>= cpos* (length chunk))
                       (setf (body-pipe-chunks s) (cdr chunks)
                             (body-pipe-chunk-pos s) 0)
                       (setf (body-pipe-chunk-pos s) cpos*))))))
    pos))

(defmethod stream-element-type ((s http-body-pipe))
  '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-listen ((s http-body-pipe))
  (http-body-pipe-listen s))

(defmethod trivial-gray-streams:stream-write-byte ((s http-body-pipe) byte)
  (write-body-pipe s (make-array 1 :element-type '(unsigned-byte 8)
                                 :initial-element byte))
  byte)

(defmethod trivial-gray-streams:stream-write-sequence
    ((s http-body-pipe) seq start end &key)
  (write-body-pipe s seq :start start :end end)
  seq)

(defmethod trivial-gray-streams:stream-finish-output ((s http-body-pipe))
  nil)

(defmethod trivial-gray-streams:stream-force-output ((s http-body-pipe))
  nil)

(defmethod trivial-gray-streams:stream-read-byte ((s http-body-pipe))
  (bt:with-lock-held ((body-pipe-lock s))
    (loop until (or (body-pipe-chunks s)
                    (not (body-pipe-write-open-p s))
                    (not (body-pipe-read-open-p s)))
          do (bt:condition-wait (body-pipe-cv s) (body-pipe-lock s)))
    (unless (body-pipe-read-open-p s)
      (return-from trivial-gray-streams:stream-read-byte :eof))
    (let ((chunks (body-pipe-chunks s)))
      (unless chunks
        (return-from trivial-gray-streams:stream-read-byte :eof))
      (let* ((chunk (car chunks))
             (pos (body-pipe-chunk-pos s))
             (b (aref chunk pos)))
        (incf pos)
        (decf (body-pipe-buffered s))
        (if (>= pos (length chunk))
            (setf (body-pipe-chunks s) (cdr chunks)
                  (body-pipe-chunk-pos s) 0)
            (setf (body-pipe-chunk-pos s) pos))
        b))))

(defmethod trivial-gray-streams:stream-read-sequence
    ((s http-body-pipe) seq start end &key)
  (let ((pos start))
    (loop while (< pos end)
          do (bt:with-lock-held ((body-pipe-lock s))
               (loop until (or (body-pipe-chunks s)
                               (not (body-pipe-write-open-p s))
                               (not (body-pipe-read-open-p s)))
                     do (bt:condition-wait (body-pipe-cv s) (body-pipe-lock s)))
               (unless (body-pipe-read-open-p s)
                 (return))
               (let ((chunks (body-pipe-chunks s)))
                 (unless chunks
                   (return))
                 (let* ((chunk (car chunks))
                        (cpos (body-pipe-chunk-pos s))
                        (n (min (- (length chunk) cpos) (- end pos))))
                   (replace seq chunk :start1 pos :end1 (+ pos n)
                            :start2 cpos :end2 (+ cpos n))
                   (incf pos n)
                   (decf (body-pipe-buffered s) n)
                   (let ((cpos* (+ cpos n)))
                     (if (>= cpos* (length chunk))
                         (setf (body-pipe-chunks s) (cdr chunks)
                               (body-pipe-chunk-pos s) 0)
                         (setf (body-pipe-chunk-pos s) cpos*)))))))
    pos))

(defmethod close ((s http-body-pipe) &key abort)
  (declare (ignore abort))
  (bt:with-lock-held ((body-pipe-lock s))
    (setf (body-pipe-write-open-p s) nil
          (body-pipe-read-open-p s) nil
          (body-pipe-chunks s) nil
          (body-pipe-buffered s) 0)
    (bt:condition-notify (body-pipe-cv s)))
  (%body-pipe-signal s)
  (call-next-method))
