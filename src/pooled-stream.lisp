(in-package #:http-protocol)

;;; Response body stream that returns the underlying CONNECTION to a pool
;;; when the body is fully consumed or the stream is closed (urllib3
;;; preload_content=False / release_conn semantics).

(defclass pooled-body-stream
    (trivial-gray-streams:fundamental-binary-input-stream)
  ((source :initarg :source :accessor pooled-stream-source
           :documentation "Underlying binary input stream (wire or CE wrap).")
   (pool :initarg :pool :accessor pooled-stream-pool :initform nil)
   (pool-key :initarg :pool-key :accessor pooled-stream-pool-key :initform nil)
   (connection :initarg :connection :accessor pooled-stream-connection :initform nil)
   (on-evict :initarg :on-evict :accessor pooled-stream-on-evict :initform nil
             :documentation "Eviction callback passed to POOL-RELEASE.")
   (released-p :initform nil :accessor pooled-stream-released-p)
   (discard-on-close :initarg :discard-on-close :accessor pooled-stream-discard-p
                     :initform nil
                     :documentation "When T, never return to pool (Connection: close).")))

(defun make-pooled-body-stream (source &key pool pool-key connection on-evict
                                         discard-on-close)
  (make-instance 'pooled-body-stream
                 :source source
                 :pool pool
                 :pool-key pool-key
                 :connection connection
                 :on-evict on-evict
                 :discard-on-close discard-on-close))

(defun %pooled-release (stream &key abort)
  (when (pooled-stream-released-p stream)
    (return-from %pooled-release nil))
  (setf (pooled-stream-released-p stream) t)
  (let ((pool (pooled-stream-pool stream))
        (key (pooled-stream-pool-key stream))
        (conn (pooled-stream-connection stream))
        (src (pooled-stream-source stream)))
    (ignore-errors (close src :abort abort))
    (cond
      ((or abort (null pool) (null conn) (pooled-stream-discard-p stream))
       (ignore-errors (pool-discard pool conn))
       (when (pooled-stream-on-evict stream)
         (ignore-errors (funcall (pooled-stream-on-evict stream) conn))))
      (t
       (pool-release pool key conn :on-evict (pooled-stream-on-evict stream))))
    (setf (pooled-stream-connection stream) nil
          (pooled-stream-source stream) nil)
    t))

(defun release-response-connection (response &key abort)
  "If RESPONSE-BODY is a POOLED-BODY-STREAM, release/discard its connection."
  (let ((body (and response (response-body response))))
    (when (typep body 'pooled-body-stream)
      (%pooled-release body :abort abort))))

(defmethod trivial-gray-streams:stream-read-byte ((s pooled-body-stream))
  (let ((src (pooled-stream-source s)))
    (unless src
      (return-from trivial-gray-streams:stream-read-byte :eof))
    (let ((b (read-byte src nil :eof)))
      (when (eq b :eof)
        (%pooled-release s :abort nil))
      b)))

(defmethod trivial-gray-streams:stream-read-sequence
    ((s pooled-body-stream) seq start end &key)
  (let ((src (pooled-stream-source s)))
    (unless src
      (return-from trivial-gray-streams:stream-read-sequence start))
    (let ((n (read-sequence seq src :start start :end end)))
      (when (< n end)
        (%pooled-release s :abort nil))
      n)))

(defmethod close ((s pooled-body-stream) &key abort)
  (%pooled-release s :abort abort)
  (call-next-method))
