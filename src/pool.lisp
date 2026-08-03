(in-package #:http-protocol)

;;; Connection pool protocol (urllib3 PoolManager / dexador connection-cache shape).
;;; Opaque CONNECTION objects are backend-owned (socket, TLS session, …).
;;; Stream responses must RELEASE via POOL-RELEASE (see pooled-stream.lisp).

(defclass http-connection-pool ()
  ()
  (:documentation "Protocol class for keep-alive connection reuse."))

(defun http-connection-pool-p (x) (typep x 'http-connection-pool))

(defgeneric pool-acquire (pool key)
  (:documentation "Remove and return a pooled CONNECTION for KEY, or NIL.")
  (:method ((pool null) key)
    (declare (ignore key))
    nil))

(defgeneric pool-release (pool key connection &key on-evict)
  (:documentation
   "Return CONNECTION to POOL under KEY.
    ON-EVICT is (lambda (conn)) called if this or another entry is evicted.")
  (:method ((pool null) key connection &key on-evict)
    (declare (ignore key))
    (when on-evict (funcall on-evict connection))
    nil))

(defgeneric pool-discard (pool connection)
  (:documentation "Drop CONNECTION without pooling (caller should close it).")
  (:method ((pool t) connection)
    (declare (ignore pool connection))
    nil))

(defgeneric pool-clear (pool)
  (:documentation "Evict every entry, invoking eviction callbacks.")
  (:method ((pool null)) nil))

(defgeneric connection-alive-p (connection)
  (:documentation "Backend: can CONNECTION be reused? Default T.")
  (:method (connection)
    (declare (ignore connection))
    t))

(defun pool-key (scheme host port &key proxy)
  "Canonical pool key. PROXY when non-NIL scopes connections through that proxy."
  (let* ((host (strip-ipv6-brackets host))
         (origin (format-host-port host port))
         (scheme (string-downcase (string scheme))))
    (if proxy
        (format nil "~A|~A://~A" proxy scheme origin)
        (format nil "~A://~A" scheme origin))))

;;; --- Default LRU pool (thread-safe) ---

(defclass lru-pool-entry ()
  ((prev :initform nil :accessor lru-entry-prev)
   (next :initform nil :accessor lru-entry-next)
   (key :initarg :key :accessor lru-entry-key)
   (connection :initarg :connection :accessor lru-entry-connection)
   (on-evict :initarg :on-evict :accessor lru-entry-on-evict :initform nil)))

(defclass lru-connection-pool (http-connection-pool)
  ((lock :initform (bt:make-lock "http-connection-pool") :reader lru-pool-lock)
   (table :initform (make-hash-table :test #'equal) :reader lru-pool-table)
   (head :initform nil :accessor lru-pool-head)
   (tail :initform nil :accessor lru-pool-tail)
   (count :initform 0 :accessor lru-pool-count)
   (max-size :initarg :max-size :accessor lru-pool-max-size :initform 8))
  (:documentation "LRU multi-map: same KEY may hold several connections."))

(defun make-lru-connection-pool (&key (max-size 8))
  (make-instance 'lru-connection-pool :max-size max-size))

(defvar *default-connection-pool* nil
  "Optional process-wide pool. Clients may share or own a private pool.")

(defun ensure-default-connection-pool (&key (max-size 8))
  (or *default-connection-pool*
      (setf *default-connection-pool* (make-lru-connection-pool :max-size max-size))))

(defun %lru-unlink (pool entry)
  (let ((prev (lru-entry-prev entry))
        (next (lru-entry-next entry)))
    (if prev
        (setf (lru-entry-next prev) next)
        (setf (lru-pool-head pool) next))
    (if next
        (setf (lru-entry-prev next) prev)
        (setf (lru-pool-tail pool) prev)))
  (let* ((key (lru-entry-key entry))
         (table (lru-pool-table pool))
         (rest (delete entry (gethash key table) :count 1)))
    (if rest
        (setf (gethash key table) rest)
        (remhash key table)))
  (decf (lru-pool-count pool))
  entry)

(defun %lru-evict-tail (pool)
  (let ((tail (lru-pool-tail pool)))
    (when tail
      (%lru-unlink pool tail)
      (values (lru-entry-connection tail) (lru-entry-on-evict tail)))))

(defmethod pool-acquire ((pool lru-connection-pool) key)
  (bt:with-lock-held ((lru-pool-lock pool))
    (loop
      (let ((entries (gethash key (lru-pool-table pool))))
        (unless entries
          (return-from pool-acquire nil))
        (let* ((entry (car entries))
               (conn (lru-entry-connection entry)))
          (%lru-unlink pool entry)
          (if (connection-alive-p conn)
              (return-from pool-acquire conn)
              (ignore-errors (pool-discard pool conn))))))))

(defmethod pool-release ((pool lru-connection-pool) key connection &key on-evict)
  (unless (connection-alive-p connection)
    (pool-discard pool connection)
    (return-from pool-release nil))
  (let (evicted-conn evicted-cb)
    (bt:with-lock-held ((lru-pool-lock pool))
      (let* ((entry (make-instance 'lru-pool-entry
                                   :key key
                                   :connection connection
                                   :on-evict on-evict))
             (old-head (lru-pool-head pool))
             (table (lru-pool-table pool)))
        (setf (lru-entry-next entry) old-head
              (lru-pool-head pool) entry)
        (when old-head
          (setf (lru-entry-prev old-head) entry))
        (unless (lru-pool-tail pool)
          (setf (lru-pool-tail pool) entry))
        (push entry (gethash key table))
        (incf (lru-pool-count pool))
        (when (> (lru-pool-count pool) (lru-pool-max-size pool))
          (setf (values evicted-conn evicted-cb) (%lru-evict-tail pool)))))
    (when evicted-conn
      (when evicted-cb
        (ignore-errors (funcall evicted-cb evicted-conn)))
      (ignore-errors (pool-discard pool evicted-conn)))
    t))

(defmethod pool-clear ((pool lru-connection-pool))
  (loop
    (multiple-value-bind (conn cb)
        (bt:with-lock-held ((lru-pool-lock pool))
          (%lru-evict-tail pool))
      (unless conn (return))
      (when cb (ignore-errors (funcall cb conn)))
      (ignore-errors (pool-discard pool conn)))))

(defun coerce-connection-pool (x &key (max-size 8))
  "T → default shared pool; HTTP-CONNECTION-POOL → itself; NIL → no pooling."
  (cond
    ((http-connection-pool-p x) x)
    ((eq x t) (ensure-default-connection-pool :max-size max-size))
    ((null x) nil)
    (t (error 'http-protocol-error
              :message (format nil "cannot coerce connection pool: ~S" x)))))

(defun effective-connection-pool (client)
  (coerce-connection-pool
   (if client
       (http-client-pool client)
       t)))
