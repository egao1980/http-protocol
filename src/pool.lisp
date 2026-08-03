(in-package #:http-protocol)

;;; Connection pool CLOS protocol (urllib3 PoolManager shape).
;;; Opaque CONNECTION objects are backend-owned (socket, TLS session, …).
;;; Concrete pools (LRU, thread-safe, …) live in backends — specialize the
;;; generics below. Stream responses RELEASE via POOLED-BODY-STREAM.

(defclass http-connection-pool ()
  ()
  (:documentation
   "Protocol class for keep-alive connection reuse.
    Backends subclass and implement POOL-ACQUIRE / POOL-RELEASE / …"))

(defun http-connection-pool-p (x) (typep x 'http-connection-pool))

(defgeneric pool-acquire (pool key)
  (:documentation "Remove and return a pooled CONNECTION for KEY, or NIL.")
  (:method ((pool null) key)
    (declare (ignore key))
    nil)
  (:method ((pool http-connection-pool) key)
    (declare (ignore key))
    (error 'unsupported-operation
           :operation 'pool-acquire
           :message "POOL-ACQUIRE not implemented for this pool class")))

(defgeneric pool-release (pool key connection &key on-evict)
  (:documentation
   "Return CONNECTION to POOL under KEY.
    ON-EVICT is (lambda (conn)) called if this or another entry is evicted.")
  (:method ((pool null) key connection &key on-evict)
    (declare (ignore key))
    (when on-evict (funcall on-evict connection))
    nil)
  (:method ((pool http-connection-pool) key connection &key on-evict)
    (declare (ignore key connection on-evict))
    (error 'unsupported-operation
           :operation 'pool-release
           :message "POOL-RELEASE not implemented for this pool class")))

(defgeneric pool-discard (pool connection)
  (:documentation "Drop CONNECTION without pooling (caller should close it).")
  (:method ((pool t) connection)
    (declare (ignore pool connection))
    nil))

(defgeneric pool-clear (pool)
  (:documentation "Evict every entry, invoking eviction callbacks.")
  (:method ((pool null)) nil)
  (:method ((pool http-connection-pool))
    (error 'unsupported-operation
           :operation 'pool-clear
           :message "POOL-CLEAR not implemented for this pool class")))

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

(defvar *default-connection-pool* nil
  "Process-wide pool instance, or NIL. Backends may set this on load.")

(defvar *connection-pool-constructor* nil
  "When non-NIL, (lambda (&key max-size)) → HTTP-CONNECTION-POOL.
   Set by backends that ship a concrete pool.")

(defun make-connection-pool (&key (max-size 8))
  "Construct a concrete pool via *CONNECTION-POOL-CONSTRUCTOR* (backend)."
  (unless *connection-pool-constructor*
    (error 'unsupported-operation
           :operation 'make-connection-pool
           :message
           "No pool implementation registered; load an HTTP backend or pass :POOL"))
  (funcall *connection-pool-constructor* :max-size max-size))

(defun ensure-default-connection-pool (&key (max-size 8))
  "Return *DEFAULT-CONNECTION-POOL*, creating via MAKE-CONNECTION-POOL if needed."
  (or *default-connection-pool*
      (when *connection-pool-constructor*
        (setf *default-connection-pool*
              (make-connection-pool :max-size max-size)))))

(defun coerce-connection-pool (x &key (max-size 8))
  "HTTP-CONNECTION-POOL → itself; T → ENSURE-DEFAULT-CONNECTION-POOL (may be NIL);
   NIL → no pooling."
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
