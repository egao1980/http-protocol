(in-package #:http-protocol)

;;; Secret header values. A SECRET wraps a credential (API key, bearer token,
;;; cookie) so that printing it -- in a backtrace frame, a condition report,
;;; DESCRIBE, a log line -- shows #<SECRET [redacted]> instead of the value.
;;; REVEAL unwraps it; backends call it only where the octets hit the wire.

(defclass secret ()
  ((value :initarg :value :reader %secret-value))
  (:documentation "Opaque wrapper for a credential. Prints redacted; REVEAL unwraps."))

(defmethod print-object ((s secret) stream)
  (print-unreadable-object (s stream :type t)
    (write-string "[redacted]" stream)))

(defmethod describe-object ((s secret) stream)
  (format stream "~&~S is a SECRET (value redacted; use REVEAL).~%" s))

(defun secret-p (x) (typep x 'secret))

(defun make-secret (value)
  "Wrap VALUE (a string) as a SECRET. A SECRET is returned unchanged; NIL stays NIL."
  (cond ((null value) nil)
        ((secret-p value) value)
        (t (check-type value string)
           (make-instance 'secret :value value))))

(defun reveal (x)
  "The plain value of X: the wrapped string for a SECRET, X itself otherwise."
  (if (secret-p x) (%secret-value x) x))

(defparameter *secret-header-names*
  '("authorization" "proxy-authorization" "cookie" "set-cookie"
    "x-api-key" "api-key" "x-auth-token" "x-goog-api-key")
  "Lowercase header names whose values are credentials.
   PROTECT-HEADERS wraps them; REDACT-HEADERS hides them.")

(defun secret-header-name-p (name)
  "T when header NAME (string or symbol) carries a credential."
  (and name
       (member (string name) *secret-header-names* :test #'string-equal)
       t))

(defun %map-headers (fn headers)
  "Copy HEADERS (alist, plist, or EQUAL hash-table) with each value V of NAME
   replaced by (FUNCALL FN NAME V)."
  (cond
    ((hash-table-p headers)
     (let ((out (make-hash-table :test (hash-table-test headers))))
       (maphash (lambda (k v) (setf (gethash k out) (funcall fn k v))) headers)
       out))
    ((and (consp headers) (atom (car headers)))
     ;; plist (:authorization "…" :accept "…")
     (loop for (k v) on headers by #'cddr
           collect k collect (funcall fn k v)))
    (t
     (mapcar (lambda (pair)
               (if (consp pair)
                   (cons (car pair) (funcall fn (car pair) (cdr pair)))
                   pair))
             headers))))

(defun protect-headers (headers)
  "Copy of HEADERS with each credential header's value wrapped in a SECRET.
   Accepts an alist, plist, or hash-table; other values are kept as they are."
  (%map-headers (lambda (name value)
                  (if (and (stringp value) (secret-header-name-p name))
                      (make-secret value)
                      value))
                headers))

(defun redact-headers (headers &key (placeholder "[redacted]"))
  "Copy of HEADERS for display: credential header values and SECRETs become
   PLACEHOLDER. Accepts an alist, plist, or hash-table."
  (%map-headers (lambda (name value)
                  (if (or (secret-p value) (secret-header-name-p name))
                      placeholder
                      value))
                headers))

(defun reveal-headers (headers)
  "Copy of HEADERS with every SECRET value unwrapped (for a backend's wire step)."
  (%map-headers (lambda (name value)
                  (declare (ignore name))
                  (reveal value))
                headers))

(defun %reveal-auth (auth)
  "AUTH with SECRET parts unwrapped: string | (:basic u p) | (:bearer tok)."
  (cond ((secret-p auth) (reveal auth))
        ((consp auth) (mapcar #'reveal auth))
        (t auth)))

(defun %auth-has-secret-p (auth)
  (or (secret-p auth)
      (and (consp auth) (some #'secret-p auth))))
