(in-package #:http-protocol)

;;; HTTP Content-Encoding protocol (RFC 9110 §8.4).
;;; Distinct from cl-mime DECODE-CONTENT / ENCODE-CONTENT (CTE).
;;;
;;; Backends (separate packages, event-backend pattern):
;;;   http-encoding-chipz   — :gzip :deflate
;;;   http-encoding-brotli  — :br     (soft; needs cl-stack-brotli natives)
;;;   http-encoding-zstd    — :zstd   (soft; needs cl-stack-zstd natives)
;;;   http-encoding-snappy  — :snappy (soft; needs cl-stack-snappy natives; raw)
;;;
;;; No plugin registry: load the ASDF system; methods appear. Soft-load
;;; probes *content-coding-systems* for Accept-Encoding.

(defparameter *content-coding-systems*
  '((:gzip . "http-encoding-chipz")
    (:deflate . "http-encoding-chipz")
    (:br . "http-encoding-brotli")
    (:zstd . "http-encoding-zstd")
    (:snappy . "http-encoding-snappy"))
  "Alist coding → ASDF system that specializes DECODE/ENCODE-CONTENT-CODING.")

(defvar *warned-missing-overlays* nil)
(defvar *coding-availability* (make-hash-table :test #'eq)
  "Cache successful coding availability probes: coding → T.")

(defun normalize-content-coding (coding)
  "Return a keyword coding (:gzip :deflate :br :zstd :snappy :identity) or NIL if blank."
  (etypecase coding
    (null nil)
    (keyword
     (case coding
       ((:gzip :x-gzip) :gzip)
       ((:deflate :zlib) :deflate)
       ((:br :brotli) :br)
       ((:zstd :zstandard) :zstd)
       ((:snappy :x-snappy) :snappy)
       ((:identity) :identity)
       (otherwise coding)))
    (string
     (let ((s (string-downcase (string-trim '(#\Space #\Tab) coding))))
       (cond ((string= s "") nil)
             ((or (string= s "gzip") (string= s "x-gzip")) :gzip)
             ((or (string= s "deflate") (string= s "zlib")) :deflate)
             ((or (string= s "br") (string= s "brotli")) :br)
             ((or (string= s "zstd") (string= s "zstandard")) :zstd)
             ((or (string= s "snappy") (string= s "x-snappy")) :snappy)
             ((string= s "identity") :identity)
             (t (intern (string-upcase s) :keyword)))))
    (symbol (normalize-content-coding (string coding)))))

(defun parse-content-encoding (header)
  "Parse Content-Encoding / Accept-Encoding into coding keywords (header order).
   Accept-Encoding q-values ignored (presence only)."
  (when (null header)
    (return-from parse-content-encoding '()))
  (let ((s (etypecase header
             (string header)
             (symbol (string header)))))
    (loop for part in (uiop:split-string s :separator '(#\,))
          for token = (string-trim '(#\Space #\Tab) part)
          for coding-part = (subseq token 0 (or (position #\; token) (length token)))
          for coding = (normalize-content-coding (string-trim '(#\Space #\Tab) coding-part))
          when coding collect coding)))

(defun coerce-to-octets (input)
  "Coerce INPUT (octet vector or string) to a simple (unsigned-byte 8) vector."
  (etypecase input
    ((simple-array (unsigned-byte 8) (*)) input)
    ((vector (unsigned-byte 8))
     (make-array (length input) :element-type '(unsigned-byte 8) :initial-contents input))
    (string
     (babel:string-to-octets input :encoding :utf-8))))

;; Back-compat internal name used by around methods.
(defun %octet-vector (input) (coerce-to-octets input))

(defun %nested-asdf-operate-p ()
  "True when ASDF is mid-OPERATE (soft-load must not nest)."
  (let* ((session-sym (find-symbol "*ASDF-SESSION*" :asdf))
         (session (and session-sym (boundp session-sym) (symbol-value session-sym)))
         (visiting-sym (or (find-symbol "SESSION-VISITING-ACTION-SET" :asdf)
                           (find-symbol "SESSION-VISITING-ACTION-SET" :asdf/session))))
    (and session visiting-sym (fboundp visiting-sym)
         (let ((table (ignore-errors (funcall visiting-sym session))))
           (and (hash-table-p table) (plusp (hash-table-count table)))))))

(defun %try-asdf-system (name)
  "Load NAME if present. Skips nested ASDF OPERATE during TEST-OP."
  (let ((sys (asdf:find-system name nil)))
    (cond ((null sys) nil)
          ((asdf:component-loaded-p sys) t)
          ((%nested-asdf-operate-p) nil)
          (t
           (handler-case
               (progn (asdf:load-system sys) t)
             (error () nil))))))

(defun %probe-coding (coding)
  "True if backend for CODING is loadable and round-trips 4 bytes."
  (let* ((c (normalize-content-coding coding))
         (sys (cdr (assoc c *content-coding-systems*))))
    (and sys
         (%try-asdf-system sys)
         (handler-case
             (let* ((raw (make-array 4 :element-type '(unsigned-byte 8)
                                     :initial-contents '(1 2 3 4)))
                    (enc (encode-content-coding c raw))
                    (dec (decode-content-coding c enc)))
               (equalp raw dec))
           (error () nil)))))

(defun %coding-available-p (coding)
  (let* ((c (normalize-content-coding coding)))
    (or (gethash c *coding-availability*)
        (let ((result (%probe-coding c)))
          (when result
            (setf (gethash c *coding-availability*) t))
          result))))

(defun %warn-missing (coding)
  (unless (member coding *warned-missing-overlays*)
    (push coding *warned-missing-overlays*)
    (warn "Content-Encoding ~S unavailable (backend ~S not loaded); omitting from Accept-Encoding"
          coding
          (cdr (assoc coding *content-coding-systems*)))))

(defun content-coding-supported-p (coding)
  "True if we can decode (and encode) CODING."
  (let ((c (normalize-content-coding coding)))
    (case c
      (:identity t)
      ((:gzip :deflate :br :zstd :snappy) (%coding-available-p c))
      (otherwise nil))))

(defun available-content-codings (&key (warn t))
  "Codings we can decode, preference order for Accept-Encoding."
  (let ((out '()))
    (dolist (c '(:gzip :deflate :br :zstd :snappy))
      (if (content-coding-supported-p c)
          (setf out (nconc out (list c)))
          (when warn (%warn-missing c))))
    out))

(defun default-accept-encoding (&key (as :string))
  "Default Accept-Encoding from available backends.
   AS :string → header value; :list → keyword list."
  (let ((codings (available-content-codings :warn t)))
    (ecase as
      (:list codings)
      (:string
       (format nil "~{~(~A~)~^,~}" codings)))))

(defgeneric decode-content-coding (coding input &key)
  (:documentation
   "Decode HTTP Content-Encoding CODING over INPUT.
    Octets → octet vector; binary input stream → decompressing stream.
    Backends specialize on coding keywords."))

(defgeneric encode-content-coding (coding input &key level quality)
  (:documentation
   "Encode INPUT with HTTP Content-Encoding CODING.
    Octets → octet vector; binary input stream → encoding stream.
    Backends specialize on coding keywords."))

(defmethod decode-content-coding :around (coding input &key)
  (let ((c (normalize-content-coding coding)))
    (cond ((null c)
           (if (streamp input) input (%octet-vector input)))
          (t (call-next-method c input)))))

(defmethod encode-content-coding :around (coding input &key level quality)
  (let ((c (normalize-content-coding coding)))
    (cond ((null c)
           (if (streamp input) input (%octet-vector input)))
          (t (call-next-method c input :level level :quality quality)))))

(defmethod decode-content-coding ((coding (eql :identity)) input &key)
  (if (streamp input) input (%octet-vector input)))

(defmethod encode-content-coding ((coding (eql :identity)) input &key level quality)
  (declare (ignore level quality))
  (if (streamp input) input (%octet-vector input)))

(defmethod decode-content-coding (coding input &key)
  (declare (ignore input))
  (error 'unsupported-content-coding :coding coding
         :message (format nil "no backend loaded for ~S (expected system ~S)"
                          coding
                          (cdr (assoc (normalize-content-coding coding)
                                      *content-coding-systems*)))))

(defmethod encode-content-coding (coding input &key level quality)
  (declare (ignore input level quality))
  (error 'unsupported-content-coding :coding coding
         :message (format nil "no backend loaded for ~S (expected system ~S)"
                          coding
                          (cdr (assoc (normalize-content-coding coding)
                                      *content-coding-systems*)))))

(defun make-decoding-stream (coding source)
  "Binary input stream decoding CODING from SOURCE (delegates to DECODE-CONTENT-CODING)."
  (check-type source stream)
  (decode-content-coding coding source))

(defun make-encoding-stream (coding source &key level quality)
  "Binary input stream encoding SOURCE with CODING (delegates to ENCODE-CONTENT-CODING)."
  (check-type source stream)
  (encode-content-coding coding source :level level :quality quality))

(defun decode-content-codings (codings input)
  "Apply CODINGS in reverse (wire order is outer-first)."
  (reduce (lambda (acc coding)
            (decode-content-coding coding acc))
          (reverse codings)
          :initial-value (if (streamp input) input (%octet-vector input))))

(defun encode-content-codings (codings input &rest keys &key &allow-other-keys)
  "Apply CODINGS left-to-right (first coding is outermost on the wire)."
  (reduce (lambda (acc coding)
            (apply #'encode-content-coding coding acc keys))
          codings
          :initial-value (if (streamp input) input (%octet-vector input))))
