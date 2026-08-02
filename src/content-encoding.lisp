(in-package #:http-protocol)

;;; HTTP Content-Encoding pipeline (RFC 9110 §8.4).
;;; Distinct from cl-mime's DECODE-CONTENT / ENCODE-CONTENT (transfer encodings).

(defvar *warned-missing-overlays* nil
  "Codings we already warned about when probing overlays.")

(defvar *brotli-available* :unknown)
(defvar *zstd-available* :unknown)

(defun normalize-content-coding (coding)
  "Return a keyword coding (:gzip :deflate :br :zstd :identity) or NIL if blank."
  (etypecase coding
    (null nil)
    (keyword
     (case coding
       ((:gzip :x-gzip) :gzip)
       ((:deflate :zlib) :deflate)
       ((:br :brotli) :br)
       ((:zstd :zstandard) :zstd)
       ((:identity) :identity)
       (otherwise coding)))
    (string
     (let ((s (string-downcase (string-trim '(#\Space #\Tab) coding))))
       (cond ((string= s "") nil)
             ((or (string= s "gzip") (string= s "x-gzip")) :gzip)
             ((or (string= s "deflate") (string= s "zlib")) :deflate)
             ((or (string= s "br") (string= s "brotli")) :br)
             ((or (string= s "zstd") (string= s "zstandard")) :zstd)
             ((string= s "identity") :identity)
             (t (intern (string-upcase s) :keyword)))))
    (symbol (normalize-content-coding (string coding)))))

(defun parse-content-encoding (header)
  "Parse a Content-Encoding / Accept-Encoding header value into coding keywords.
   Accept-Encoding q-values are ignored (presence only). Returns list in header order."
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

(defun %octet-vector (input)
  (etypecase input
    ((simple-array (unsigned-byte 8) (*)) input)
    ((vector (unsigned-byte 8))
     (make-array (length input) :element-type '(unsigned-byte 8) :initial-contents input))
    (string
     (map '(simple-array (unsigned-byte 8) (*)) #'char-code input))))

(defun %try-asdf-system (name)
  "Load NAME if present. Skips nested ASDF OPERATE during TEST-OP (soft overlay)."
  (let ((sys (asdf:find-system name nil)))
    (cond ((null sys) nil)
          ((asdf:component-loaded-p sys) t)
          ;; Nested OPERATE during TEST-OP is deprecated; treat as unavailable
          ;; unless the overlay was already loaded by the consumer/CI.
          ((and (boundp 'asdf::*asdf-session*)
                asdf::*asdf-session*
                (plusp (hash-table-count
                        (asdf::session-visiting-action-set asdf::*asdf-session*))))
           nil)
          (t
           (handler-case
               (progn (asdf:load-system sys) t)
             (error () nil))))))

(defun %probe-overlay (system package compress-name decompress-name &key (quality 1) (level 1))
  (and (%try-asdf-system system)
       (find-package package)
       (handler-case
           (let* ((raw (make-array 4 :element-type '(unsigned-byte 8)
                                   :initial-contents '(1 2 3 4)))
                  (compress (find-symbol compress-name package))
                  (decompress (find-symbol decompress-name package))
                  (enc (if (eq package :cl-stack-brotli)
                           (funcall compress raw :quality quality)
                           (funcall compress raw :level level)))
                  (dec (funcall decompress enc)))
             (equalp raw dec))
         (error () nil))))

(defun %brotli-available-p ()
  (when (eq *brotli-available* :unknown)
    (setf *brotli-available*
          (%probe-overlay "cl-stack-brotli" :cl-stack-brotli "COMPRESS" "DECOMPRESS"
                          :quality 1)))
  *brotli-available*)

(defun %zstd-available-p ()
  (when (eq *zstd-available* :unknown)
    (setf *zstd-available*
          (%probe-overlay "cl-stack-zstd" :cl-stack-zstd "COMPRESS" "DECOMPRESS"
                          :level 1)))
  *zstd-available*)

(defun %warn-missing (coding)
  (unless (member coding *warned-missing-overlays*)
    (push coding *warned-missing-overlays*)
    (warn "Content-Encoding ~S unavailable (overlay not loaded); omitting from Accept-Encoding"
          coding)))

(defun content-coding-supported-p (coding)
  "True if we can decode (and encode) CODING."
  (let ((c (normalize-content-coding coding)))
    (case c
      ((:gzip :deflate :identity) t)
      (:br (%brotli-available-p))
      (:zstd (%zstd-available-p))
      (otherwise nil))))

(defun available-content-codings (&key (warn t))
  "Codings we can decode, preference order for Accept-Encoding."
  (let ((out '(:gzip :deflate)))
    (if (%brotli-available-p)
        (setf out (append out '(:br)))
        (when warn (%warn-missing :br)))
    (if (%zstd-available-p)
        (setf out (append out '(:zstd)))
        (when warn (%warn-missing :zstd)))
    out))

(defun default-accept-encoding (&key (as :string))
  "Default Accept-Encoding tokens for supported codings.
   AS :string → header value; :list → keyword list."
  (let ((codings (available-content-codings :warn t)))
    (ecase as
      (:list codings)
      (:string
       (format nil "~{~(~A~)~^,~}" codings)))))

(defgeneric decode-content-coding (coding input &key)
  (:documentation "Decode HTTP Content-Encoding CODING over INPUT (octets). → octet vector."))

(defgeneric encode-content-coding (coding input &key level quality)
  (:documentation "Encode INPUT octets with HTTP Content-Encoding CODING. → octet vector."))

(defmethod decode-content-coding :around (coding input &key)
  (let ((c (normalize-content-coding coding)))
    (if c
        (call-next-method c input)
        (%octet-vector input))))

(defmethod encode-content-coding :around (coding input &key level quality)
  (let ((c (normalize-content-coding coding)))
    (if c
        (call-next-method c input :level level :quality quality)
        (%octet-vector input))))

(defmethod decode-content-coding ((coding (eql :identity)) input &key)
  (%octet-vector input))

(defmethod encode-content-coding ((coding (eql :identity)) input &key level quality)
  (declare (ignore level quality))
  (%octet-vector input))

(defmethod decode-content-coding ((coding (eql :gzip)) input &key)
  (chipz:decompress nil 'chipz:gzip (%octet-vector input)))

(defmethod encode-content-coding ((coding (eql :gzip)) input &key level quality)
  (declare (ignore level quality))
  (salza2:compress-data (%octet-vector input) 'salza2:gzip-compressor))

;;; HTTP "deflate" is zlib-wrapped in practice (browsers / httpx).
(defmethod decode-content-coding ((coding (eql :deflate)) input &key)
  (let ((octets (%octet-vector input)))
    (handler-case
        (chipz:decompress nil 'chipz:zlib octets)
      (error ()
        (chipz:decompress nil 'chipz:deflate octets)))))

(defmethod encode-content-coding ((coding (eql :deflate)) input &key level quality)
  (declare (ignore level quality))
  (salza2:compress-data (%octet-vector input) 'salza2:zlib-compressor))

(defmethod decode-content-coding ((coding (eql :br)) input &key)
  (unless (content-coding-supported-p :br)
    (error 'unsupported-content-coding :coding :br
           :message "cl-stack-brotli overlay not available"))
  (funcall (find-symbol "DECOMPRESS" :cl-stack-brotli) (%octet-vector input)))

(defmethod encode-content-coding ((coding (eql :br)) input &key level quality)
  (declare (ignore level))
  (unless (content-coding-supported-p :br)
    (error 'unsupported-content-coding :coding :br
           :message "cl-stack-brotli overlay not available"))
  (funcall (find-symbol "COMPRESS" :cl-stack-brotli)
           (%octet-vector input)
           :quality (or quality 5)))

(defmethod decode-content-coding ((coding (eql :zstd)) input &key)
  (unless (content-coding-supported-p :zstd)
    (error 'unsupported-content-coding :coding :zstd
           :message "cl-stack-zstd overlay not available"))
  (funcall (find-symbol "DECOMPRESS" :cl-stack-zstd) (%octet-vector input)))

(defmethod encode-content-coding ((coding (eql :zstd)) input &key level quality)
  (declare (ignore quality))
  (unless (content-coding-supported-p :zstd)
    (error 'unsupported-content-coding :coding :zstd
           :message "cl-stack-zstd overlay not available"))
  (funcall (find-symbol "COMPRESS" :cl-stack-zstd)
           (%octet-vector input)
           :level (or level 3)))

(defmethod decode-content-coding (coding input &key)
  (declare (ignore input))
  (error 'unsupported-content-coding :coding coding))

(defmethod encode-content-coding (coding input &key level quality)
  (declare (ignore input level quality))
  (error 'unsupported-content-coding :coding coding))

(defun decode-content-codings (codings input)
  "Apply CODINGS in reverse (wire order is outer-first)."
  (reduce (lambda (octets coding)
            (decode-content-coding coding octets))
          (reverse codings)
          :initial-value (%octet-vector input)))

(defun encode-content-codings (codings input &rest keys &key &allow-other-keys)
  "Apply CODINGS left-to-right (first coding is outermost on the wire)."
  (reduce (lambda (octets coding)
            (apply #'encode-content-coding coding octets keys))
          codings
          :initial-value (%octet-vector input)))
