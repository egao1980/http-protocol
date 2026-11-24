(in-package #:http-protocol)

;;; Multipart/form-data (cl-stack#73). Stream / octets / string parts only — no FS.

(defun %stringify-field-name (name)
  (ctypecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))
    (character (string name))))

(defun normalize-multipart-file (name value)
  "Normalize a :FILES entry → part plist for the multipart encoder.
   VALUE is http-file | stream | octets | string | plist with :content.
   Content of another type (a vector of other elements included) →
   HTTP-PROTOCOL-ERROR here, not when the body is streamed (http-protocol#57)."
  (let ((field (or (and (http-file-p value) (http-file-field-name value))
                   (%stringify-field-name name))))
    (labels ((missing ()
               (error 'http-protocol-error
                      :message (format nil "files entry ~A missing :content" field)))
             (from-content (content &key filename content-type content-length)
               (%check-body-content content (format nil "files entry ~A content" field))
               (list :name field
                     :filename (or filename "blob")
                     :content-type (or content-type "application/octet-stream")
                     :content-length content-length
                     :content content)))
      (typecase value
        (http-file
         (from-content (http-file-content value)
                       :filename (http-file-filename value)
                       :content-type (http-file-content-type value)
                       :content-length (http-file-content-length value)))
        (null (missing))
        (cons
         (from-content (or (getf value :content) (missing))
                       :filename (getf value :filename)
                       :content-type (getf value :content-type)
                       :content-length (getf value :content-length)))
        (t (from-content value))))))

(defun normalize-multipart-data (data)
  "DATA alist ((name . value)…) of string/octets fields → part plists.
   A value of another type → HTTP-PROTOCOL-ERROR here (http-protocol#57)."
  (mapcar (lambda (pair)
            (let ((name (%stringify-field-name (car pair))))
              (list :name name
                    :filename nil
                    :content-type nil
                    :content (%check-body-content
                              (cdr pair) (format nil "multipart field ~A" name)))))
          data))

(defvar *boundary-random-state* nil
  "RANDOM-STATE that multipart boundaries are drawn from: process-local, seeded
   by (MAKE-RANDOM-STATE T) on first use (http-protocol#56). Not
   CL:*RANDOM-STATE*, which starts out the same in every fresh SBCL image, so
   the first boundary of each process would be the same string. Cleared before
   an image is saved, so each run of a saved image seeds its own.")

(defvar *boundary-lock* (bt:make-lock "http-protocol multipart boundary")
  "Held while a boundary is drawn: a RANDOM-STATE is not safe to share
   between threads.")

(defun %reset-boundary-random-state ()
  "Forget the boundary state; the next boundary seeds a fresh one. Runs
   before an image is saved."
  (setf *boundary-random-state* nil))

(progn
  ;; uiop:dump-image saves through save-lisp-and-die on SBCL, so SBCL's own
  ;; hook covers both ways to save; the UIOP hook serves the other Lisps.
  #+sbcl (pushnew '%reset-boundary-random-state sb-ext:*save-hooks*)
  #-sbcl (uiop:register-image-dump-hook '%reset-boundary-random-state))

(defun make-multipart-boundary ()
  "A fresh boundary: \"----cl-stack-\" and 32 lowercase hex digits, 128 bits
   drawn from *BOUNDARY-RANDOM-STATE* (http-protocol#56). 45 characters, all
   RFC 2046 bchars and HTTP token characters, so Content-Type needs no quotes.
   MAKE-MULTIPART-BODY scans in-memory parts for it; streamed parts cannot be
   scanned in advance and rely on these 128 random bits alone."
  (let ((n (bt:with-lock-held (*boundary-lock*)
             (random (ash 1 128)
                     (or *boundary-random-state*
                         (setf *boundary-random-state* (make-random-state t)))))))
    (format nil "----cl-stack-~(~32,'0x~)" n)))

(defun %part-body-length (part)
  (or (getf part :content-length)
      (let ((content (getf part :content)))
        (ctypecase content
          (null 0)
          (string (length (babel:string-to-octets content :encoding :utf-8)))
          ((vector (unsigned-byte 8)) (length content))
          (stream nil)))))

(defun %octets (string)
  (encoding-protocol:encode string :encoding :utf-8))

(defun %check-part-header-value (what value)
  "A part Content-Type with CR, LF or another control character would end
   the header line and forge headers: refuse it."
  (when (find-if (lambda (c)
                   (let ((code (char-code c)))
                     (or (and (< code 32) (/= code 9)) (= code 127))))
                 value)
    (error 'http-protocol-error
           :message (format nil "multipart part ~A contains a control character: ~S"
                            what value)))
  value)

(defun %part-header-string (part)
  "Content-Disposition (+ Content-Type) lines and the blank line for PART.
   name / filename go through mime-protocol (RFC 7578 quoting, filename*
   for non-ASCII), so they cannot inject parameters or headers."
  (let ((ct (getf part :content-type))
        (filename (let ((f (getf part :filename)))
                    (typecase f
                      (null nil)
                      (pathname (file-namestring f))
                      (t (string f))))))
    (with-output-to-string (o)
      (flet ((crlf () (write-char #\Return o) (write-char #\Newline o)))
        (write-string "Content-Disposition: " o)
        (write-string (mime-protocol:format-form-data-disposition
                       (getf part :name) :filename filename)
                      o)
        (crlf)
        (when ct
          (write-string "Content-Type: " o)
          (write-string (%check-part-header-value "Content-Type" ct) o)
          (crlf))
        (crlf)))))

(defun %multipart-bchar-p (char)
  "RFC 2046 5.1.1 bchars: DIGIT / ALPHA / ' ( ) + _ , - . / : = ? / space."
  (and (< (char-code char) 128)
       (or (alphanumericp char) (find char "'()+_,-./:=? "))))

(defun %check-multipart-boundary (boundary)
  "BOUNDARY when it is 1-70 bchars and does not end in a space (RFC 2046
   5.1.1), else HTTP-PROTOCOL-ERROR."
  (unless (and (stringp boundary)
               (<= 1 (length boundary) 70)
               (every #'%multipart-bchar-p boundary)
               (char/= #\Space (char boundary (1- (length boundary)))))
    (error 'http-protocol-error
           :message (format nil "multipart boundary ~S is not 1-70 RFC 2046 ~
                                 bchars ending in a non-space"
                            boundary)))
  boundary)

(defun %part-contains-delimiter-p (part delimiter delimiter-octets)
  "True when DELIMITER (\"--\" and the boundary) occurs in PART's header lines
   or in its in-memory content. A string is searched as characters: the
   delimiter is ASCII, so that equals searching its UTF-8 octets. Stream
   content cannot be scanned in advance and is not."
  (let ((content (getf part :content)))
    (or (search delimiter (%part-header-string part))
        (typecase content
          (string (search delimiter content))
          ((vector (unsigned-byte 8)) (search delimiter-octets content))))))

(defun %choose-multipart-boundary (parts boundary &key (replace t))
  "A boundary whose delimiter occurs in no in-memory part of PARTS (RFC 2046
   5.1.1). BOUNDARY, when given, is checked by %CHECK-MULTIPART-BOUNDARY and
   used unless it occurs in a part: then REPLACE draws a fresh one, and a NIL
   REPLACE signals HTTP-PROTOCOL-ERROR. Without BOUNDARY, a drawn boundary
   that occurs in a part is drawn again. Streamed parts are not scanned: they
   rely on the 128 random bits of MAKE-MULTIPART-BOUNDARY."
  (when boundary
    (%check-multipart-boundary boundary))
  (flet ((free-p (candidate)
           (let* ((delimiter (concatenate 'string "--" candidate))
                  (octets (%octets delimiter)))
             (notany (lambda (part) (%part-contains-delimiter-p part delimiter octets))
                     parts))))
    (cond ((and boundary (free-p boundary)) boundary)
          ((and boundary (not replace))
           (error 'http-protocol-error
                  :message (format nil "multipart boundary ~S occurs in a part" boundary)))
          (t
           (loop for attempt from 1 to 8
                 for candidate = (make-multipart-boundary)
                 when (free-p candidate)
                   return candidate
                 finally (error 'http-protocol-error
                                :message "8 drawn multipart boundaries all occur in a part"))))))

(defun %multipart-content-type (boundary)
  "Content-Type for a multipart/form-data body with BOUNDARY; mime-protocol
   quotes a boundary that is not an HTTP token."
  (mime-protocol:format-media-type
   (mime-protocol:make-media-type "multipart" "form-data"
                                  (list (cons "boundary" boundary)))))

(defun multipart-content-length (parts boundary)
  "Total Content-Length in octets when every part length is known; else NIL."
  (let ((blen (length (%octets boundary)))
        (total 0))
    (dolist (part parts)
      (let ((body-len (%part-body-length part)))
        (unless body-len
          (return-from multipart-content-length nil))
        ;; --boundary CRLF, headers (UTF-8 octets), body, CRLF
        (incf total (+ 2 blen 2
                       (length (%octets (%part-header-string part)))
                       body-len
                       2))))
    ;; closing --boundary-- CRLF
    (+ total 2 blen 2 2)))

(defun %part-header-octets (part boundary)
  (concatenate '(simple-array (unsigned-byte 8) (*))
               (%octets (format nil "--~A~C~C" boundary #\Return #\Newline))
               (%octets (%part-header-string part))))

;;; Streaming multipart reader — phases: :header :body :crlf :next :end :done

(defclass multipart-form-stream
    (trivial-gray-streams:fundamental-binary-input-stream)
  ((parts :initarg :parts :reader multipart-parts)
   (boundary :initarg :boundary :reader multipart-boundary)
   (index :initform 0 :accessor multipart-index)
   (phase :initform :header :accessor multipart-phase)
   (buf :initform nil :accessor multipart-buf)
   (buf-pos :initform 0 :accessor multipart-buf-pos)
   (body-stream :initform nil :accessor multipart-body-stream)
   (close-body-p :initform nil :accessor multipart-close-body-p)))

(defmethod stream-element-type ((s multipart-form-stream))
  '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-listen ((s multipart-form-stream))
  "True until the closing boundary was read: the next octet is a boundary or
   header octet, or comes from a part's content."
  (and (open-stream-p s)
       (not (eq (multipart-phase s) :done))))

(defmethod trivial-gray-streams:stream-clear-input ((s multipart-form-stream))
  nil)

(defun make-multipart-form-stream (parts &key boundary)
  "Binary input stream yielding multipart/form-data for PARTS (plist list).

   Without BOUNDARY a fresh one is drawn whose delimiter occurs in no
   in-memory part (string or octets, headers included); MULTIPART-BOUNDARY
   of the stream returns it. A given BOUNDARY must be 1-70 RFC 2046 bchars,
   and one that occurs in an in-memory part → HTTP-PROTOCOL-ERROR (the caller
   holds it, so it cannot be swapped here; MAKE-MULTIPART-BODY swaps it).
   Stream content cannot be scanned in advance: it relies on the 128 random
   bits of a drawn boundary (http-protocol#56)."
  (make-instance 'multipart-form-stream
                 :parts parts
                 :boundary (%choose-multipart-boundary parts boundary :replace nil)))

(defun %multipart-open-body (content)
  "Return (values binary-input-stream close-p). CONTENT is never a pathname."
  (ctypecase content
    (stream (values content nil))
    (string
     (values (make-octet-input-stream
              (babel:string-to-octets content :encoding :utf-8))
             t))
    ((vector (unsigned-byte 8))
     (values (make-octet-input-stream content) t))
    (null
     (values (make-octet-input-stream
              (make-array 0 :element-type '(unsigned-byte 8)))
             t))))

(defun %multipart-close-body (s)
  (when (multipart-close-body-p s)
    (ignore-errors (close (multipart-body-stream s))))
  (setf (multipart-body-stream s) nil
        (multipart-close-body-p s) nil))

(defun %multipart-start-part (s)
  "Load header octets for current index, open body stream, phase → :body."
  (let* ((parts (multipart-parts s))
         (i (multipart-index s))
         (boundary (multipart-boundary s)))
    (cond
      ((>= i (length parts))
       (setf (multipart-buf s)
             (%octets (format nil "--~A--~C~C" boundary #\Return #\Newline))
             (multipart-buf-pos s) 0
             (multipart-phase s) :closing))
      (t
       (let ((part (nth i parts)))
         (setf (multipart-buf s) (%part-header-octets part boundary)
               (multipart-buf-pos s) 0
               (multipart-phase s) :header)
         (multiple-value-bind (in close-p)
             (%multipart-open-body (getf part :content))
           (setf (multipart-body-stream s) in
                 (multipart-close-body-p s) close-p)))))))

(defun %multipart-advance (s)
  "After header/body/crlf buffer drained, move to next phase."
  (ecase (multipart-phase s)
    (:header
     ;; header done → read body bytes
     (setf (multipart-phase s) :body
           (multipart-buf s) nil
           (multipart-buf-pos s) 0))
    (:body
     ;; body EOF → emit CRLF
     (setf (multipart-phase s) :crlf
           (multipart-buf s) (%octets (format nil "~C~C" #\Return #\Newline))
           (multipart-buf-pos s) 0))
    (:crlf
     (%multipart-close-body s)
     (incf (multipart-index s))
     (%multipart-start-part s))
    (:closing
     (setf (multipart-phase s) :done
           (multipart-buf s) nil))
    (:done nil)))

;; Initialize on first read
(defmethod initialize-instance :after ((s multipart-form-stream) &key)
  (%multipart-start-part s))

(defmethod trivial-gray-streams:stream-read-byte ((s multipart-form-stream))
  (loop
    (when (eq (multipart-phase s) :done)
      (return :eof))
    (when (and (eq (multipart-phase s) :body)
               (multipart-body-stream s)
               (or (null (multipart-buf s))
                   (>= (multipart-buf-pos s) (length (multipart-buf s)))))
      (let ((b (read-byte (multipart-body-stream s) nil :eof)))
        (if (eq b :eof)
            (%multipart-advance s)
            (return b))))
    (when (and (multipart-buf s)
               (< (multipart-buf-pos s) (length (multipart-buf s))))
      (let ((p (multipart-buf-pos s)))
        (return (prog1 (aref (multipart-buf s) p)
                  (incf (multipart-buf-pos s))
                  (when (>= (multipart-buf-pos s) (length (multipart-buf s)))
                    (%multipart-advance s))))))
    ;; buffer empty / no body byte — advance or eof
    (if (eq (multipart-phase s) :done)
        (return :eof)
        (%multipart-advance s))))

(defmethod trivial-gray-streams:stream-read-sequence
    ((s multipart-form-stream) seq start end &key)
  (let ((pos start))
    (loop while (< pos end)
          do (let ((b (trivial-gray-streams:stream-read-byte s)))
               (when (eq b :eof) (return))
               (setf (aref seq pos) b)
               (incf pos)))
    pos))

(defmethod close ((s multipart-form-stream) &key abort)
  (declare (ignore abort))
  (when (open-stream-p s)
    (when (multipart-close-body-p s)
      (ignore-errors (close (multipart-body-stream s))))
    (call-next-method)))

(defun build-multipart-parts (data files)
  "Combine :DATA and :FILES into part plists (files after fields).
   FILES may be an alist ((name . value)…) or a list of HTTP-FILE."
  (append (normalize-multipart-data (or data '()))
          (cond
            ((null files) '())
            ((and (consp files) (http-file-p (first files)))
             (mapcar (lambda (f)
                       (normalize-multipart-file
                        (or (http-file-field-name f) "file") f))
                     files))
            (t
             (mapcar (lambda (pair)
                       (normalize-multipart-file (car pair) (cdr pair)))
                     files)))))

(defun make-multipart-body (data files &key boundary)
  "Returns (values stream content-type-header content-length-or-nil boundary).

   BOUNDARY defaults to a fresh MAKE-MULTIPART-BOUNDARY; a given one must be
   1-70 RFC 2046 bchars (else HTTP-PROTOCOL-ERROR). A boundary whose
   delimiter (\"--\" and the boundary) occurs in an in-memory part (string or
   octets, headers included) is replaced by a fresh one (RFC 2046 5.1.1,
   http-protocol#56), so use the returned Content-Type and boundary. Stream
   content cannot be scanned in advance: it relies on the 128 random bits of
   a drawn boundary."
  (let* ((parts (build-multipart-parts data files))
         (boundary (%choose-multipart-boundary parts boundary))
         (stream (make-buffered-binary-input-stream
                  (make-instance 'multipart-form-stream :parts parts :boundary boundary)))
         (ct (%multipart-content-type boundary))
         (clen (multipart-content-length parts boundary)))
    (values stream ct clen boundary)))

(defun prepare-request-body (request &key (buffer-size *http-stream-buffer-size*))
  "Resolve CONTENT / DATA / FORM-DATA / FILES → wire body for a backend.

   Returns (values wire-content extra-header-alist content-length-or-nil).

   Body rules:
   - :FILES (optional :FORM-DATA) → multipart/form-data
   - :FORM-DATA alone → application/x-www-form-urlencoded
   - :DATA → ENCODE-HTTP-DATA (:DATA-TYPE + Content-Type header if set)
   - :CONTENT → raw body

   :CONTENT, :DATA, and :FORM-DATA/:FILES are mutually exclusive."
  (declare (ignore buffer-size))
  (let ((content (http-request-content request))
        (data (http-request-data request))
        (form-data (http-request-form-data request))
        (files (http-request-files request))
        (coding (http-request-content-encoding request)))
    (when (> (count-if #'identity
                       (list (not (null content))
                             (not (null data))
                             (or form-data files)))
             1)
      (error 'http-protocol-error
             :message "specify only one of :content, :data, or :form-data/:files"))
    (cond
      (files
       (multiple-value-bind (stream ct clen)
           (make-multipart-body form-data files)
         (values stream (list (cons "content-type" ct)) clen)))
      (form-data
       (let ((octets (etypecase form-data
                       (list (encode-urlencoded form-data))
                       (string (babel:string-to-octets form-data :encoding :utf-8))
                       ((vector (unsigned-byte 8)) form-data))))
         (multiple-value-bind (wire ce)
             (prepare-request-content octets :coding coding)
           (let ((extra (list (cons "content-type"
                                    "application/x-www-form-urlencoded"))))
             (when ce (setf extra (acons "content-encoding" ce extra)))
             (values wire extra (when (vectorp wire) (length wire)))))))
      (data
       (multiple-value-bind (wire ct clen)
           (encode-http-data data
                             (http-request-data-type request)
                             (request-header-content-type request))
         (multiple-value-bind (wire* ce)
             (prepare-request-content wire :coding coding)
           (let ((extra (when ct (list (cons "content-type" ct)))))
             (when ce (setf extra (acons "content-encoding" ce extra)))
             (values wire* extra
                     (or clen (when (vectorp wire*) (length wire*))))))))
      ((http-file-p content)
       (multiple-value-bind (wire ce)
           (prepare-request-content content :coding coding)
         (let ((extra (when ce (list (cons "content-encoding" ce))))
               (ct (http-file-content-type content))
               (clen (http-file-content-length content)))
           (when ct
             (setf extra (acons "content-type" ct extra)))
           (values wire extra (or clen (when (vectorp wire) (length wire)))))))
      (t
       (multiple-value-bind (wire ce)
           (prepare-request-content content :coding coding)
         (values wire
                 (when ce (list (cons "content-encoding" ce)))
                 (when (vectorp wire) (length wire))))))))
