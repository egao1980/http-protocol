(in-package #:http-protocol)

;;; Multipart/form-data (cl-stack#73). Stream / octets / string parts only — no FS.

(defun %stringify-field-name (name)
  (ctypecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))
    (character (string name))))

(defun normalize-multipart-file (name value)
  "Normalize a :FILES entry → part plist for the multipart encoder.
   VALUE is http-file | stream | octets | string | plist with :content."
  (let ((field (or (and (http-file-p value) (http-file-field-name value))
                   (%stringify-field-name name))))
    (labels ((from-content (content &key filename content-type content-length)
               (check-type content (or stream string vector null))
               (list :name field
                     :filename (or filename "blob")
                     :content-type (or content-type "application/octet-stream")
                     :content-length content-length
                     :content content)))
      (ctypecase value
        (http-file
         (from-content (http-file-content value)
                       :filename (http-file-filename value)
                       :content-type (http-file-content-type value)
                       :content-length (http-file-content-length value)))
        (stream (from-content value))
        ((or string vector) (from-content value))
        (cons
         (from-content (or (getf value :content)
                           (error 'http-protocol-error
                                  :message (format nil "files entry ~A missing :content"
                                                   field)))
                       :filename (getf value :filename)
                       :content-type (getf value :content-type)
                       :content-length (getf value :content-length)))))))

(defun normalize-multipart-data (data)
  "DATA alist ((name . value)…) of string/octets fields → part plists."
  (mapcar (lambda (pair)
            (list :name (%stringify-field-name (car pair))
                  :filename nil
                  :content-type nil
                  :content (cdr pair)))
          data))

(defun make-multipart-boundary ()
  (format nil "----cl-stack-~A" (random (expt 36 10))))

(defun %part-body-length (part)
  (or (getf part :content-length)
      (let ((content (getf part :content)))
        (ctypecase content
          (null 0)
          (string (length (babel:string-to-octets content :encoding :utf-8)))
          (vector (length content))
          (stream nil)))))

(defun multipart-content-length (parts boundary)
  "Total Content-Length when every part length is known; else NIL."
  (let ((blen (length boundary))
        (total 0))
    (dolist (part parts)
      (let ((body-len (%part-body-length part)))
        (unless body-len
          (return-from multipart-content-length nil))
        (let* ((disp (format nil "Content-Disposition: form-data; name=\"~A\"~@[; filename=\"~A\"~]~C~C"
                             (getf part :name)
                             (getf part :filename)
                             #\Return #\Newline))
               (ct (getf part :content-type))
               (ct-line (when ct
                          (format nil "Content-Type: ~A~C~C" ct #\Return #\Newline))))
          (incf total (+ 2 blen 2
                         (length disp)
                         (if ct-line (length ct-line) 0)
                         2
                         body-len
                         2)))))
    ;; closing --boundary-- CRLF
    (+ total 2 blen 2 2)))

(defun %octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun %part-header-octets (part boundary)
  (let ((out (make-array 128 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0)))
    (labels ((emit (oct)
               (loop for b across oct do (vector-push-extend b out))))
      (emit (%octets (format nil "--~A~C~C" boundary #\Return #\Newline)))
      (emit (%octets
             (format nil "Content-Disposition: form-data; name=\"~A\"~@[; filename=\"~A\"~]~C~C"
                     (getf part :name)
                     (getf part :filename)
                     #\Return #\Newline)))
      (when (getf part :content-type)
        (emit (%octets (format nil "Content-Type: ~A~C~C"
                               (getf part :content-type)
                               #\Return #\Newline))))
      (emit (%octets (format nil "~C~C" #\Return #\Newline)))
      (coerce out '(simple-array (unsigned-byte 8) (*))))))

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

(defun make-multipart-form-stream (parts &key (boundary (make-multipart-boundary)))
  "Binary input stream yielding multipart/form-data for PARTS (plist list)."
  (make-instance 'multipart-form-stream :parts parts :boundary boundary))

(defun %multipart-open-body (content)
  "Return (values binary-input-stream close-p). CONTENT is never a pathname."
  (ctypecase content
    (stream (values content nil))
    (string
     (values (make-octet-input-stream
              (babel:string-to-octets content :encoding :utf-8))
             t))
    (vector
     (values (make-octet-input-stream content) t))
    (null
     (values (make-octet-input-stream #()) t))))

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
  "Returns (values stream content-type-header content-length-or-nil boundary)."
  (let* ((boundary (or boundary (make-multipart-boundary)))
         (parts (build-multipart-parts data files))
         (stream (make-buffered-binary-input-stream
                  (make-multipart-form-stream parts :boundary boundary)))
         (ct (format nil "multipart/form-data; boundary=~A" boundary))
         (clen (multipart-content-length parts boundary)))
    (values stream ct clen boundary)))

(defun prepare-request-body (request &key (buffer-size *http-stream-buffer-size*))
  "Resolve CONTENT / DATA / FILES → wire body for a backend.

   Returns (values wire-content extra-header-alist content-length-or-nil).
   Wire content is a stream, octet vector, or NIL.

   Body rules (requests/httpx-shaped):
   - :FILES (with optional :DATA) → multipart/form-data
   - :DATA alone (alist) → application/x-www-form-urlencoded
   - :CONTENT → raw body (octets/string/stream/http-file)"
  (declare (ignore buffer-size))
  (let ((content (http-request-content request))
        (data (http-request-data request))
        (files (http-request-files request))
        (coding (http-request-content-encoding request)))
    (when (and content (or data files))
      (error 'http-protocol-error
             :message "specify either :content or :data/:files, not both"))
    (cond
      (files
       (multiple-value-bind (stream ct clen)
           (make-multipart-body data files)
         (values stream (list (cons "content-type" ct)) clen)))
      (data
       (let ((octets (etypecase data
                       (list (encode-urlencoded data))
                       (string (babel:string-to-octets data :encoding :utf-8))
                       ((vector (unsigned-byte 8)) data))))
         (multiple-value-bind (wire ce)
             (prepare-request-content octets :coding coding)
           (let ((extra (list (cons "content-type"
                                    "application/x-www-form-urlencoded"))))
             (when ce (setf extra (acons "content-encoding" ce extra)))
             (values wire extra (when (vectorp wire) (length wire)))))))
      ((http-file-p content)
       ;; Single-file body (e.g. PUT): stream content; optional length/type.
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
