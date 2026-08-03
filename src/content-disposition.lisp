(in-package #:http-protocol)

;;; Content-Disposition — RFC 6266 (filename / filename*)
;;; ext-value encoding — RFC 8187 (obsoletes RFC 5987)

(defun %cd-whitespace-p (c)
  (or (char= c #\Space) (char= c #\Tab)))

(defun %cd-skip-ws (s i)
  (loop while (and (< i (length s)) (%cd-whitespace-p (char s i)))
        do (incf i)
        finally (return i)))

(defun %cd-token-char-p (c)
  "RFC 9110 token (excludes separators / CTL)."
  (let ((code (char-code c)))
    (and (<= 33 code 126)
         (not (find c "()<>@,;:\\\"/[]?={} " :test #'char=)))))

(defun %cd-parse-token (s i)
  "Return (values token next-index) or NIL."
  (let ((start i))
    (loop while (and (< i (length s)) (%cd-token-char-p (char s i)))
          do (incf i))
    (when (> i start)
      (values (subseq s start i) i))))

(defun %cd-parse-quoted-string (s i)
  "RFC 9110 quoted-string with quoted-pair. Return (values text next) or NIL."
  (unless (and (< i (length s)) (char= (char s i) #\"))
    (return-from %cd-parse-quoted-string nil))
  (incf i)
  (let ((out (make-array 16 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop
      (when (>= i (length s))
        (return-from %cd-parse-quoted-string nil))
      (let ((c (char s i)))
        (cond
          ((char= c #\")
           (return-from %cd-parse-quoted-string (values (coerce out 'string) (1+ i))))
          ((char= c #\\)
           (incf i)
           (when (>= i (length s))
             (return-from %cd-parse-quoted-string nil))
           (vector-push-extend (char s i) out)
           (incf i))
          (t
           (vector-push-extend c out)
           (incf i)))))))

(defun %cd-parse-value (s i)
  "value = token / quoted-string (RFC 6266 → RFC 9110)."
  (setf i (%cd-skip-ws s i))
  (when (>= i (length s))
    (return-from %cd-parse-value nil))
  (if (char= (char s i) #\")
      (%cd-parse-quoted-string s i)
      (%cd-parse-token s i)))

(defun %cd-parse-ext-value-token (s i)
  "Scan RFC 8187 ext-value token (charset'lang'value-chars) until ';' / WS / end."
  (setf i (%cd-skip-ws s i))
  (let ((start i))
    (loop while (and (< i (length s))
                     (not (char= (char s i) #\;))
                     (not (%cd-whitespace-p (char s i))))
          do (incf i))
    (when (> i start)
      (values (subseq s start i) i))))

;;; MIME charset → babel encoding (RFC 8187 mime-charset). UTF-8 preferred on
;;; the wire; recipients SHOULD decode any charset they implement (babel).
(defparameter *cd-charset-aliases*
  '(("UTF8" . :utf-8)
    ("UTF-8" . :utf-8)
    ("ISO8859-1" . :iso-8859-1)
    ("ISO-8859-1" . :iso-8859-1)
    ("LATIN1" . :iso-8859-1)
    ("LATIN-1" . :iso-8859-1)
    ("WINDOWS-1252" . :cp1252)
    ("CP1252" . :cp1252)
    ("SHIFT_JIS" . :cp932)
    ("SHIFT-JIS" . :cp932)
    ("SJIS" . :cp932)
    ("EUC-JP" . :eucjp)
    ("EUCJP" . :eucjp)))

(defun %cd-charset-encoding (charset)
  "Map RFC 8187 charset token → babel encoding keyword, or NIL if unsupported."
  (let* ((up (string-upcase (string-trim '(#\Space #\Tab) charset)))
         (enc (or (cdr (assoc up *cd-charset-aliases* :test #'string=))
                  (intern up :keyword))))
    (handler-case
        (progn (babel:make-external-format enc) enc)
      (error () nil))))

(defun %cd-decode-ext-value (raw)
  "RFC 8187 §3.2: charset ' [language] ' value-chars.

   Percent-decode value-chars to octets, then decode with babel using CHARSET.
   Unknown/unsupported charset → NIL (caller falls back to filename=)."
  (let* ((raw (string-trim '(#\Space #\Tab) raw))
         (q1 (position #\' raw)))
    (unless q1
      (return-from %cd-decode-ext-value nil))
    (let* ((charset (subseq raw 0 q1))
           (rest (subseq raw (1+ q1)))
           (q2 (position #\' rest)))
      (unless q2
        (return-from %cd-decode-ext-value nil))
      (let ((enc (%cd-charset-encoding charset))
            (value-chars (subseq rest (1+ q2))))
        (unless enc
          (return-from %cd-decode-ext-value nil))
        (handler-case
            (quri:url-decode value-chars :encoding enc)
          (error () nil))))))

(defun %cd-sanitize-filename (name)
  "RFC 6266 §4.3: treat filename as advisory; keep last path segment only."
  (when (and name (plusp (length name)))
    (let* ((n (substitute #\/ #\\ name))
           (slash (position #\/ n :from-end t))
           (base (if slash (subseq n (1+ slash)) n)))
      (when (plusp (length base))
        base))))

(defun parse-content-disposition (header)
  "Parse a Content-Disposition header field value (RFC 6266).

   Returns (values disposition-type params) where PARAMS is an alist of
   downcased parameter names to decoded string values. Both `filename` and
   `filename*` may be present; `filename*` is decoded per RFC 8187 via babel."
  (unless (and header (stringp header) (plusp (length header)))
    (return-from parse-content-disposition (values nil nil)))
  (let ((i (%cd-skip-ws header 0)))
    (multiple-value-bind (dtype next) (%cd-parse-token header i)
      (unless dtype
        (return-from parse-content-disposition (values nil nil)))
      (setf i (%cd-skip-ws header next))
      (let ((params '()))
        (loop
          (when (>= i (length header))
            (return))
          (unless (char= (char header i) #\;)
            (return))
          (setf i (%cd-skip-ws header (1+ i)))
          (multiple-value-bind (name ni) (%cd-parse-token header i)
            (unless name (return))
            (setf i (%cd-skip-ws header ni))
            (unless (and (< i (length header)) (char= (char header i) #\=))
              (return))
            (setf i (1+ i))
            (let ((key (string-downcase name)))
              (if (and (plusp (length name))
                       (char= (char name (1- (length name))) #\*))
                  (multiple-value-bind (raw ri) (%cd-parse-ext-value-token header i)
                    (unless raw (return))
                    (setf i (%cd-skip-ws header ri))
                    (let ((decoded (%cd-decode-ext-value raw)))
                      (when decoded
                        (setf params (acons key decoded params)))))
                  (multiple-value-bind (val vi) (%cd-parse-value header i)
                    (unless val (return))
                    (setf i (%cd-skip-ws header vi))
                    (setf params (acons key val params)))))))
        (values (string-downcase dtype) (nreverse params))))))

(defun content-disposition-filename (header)
  "Suggested filename from Content-Disposition (RFC 6266 §4.3).

   Prefers `filename*` over `filename` when both are present. Returns a
   sanitized basename (last path segment) or NIL."
  (multiple-value-bind (dtype params)
      (parse-content-disposition header)
    (declare (ignore dtype))
    (%cd-sanitize-filename
     (or (cdr (assoc "filename*" params :test #'string=))
         (cdr (assoc "filename" params :test #'string=))))))
