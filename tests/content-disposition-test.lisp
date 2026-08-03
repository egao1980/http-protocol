(in-package #:http-protocol/tests)

;;; RFC 6266 examples + RFC 8187 ext-value

(deftest cd-filename-token
  "RFC 6266 §4.3 example: Attachment; filename=example.html"
  (ok (string= "example.html"
               (content-disposition-filename
                "Attachment; filename=example.html"))))

(deftest cd-filename-quoted-with-space
  "RFC 6266: INLINE; FILENAME= \"an example.html\""
  (ok (string= "an example.html"
               (content-disposition-filename
                "INLINE; FILENAME= \"an example.html\""))))

(deftest cd-filename-star-utf8
  "RFC 6266 / 8187: filename*= UTF-8''%e2%82%ac%20rates → € rates"
  (ok (string= (format nil "~C rates" (code-char #x20AC))
               (content-disposition-filename
                "attachment; filename*= UTF-8''%e2%82%ac%20rates"))))

(deftest cd-prefer-filename-star
  "RFC 6266 §4.3: when both present, prefer filename*"
  (ok (string= (format nil "~C rates" (code-char #x20AC))
               (content-disposition-filename
                "attachment; filename=\"EURO rates\"; filename*=utf-8''%e2%82%ac%20rates"))))

(deftest cd-filename-star-before-filename
  "Still prefer filename* when it appears first"
  (ok (string= (format nil "~C rates" (code-char #x20AC))
               (content-disposition-filename
                "attachment; filename*=utf-8''%e2%82%ac%20rates; filename=\"EURO rates\""))))

(deftest cd-sanitize-path-segments
  "RFC 6266 §4.3: strip all but last path segment"
  (ok (string= "passwd"
               (content-disposition-filename
                "attachment; filename=\"../../etc/passwd\"")))
  ;; Backslashes via filename* (quoted-string \ is quoted-pair per RFC 9110)
  (ok (string= "x.bin"
               (content-disposition-filename
                "attachment; filename*=UTF-8''C%3A%5Cfoo%5Cx.bin"))))

(deftest cd-quoted-pair-escape
  (ok (string= "a\"b"
               (content-disposition-filename
                "attachment; filename=\"a\\\"b\""))))

(deftest cd-iso-8859-1-via-babel
  "RFC 8187 mime-charset decoded with babel (not UTF-8-only)."
  (ok (string= (map 'string #'code-char '(99 97 102 #xe9))
               (content-disposition-filename
                "attachment; filename*=ISO-8859-1''%63%61%66%E9"))))

(deftest cd-unknown-charset-falls-back
  "RFC 8187: unsupported charset → ignore filename*; use filename="
  (ok (string= "fallback.txt"
               (content-disposition-filename
                "attachment; filename=\"fallback.txt\"; filename*=X-UNKNOWN-CS''foo"))))

(deftest parse-content-disposition-type
  (multiple-value-bind (type params)
      (parse-content-disposition "attachment; filename=\"x.png\"")
    (ok (string= "attachment" type))
    (ok (string= "x.png" (cdr (assoc "filename" params :test #'string=))))))
