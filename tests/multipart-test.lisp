(in-package #:http-protocol/tests)

(deftest http-file-roundtrip-multipart
  "Multiple http-file parts streamed into one multipart body."
  (let* ((f1 (make-http-file (make-octet-input-stream (coerce-to-octets "one"))
                             :filename "a.txt"
                             :content-type "text/plain"
                             :content-length 3
                             :field-name "photos"))
         (f2 (make-http-file (make-octet-input-stream (coerce-to-octets "two"))
                             :filename "b.txt"
                             :content-type "text/plain"
                             :content-length 3
                             :field-name "photos"))
         (req (make-http-request :url "http://x"
                                 :form-data '(("title" . "hi"))
                                 :files (list f1 f2))))
    (multiple-value-bind (stream extra clen)
        (prepare-request-body req)
      (ok (streamp stream))
      (ok (search "multipart/form-data"
                  (cdr (assoc "content-type" extra :test #'string-equal))))
      (ok (integerp clen))
      (let* ((octets (slurp-octets stream))
             (text (map 'string #'code-char octets)))
        (ok (= clen (length octets)))
        (ok (search "name=\"title\"" text))
        (ok (search "hi" text))
        (ok (search "name=\"photos\"" text))
        (ok (search "filename=\"a.txt\"" text))
        (ok (search "filename=\"b.txt\"" text))
        (ok (search "one" text))
        (ok (search "two" text))))))

(deftest http-file-alist-files
  (let* ((f (make-http-file (coerce-to-octets "x")
                            :filename "x.bin"
                            :content-length 1))
         (req (make-http-request :url "http://x"
                                 :files `(("upload" . ,f)))))
    (multiple-value-bind (stream extra clen)
        (prepare-request-body req)
      (declare (ignore extra))
      (ok (streamp stream))
      (ok (= clen (length (slurp-octets stream)))))))

(deftest http-file-as-content
  (let* ((f (make-http-file (coerce-to-octets "abc")
                            :content-type "text/plain"
                            :content-length 3))
         (req (make-http-request :url "http://x" :content f)))
    (multiple-value-bind (wire extra clen)
        (prepare-request-body req)
      (ok (equalp (coerce-to-octets "abc") wire))
      (ok (string= "text/plain"
                   (cdr (assoc "content-type" extra :test #'string-equal))))
      (ok (= 3 clen)))))

(deftest response-as-http-file-wraps
  (let* ((ht (make-hash-table :test #'equal))
         (_ (setf (gethash "content-type" ht) "image/png"
                  (gethash "content-length" ht) "4"
                  (gethash "content-disposition" ht)
                  "attachment; filename=\"x.png\""))
         (res (make-instance 'http-response
                             :status 200
                             :headers ht
                             :body (coerce-to-octets "PNG!")))
         (file (response-as-http-file res)))
    (declare (ignore _))
    (ok (http-file-p file))
    (ok (string= "x.png" (http-file-filename file)))
    (ok (string= "image/png" (http-file-content-type file)))
    (ok (= 4 (http-file-content-length file)))
    (ok (equalp (coerce-to-octets "PNG!")
                (slurp-octets (body-stream
                               (make-instance 'http-response
                                              :status 200
                                              :headers ht
                                              :body file)))))))

(deftest prepare-body-stream-content
  (with-open-stream (src (make-octet-input-stream (coerce-to-octets "abc")))
    (let ((req (make-http-request :url "http://x" :content src)))
      (multiple-value-bind (wire extra clen)
          (prepare-request-body req)
        (declare (ignore extra clen))
        (ok (streamp wire))
        (ok (equalp (coerce-to-octets "abc") (slurp-octets wire)))))))

;;; http-protocol#44: name / filename cannot inject; lengths are octets.

(defun %mp-text (req)
  "→ (values decoded-text octet-count content-length)."
  (multiple-value-bind (stream extra clen) (prepare-request-body req)
    (declare (ignore extra))
    (let ((octets (slurp-octets stream)))
      (values (encoding-protocol:decode octets :encoding :utf-8)
              (length octets)
              clen))))

(defun %header-lines (text)
  "Lines of TEXT split on CRLF."
  (loop with start = 0
        for pos = (search (format nil "~C~C" #\Return #\Newline) text :start2 start)
        collect (subseq text start (or pos (length text)))
        while pos
        do (setf start (+ pos 2))))

(deftest multipart-crlf-and-quotes-cannot-inject
  (let* ((evil-name (format nil "f\"; filename=\"x.exe~C~CX-Injected: 1" #\Return #\Newline))
         (evil-file (format nil "a.txt\"~C~CContent-Type: text/html~C~C~C~Cpwned"
                            #\Return #\Newline #\Return #\Newline #\Return #\Newline))
         (req (make-http-request :url "http://x"
                                 :form-data `((,evil-name . "v"))
                                 :files `(("upload" . (:content "data" :filename ,evil-file))))))
    (multiple-value-bind (text n clen) (%mp-text req)
      (let ((lines (%header-lines text)))
        (ok (= n clen) "Content-Length matches the octets")
        (ok (notany (lambda (l) (search "X-Injected" l :end2 (min 10 (length l)))) lines)
            "no forged header line")
        (ok (= 1 (count-if (lambda (l) (string= l "Content-Type: application/octet-stream")) lines))
            "only the real Content-Type line")
        (ok (notany (lambda (l) (string= l "Content-Type: text/html")) lines))
        (ok (notany (lambda (l) (string= l "pwned")) lines) "no forged body")
        (let ((disp (remove-if-not (lambda (l) (search "Content-Disposition:" l)) lines)))
          (ok (= 2 (length disp)) "one disposition per part")
          (ok (search "name=\"f%22; filename=%22x.exe%0D%0AX-Injected: 1\"" (first disp)))
          (ok (search "filename=\"a.txt%22%0D%0AContent-Type: text/html%0D%0A%0D%0Apwned\""
                      (second disp))))))))

(deftest multipart-content-type-crlf-refused
  (let ((req (make-http-request
              :url "http://x"
              :files `(("f" . (:content "x" :filename "a"
                               :content-type ,(format nil "text/plain~C~CX: y"
                                                      #\Return #\Newline)))))))
    (ok (signals (prepare-request-body req) 'http-protocol-error))))

(deftest multipart-non-ascii-filename
  (let* ((name "résumé – 履歴書.pdf")
         (req (make-http-request :url "http://x"
                                 :form-data '(("título" . "ñ"))
                                 :files `(("file" . (:content "%PDF" :filename ,name
                                                     :content-type "application/pdf"))))))
    (multiple-value-bind (text n clen) (%mp-text req)
      (ok (integerp clen))
      (ok (= n clen) "Content-Length counts UTF-8 octets, not characters")
      (ok (> n (length text)) "fixture really is multi-byte")
      (ok (search (format nil "filename=\"~A\"" name) text) "UTF-8 filename (RFC 7578 4.2)")
      (ok (search (format nil "filename*=~A" (mime-protocol:encode-rfc5987-value name)) text)
          "RFC 8187 filename*")
      (ok (search "filename*=UTF-8''r%C3%A9sum%C3%A9%20%E2%80%93%20" text))
      (ok (search "name=\"título\"" text)))))
