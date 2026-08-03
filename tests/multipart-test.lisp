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
                                 :data '(("title" . "hi"))
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
