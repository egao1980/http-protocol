(in-package #:http-protocol/tests)

(deftest encode-urlencoded-basic
  (let ((octets (encode-urlencoded '(("a" . "1") ("b" . "x y")))))
    (ok (equalp (coerce-to-octets "a=1&b=x+y") octets))))

(deftest encode-urlencoded-symbol-keys
  (ok (equalp (coerce-to-octets "foo=bar")
              (encode-urlencoded '((:foo . "bar"))))))

(deftest prepare-body-urlencoded-form-data
  (let ((req (make-http-request :url "http://x" :form-data '(("q" . "hi there")))))
    (multiple-value-bind (wire extra clen)
        (prepare-request-body req)
      (ok (equalp (coerce-to-octets "q=hi+there") wire))
      (ok (string-equal "application/x-www-form-urlencoded"
                        (cdr (assoc "content-type" extra :test #'string-equal))))
      (ok (= clen (length wire))))))

(deftest prepare-body-multipart-when-files
  " :form-data + :files still multipart (not urlencoded)."
  (let* ((f (make-http-file (coerce-to-octets "x")
                            :filename "x.bin"
                            :content-length 1))
         (req (make-http-request :url "http://x"
                                 :form-data '(("title" . "hi"))
                                 :files `(("upload" . ,f)))))
    (multiple-value-bind (stream extra clen)
        (prepare-request-body req)
      (declare (ignore clen))
      (ok (streamp stream))
      (ok (search "multipart/form-data"
                  (cdr (assoc "content-type" extra :test #'string-equal)))))))

(deftest apply-params-merges-query
  (ok (string= "http://ex.com/p?keep=1&q=hi%20there"
               (apply-request-params "http://ex.com/p?keep=1"
                                     '(("q" . "hi there")))))
  (ok (string= "http://ex.com/"
               (apply-request-params "http://ex.com/" nil))))

(deftest finalize-request-url-consumes-params
  (let ((req (make-http-request :url "http://ex.com/search"
                                :params '(("q" . "a b") ("page" . 2)))))
    (ok (string= "http://ex.com/search?q=a%20b&page=2"
                 (finalize-request-url! req)))
    (ok (null (http-request-params req)))
    (ok (string= "http://ex.com/search?q=a%20b&page=2"
                 (finalize-request-url! req)))))

(defclass %form-probe-backend (http-backend)
  ((last-url :initform nil :accessor %probe-url))
  (:default-initargs :name "form-probe"))

(defmethod send ((backend %form-probe-backend) client request &key)
  (declare (ignore client))
  (setf (%probe-url backend) (http-request-url request))
  (make-instance 'http-response :status 200 :headers (make-hash-table) :body #()))

(deftest send-before-applies-params
  (let* ((backend (make-instance '%form-probe-backend))
         (client (make-http-client backend))
         (req (make-http-request :url "http://ex.com/x"
                                 :params '(("a" . "1")))))
    (send backend client req)
    (ok (string= "http://ex.com/x?a=1" (%probe-url backend)))
    (ok (null (http-request-params req)))))
