(in-package #:http-protocol/tests)

(deftest encode-data-text
  (multiple-value-bind (wire ct clen)
      (encode-http-data "hi" :text nil)
    (ok (equalp (coerce-to-octets "hi") wire))
    (ok (search "text/plain" ct))
    (ok (= 2 clen))))

(deftest encode-data-auto-string
  (multiple-value-bind (wire ct clen)
      (encode-http-data "x" :auto nil)
    (declare (ignore clen))
    (ok (equalp (coerce-to-octets "x") wire))
    (ok (search "text/plain" ct))))

(deftest prepare-body-typed-data
  (let ((req (make-http-request :url "http://x" :data "payload" :data-type :text)))
    (multiple-value-bind (wire extra clen)
        (prepare-request-body req)
      (ok (equalp (coerce-to-octets "payload") wire))
      (ok (search "text/plain"
                  (cdr (assoc "content-type" extra :test #'string-equal))))
      (ok (= 7 clen)))))

(deftest prepare-body-rejects-content-and-data
  (ok (signals
          (prepare-request-body
           (make-http-request :url "http://x" :content "a" :data "b"))
          'http-protocol-error)))

(deftest decode-urlencoded-response-data
  (let* ((ht (make-hash-table :test #'equal))
         (_ (setf (gethash "content-type" ht)
                  "application/x-www-form-urlencoded"))
         (res (make-instance 'http-response
                             :status 200
                             :headers ht
                             :body (coerce-to-octets "a=1&b=x+y"))))
    (declare (ignore _))
    (let ((data (response-data res)))
      (ok (equal "1" (cdr (assoc "a" data :test #'string=))))
      (ok (equal "x y" (cdr (assoc "b" data :test #'string=)))))))

(deftest with-data-deserializer-json
  (let* ((ht (make-hash-table :test #'equal))
         (_ (setf (gethash "content-type" ht) "application/json"))
         (res (make-instance 'http-response
                             :status 200
                             :headers ht
                             :body (coerce-to-octets "{\"ok\":true}")))
         (seen nil))
    (declare (ignore _))
    (with-data-deserializer (:json (lambda (octets)
                                     (setf seen octets)
                                     :parsed))
      (ok (eq :parsed (response-data res :json))))
    (ok (equalp (coerce-to-octets "{\"ok\":true}") seen))
    (ok (signals (response-data res :json) 'unsupported-operation))))

(deftest with-data-serializer-json
  (with-data-serializer (:json (lambda (data)
                                 (declare (ignore data))
                                 "{\"n\":1}"))
    (multiple-value-bind (wire ct clen)
        (encode-http-data '(:n 1) :json nil)
      (ok (equalp (coerce-to-octets "{\"n\":1}") wire))
      (ok (string-equal "application/json" ct))
      (ok (= clen (length wire)))))
  (ok (signals (encode-http-data '(:n 1) :json nil) 'unsupported-operation)))

(deftest with-data-codec-roundtrip
  (with-data-codec (:json
                    :encoder (lambda (d) (format nil "~A" d))
                    :decoder (lambda (o) (babel:octets-to-string o)))
    (multiple-value-bind (wire ct clen)
        (encode-http-data 'hello :json nil)
      (declare (ignore ct clen))
      (ok (string= "HELLO" (string-upcase
                            (decode-http-data wire :json "application/json")))))))

(deftest with-data-deserializer-custom-type
  (with-data-deserializer (:csv (lambda (o)
                                  (babel:octets-to-string o)))
    (ok (string= "a,b"
                 (decode-http-data (coerce-to-octets "a,b") :csv nil)))))
