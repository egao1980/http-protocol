(in-package #:http-protocol/tests)

(deftest make-client-default-cookie-jar
  (let* ((backend (make-instance 'http-backend :name "x"))
         (client (make-http-client backend)))
    (ok (typep (http-client-cookie-jar client) 'cl-cookie:cookie-jar))
    (ok (null (cl-cookie:cookie-jar-cookies (http-client-cookie-jar client))))))

(deftest resolve-merges-alist-cookies
  (let* ((backend (make-instance 'http-backend :name "x"))
         (client (make-http-client backend))
         (req (make-http-request :url "http://example.com/a"
                                 :cookies '(("sid" . "abc") ("x" . "1")))))
    (let ((jar (resolve-cookie-jar client req)))
      (ok (eq jar (http-client-cookie-jar client)))
      (ok (= 2 (length (cl-cookie:cookie-jar-cookies jar))))
      (ok (search "sid=abc" (cookie-header-value jar "http://example.com/a"))))))

(deftest merge-set-cookie-into-jar
  (let ((jar (cl-cookie:make-cookie-jar))
        (ht (let ((h (make-hash-table :test #'equal)))
              (setf (gethash "set-cookie" h) "session=xyz; Path=/")
              h)))
    (let ((new (merge-response-cookies jar "http://example.com/" ht)))
      (ok (= 1 (length new)))
      (ok (string= "session" (cl-cookie:cookie-name (first new))))
      (ok (string= "xyz" (cl-cookie:cookie-value (first new))))
      (ok (search "session=xyz"
                  (cookie-header-value jar "http://example.com/x"))))))

(deftest inject-cookie-header-replaces
  (let* ((jar (cl-cookie:make-cookie-jar))
         (_ (cl-cookie:merge-cookies
             jar
             (list (cl-cookie:make-cookie :name "a" :value "1"
                                          :origin-host "example.com"
                                          :path "/"
                                          :sanity-check nil))))
         (headers (inject-cookie-header
                   '(("cookie" . "stale=1") ("accept" . "*/*"))
                   jar "http://example.com/")))
    (declare (ignore _))
    (ok (string= "a=1" (cdr (assoc "cookie" headers :test #'string-equal))))
    (ok (string= "*/*" (cdr (assoc "accept" headers :test #'string-equal))))))
