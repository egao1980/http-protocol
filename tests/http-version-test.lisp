(in-package #:http-protocol/tests)

(deftest normalize-http-version-coerces
  (ok (eq :auto (normalize-http-version nil)))
  (ok (eq :http/1.1 (normalize-http-version "HTTP/1.1")))
  (ok (eq :http/1.1 (normalize-http-version 1.1)))
  (ok (eq :http/2 (normalize-http-version "h2")))
  (ok (eq :http/2 (normalize-http-version :http/2)))
  (ok (signals (normalize-http-version "http/3") 'http-protocol-error)))

(deftest alpn-protocols-for-version-offers
  (ok (equal '("h2" "http/1.1") (alpn-protocols-for-version :auto)))
  (ok (equal '("h2" "http/1.1") (alpn-protocols-for-version :http/2)))
  (ok (equal '("http/1.1") (alpn-protocols-for-version :http/1.1))))

(deftest http-version-from-alpn-maps
  (ok (eq :http/2 (http-version-from-alpn "h2")))
  (ok (eq :http/1.1 (http-version-from-alpn "http/1.1")))
  (ok (null (http-version-from-alpn nil))))

(deftest effective-http-version-inherits
  (let* ((backend (make-instance 'http-backend :name "t"))
         (client (make-http-client backend :http-version :http/2))
         (req (make-http-request :url "https://ex.test/")))
    (ok (eq :http/2 (effective-http-version client req)))
    (setf (http-request-http-version req) :http/1.1)
    (ok (eq :http/1.1 (effective-http-version client req)))))

(deftest ensure-http-version-available-enforces
  (ok (eq :http/2 (ensure-http-version-available :http/2 :http/2)))
  (ok (eq :http/1.1 (ensure-http-version-available :auto :http/1.1)))
  (ok (signals (ensure-http-version-available :http/2 :http/1.1)
               'http-version-not-available)))

(deftest backend-http-versions-default-http11
  (let ((b (make-instance 'http-backend :name "plain")))
    (ok (equal '(:http/1.1) (backend-http-versions b)))
    (ok (backend-supports-http-version-p b :auto))
    (ok (backend-supports-http-version-p b :http/1.1))
    (ok (not (backend-supports-http-version-p b :http/2)))))

(deftest filter-headers-for-http2-strips-connection-specific
  (let* ((in '(("Host" . "ex.test")
               ("Accept" . "*/*")
               ("Connection" . "keep-alive")
               ("Transfer-Encoding" . "chunked")
               ("X-Foo" . "1")))
         (out (filter-headers-for-http-version in :http/2)))
    (ok (equal '(("Accept" . "*/*") ("X-Foo" . "1")) out))
    (ok (equal in (filter-headers-for-http-version in :http/1.1)))))

(deftest filter-headers-for-http2-keeps-te-trailers
  (let* ((in '(("Accept" . "*/*")
               ("TE" . "trailers")
               ("te" . "gzip")))
         (out (filter-headers-for-http-version in :http/2)))
    (ok (equal '(("Accept" . "*/*") ("TE" . "trailers")) out))
    (ok (equal in (filter-headers-for-http-version in :http/1.1)))))

(deftest make-http2-request-headers-rfc9113-order
  (let* ((uri (quri:uri "https://ex.test:8443/a?q=1"))
         (hdrs (make-http2-request-headers
                :get uri '(("Host" . "ex.test:8443")
                           ("Accept" . "text/plain")
                           ("Connection" . "close")))))
    (ok (equal '(:method :scheme :path :authority)
               (mapcar #'car (subseq hdrs 0 4))))
    (ok (equal "GET" (cdr (assoc :method hdrs))))
    (ok (equal "https" (cdr (assoc :scheme hdrs))))
    (ok (equal "/a?q=1" (cdr (assoc :path hdrs))))
    (ok (equal "ex.test:8443" (cdr (assoc :authority hdrs))))
    (ok (equal "text/plain" (cdr (assoc "accept" hdrs :test #'string=))))
    (ok (null (assoc "host" hdrs :test #'string-equal)))
    (ok (null (assoc "connection" hdrs :test #'string-equal)))))

(deftest send-before-rejects-unsupported-version
  (let* ((backend (make-instance 'http-backend :name "h1-only"))
         (client (make-http-client backend :http-version :http/2))
         (req (make-http-request :url "https://ex.test/")))
    (ok (signals (send backend client req) 'http-version-not-available))))
