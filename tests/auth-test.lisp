(in-package #:http-protocol/tests)

(deftest auth-basic-header
  (ok (string= "Basic dXNlcjpwYXNz"
               (authorization-header-value '(:basic "user" "pass")))))

(deftest auth-bearer-header
  (ok (string= "Bearer tok-123"
               (authorization-header-value '(:bearer "tok-123")))))

(deftest auth-digest-unsupported
  (ok (signals (authorization-header-value '(:digest "u" "p"))
               'unsupported-operation)))

(deftest range-header
  (ok (string= "bytes=0-1023" (range-header-value '(0 1023))))
  (ok (string= "bytes=100-" (range-header-value '(100))))
  (ok (string= "bytes=0-1" (range-header-value "bytes=0-1"))))

(deftest inject-auth-range
  (let ((h (inject-auth-range-headers nil
                                      :auth '(:bearer "x")
                                      :range '(0 9))))
    (ok (string= "Bearer x"
                 (cdr (assoc "authorization" h :test #'string-equal))))
    (ok (string= "bytes=0-9"
                 (cdr (assoc "range" h :test #'string-equal)))))
  ;; Existing Authorization wins
  (let ((h (inject-auth-range-headers
            '(("authorization" . "keep"))
            :auth '(:bearer "x"))))
    (ok (string= "keep"
                 (cdr (assoc "authorization" h :test #'string-equal))))))

(deftest effective-auth-prefers-request
  (let* ((backend (make-instance 'mock-backend))
         (client (make-http-client backend :auth '(:bearer "client")))
         (req (make-http-request :url "http://x" :auth '(:bearer "req"))))
    (ok (equal '(:bearer "req") (effective-auth client req)))
    (ok (equal '(:bearer "client")
               (effective-auth client (make-http-request :url "http://x"))))))
