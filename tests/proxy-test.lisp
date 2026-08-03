(in-package #:http-protocol/tests)

(deftest resolve-proxy-alist-specificity
  (let* ((cfg (make-http-proxy-config
               :from-environment nil
               :proxy '(("https://internal.git" . "http://10.0.0.1:3128/")
                        ("https" . "http://https-proxy:8080/")
                        ("*" . "http://all-proxy:8080/"))
               :no-proxy nil)))
    (ok (string= "http://10.0.0.1:3128/"
                 (resolve-proxy cfg "https://internal.git/x")))
    (ok (string= "http://https-proxy:8080/"
                 (resolve-proxy cfg "https://example.com/")))
    (ok (string= "http://all-proxy:8080/"
                 (resolve-proxy cfg "http://example.com/")))))

(deftest no-proxy-hostname-and-cidr
  (let ((cfg (make-http-proxy-config
              :from-environment nil
              :proxy "http://proxy:8080"
              :no-proxy "localhost, .corp.example, 10.0.0.0/8, ::1")))
    (ok (null (resolve-proxy cfg "http://localhost/")))
    (ok (null (resolve-proxy cfg "http://api.corp.example/")))
    (ok (null (resolve-proxy cfg "http://10.1.2.3/")))
    (ok (null (resolve-proxy cfg "http://[::1]/")))
    (ok (string= "http://proxy:8080"
                 (resolve-proxy cfg "http://example.com/")))))

(deftest strip-ipv6-and-format-host-port
  (ok (string= "::1" (strip-ipv6-brackets "[::1]")))
  (ok (string= "[::1]:443" (format-host-port "::1" 443)))
  (ok (string= "example.com:80" (format-host-port "example.com" 80))))

(deftest parse-proxy-uri-credentials
  (multiple-value-bind (scheme host port user pass)
      (parse-proxy-uri "http://user:secret@proxy.example:3128")
    (ok (string-equal "http" scheme))
    (ok (string= "proxy.example" host))
    (ok (= 3128 port))
    (ok (string= "user" user))
    (ok (string= "secret" pass))))

(deftest coerce-proxy-string
  (ok (string= "http://p"
               (resolve-proxy (coerce-proxy-config "http://p")
                              "https://example.com"))))
