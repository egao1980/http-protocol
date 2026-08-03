(in-package #:http-protocol/tests)

(defun %call-with-env (bindings thunk)
  "BINDINGS = ((name value)…); VALUE NIL unsets. Restores previous values."
  (let ((saved (mapcar (lambda (b)
                         (list (first b) (uiop:getenv (first b))))
                       bindings)))
    (unwind-protect
         (progn
           (dolist (b bindings)
             (destructuring-bind (name value) b
               (if value
                   (setf (uiop:getenv name) value)
                   #+sbcl (sb-posix:unsetenv name)
                   #-sbcl (setf (uiop:getenv name) ""))))
           (funcall thunk))
      (dolist (b saved)
        (destructuring-bind (name value) b
          (if value
              (setf (uiop:getenv name) value)
              #+sbcl (sb-posix:unsetenv name)
              #-sbcl (setf (uiop:getenv name) "")))))))

(defmacro with-env (bindings &body body)
  `(%call-with-env ',bindings (lambda () ,@body)))

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

(deftest load-proxy-environment-method
  (let ((cfg (make-http-proxy-config :from-environment nil)))
    (with-env (("https_proxy" "http://env-proxy:8080")
               ("HTTPS_PROXY" nil)
               ("http_proxy" nil)
               ("HTTP_PROXY" nil)
               ("all_proxy" nil)
               ("ALL_PROXY" nil)
               ("no_proxy" "localhost")
               ("NO_PROXY" nil))
      (load-proxy-environment cfg)
      (ok (string= "http://env-proxy:8080"
                   (resolve-proxy cfg "https://example.com/")))
      (ok (null (resolve-proxy cfg "http://localhost/"))))))

(deftest programmatic-proxy-not-clobbered-by-environment
  (let ((cfg (make-http-proxy-config :from-environment nil
                                     :proxy "http://explicit:9")))
    (with-env (("http_proxy" "http://env:1")
               ("HTTP_PROXY" nil)
               ("https_proxy" nil)
               ("HTTPS_PROXY" nil)
               ("all_proxy" nil)
               ("ALL_PROXY" nil)
               ("no_proxy" nil)
               ("NO_PROXY" nil))
      (load-proxy cfg :environment t :system nil)
      (ok (string= "http://explicit:9"
                   (resolve-proxy cfg "http://example.com/"))))))

(deftest resolve-proxy-system-automatic
  (let ((cfg (make-http-proxy-config :from-environment nil
                                     :system-automatic-p t)))
    (ok (eq :system (resolve-proxy cfg "http://example.com/")))
    (setf (proxy-config-no-proxy cfg) "example.com")
    (ok (null (resolve-proxy cfg "http://example.com/")))))

(deftest load-proxy-script-requires-fetch
  (let ((cfg (make-http-proxy-config :from-environment nil
                                     :script-url "http://wpad/wpad.dat")))
    (ok (signals (load-proxy-script cfg) 'unsupported-operation))
    (load-proxy-script cfg :fetch (lambda (u)
                                    (ok (string= "http://wpad/wpad.dat" u))
                                    "PAC"))
    (ok (string= "PAC" (proxy-config-script-text cfg)))))
