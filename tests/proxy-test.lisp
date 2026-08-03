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
               :system nil
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
              :system nil
              :proxy "http://proxy:8080"
              :no-proxy "localhost, .corp.example, 10.0.0.0/8, ::1")))
    (ok (null (resolve-proxy cfg "http://localhost/")))
    (ok (null (resolve-proxy cfg "http://api.corp.example/")))
    (ok (null (resolve-proxy cfg "http://10.1.2.3/")))
    (ok (null (resolve-proxy cfg "http://[::1]/")))
    (ok (string= "http://proxy:8080"
                 (resolve-proxy cfg "http://example.com/")))))

(deftest no-proxy-list-of-globs
  "dexador#202 list form + *.domain globs."
  ;; dexador host-matching corpus
  (ok (host-bypassed-p "example.com" "example.com"))
  (ok (host-bypassed-p "api.example.com" ".example.com"))
  (ok (host-bypassed-p "api.example.com" "example.com"))
  (ok (not (host-bypassed-p "notexample.com" "example.com")))
  (ok (host-bypassed-p "anything.net" "*"))
  (ok (host-bypassed-p "example.com" "example.com:443"))
  (ok (host-bypassed-p "a.foo.org" '("bar.com" ".foo.org")))
  (ok (not (host-bypassed-p "a.foo.org" nil)))
  ;; globs
  (ok (host-bypassed-p "api.corp.example" '("*.corp.example" "localhost")))
  (ok (not (host-bypassed-p "corp.example" '("*.corp.example")))
      "*.corp.example needs a label before .corp.example")
  (ok (host-bypassed-p "corp.example" '(".corp.example"))
      "dexador-style suffix still covers the bare domain")
  (ok (not (host-bypassed-p "evilcorp.example" '("*.corp.example"))))
  (ok (host-bypassed-p "x.y.z" '("x.?.z")))
  ;; IP / CIDR (dexador)
  (ok (host-bypassed-p "127.0.0.1" "127.0.0.1"))
  (ok (host-bypassed-p "10.1.2.3" "10.0.0.0/8"))
  (ok (host-bypassed-p "::1" "::1"))
  (ok (host-bypassed-p "[::1]" "::1"))
  (ok (host-bypassed-p "fd12:3456::1" "fd00::/8"))
  (ok (not (host-bypassed-p "2001:db8::1" "fd00::/8")))
  (let ((cfg (make-http-proxy-config
              :system nil
              :proxy "http://proxy:8080"
              :no-proxy '("*.internal" "10.0.0.0/8"))))
    (ok (null (resolve-proxy cfg "http://api.internal/")))
    (ok (null (resolve-proxy cfg "http://10.9.9.9/")))
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

(deftest configure-proxy-manual
  (let ((cfg (make-http-proxy-config :system nil)))
    (configure-proxy cfg :proxy "http://manual:8080" :no-proxy "localhost")
    (ok (string= "http://manual:8080"
                 (resolve-proxy cfg "http://example.com/")))
    (ok (null (resolve-proxy cfg "http://localhost/")))))

(deftest load-proxy-system-includes-environment
  "System discovery includes unix env vars."
  (let ((cfg (make-http-proxy-config :system nil)))
    (with-env (("https_proxy" "http://env-proxy:8080")
               ("HTTPS_PROXY" nil)
               ("http_proxy" nil)
               ("HTTP_PROXY" nil)
               ("all_proxy" nil)
               ("ALL_PROXY" nil)
               ("no_proxy" "localhost")
               ("NO_PROXY" nil))
      (load-proxy-system cfg)
      (ok (string= "http://env-proxy:8080"
                   (resolve-proxy cfg "https://example.com/")))
      (ok (null (resolve-proxy cfg "http://localhost/"))))))

(deftest manual-proxy-not-clobbered-by-system
  (let ((cfg (make-http-proxy-config :system nil)))
    (configure-proxy cfg :proxy "http://explicit:9")
    (with-env (("http_proxy" "http://env:1")
               ("HTTP_PROXY" nil)
               ("https_proxy" nil)
               ("HTTPS_PROXY" nil)
               ("all_proxy" nil)
               ("ALL_PROXY" nil)
               ("no_proxy" nil)
               ("NO_PROXY" nil))
      (load-proxy-system cfg)
      (ok (string= "http://explicit:9"
                   (resolve-proxy cfg "http://example.com/"))))))

(deftest resolve-proxy-system-automatic
  (let ((cfg (make-http-proxy-config :system nil
                                     :system-automatic-p t)))
    (ok (eq :system (resolve-proxy cfg "http://example.com/")))
    (setf (proxy-config-no-proxy cfg) "example.com")
    (ok (null (resolve-proxy cfg "http://example.com/")))))

(deftest socks-proxy-uri-and-kind
  (ok (eq :socks5 (proxy-kind "socks5://127.0.0.1:9050")))
  (ok (eq :socks5 (proxy-kind "socks5h://user:pass@proxy:1080")))
  (ok (socks-remote-dns-p "socks5h"))
  (ok (not (socks-remote-dns-p "socks5")))
  (multiple-value-bind (scheme host port user pass)
      (parse-proxy-uri "socks5h://u:p@127.0.0.1:1080")
    (ok (string= "socks5h" scheme))
    (ok (string= "127.0.0.1" host))
    (ok (= 1080 port))
    (ok (string= "u" user))
    (ok (string= "p" pass)))
  (ok (socks-proxy-scheme-p "socks"))
  (ok (http-proxy-scheme-p "http")))

(deftest configure-proxy-script-manual
  (let ((cfg (make-http-proxy-config :system nil)))
    (ok (signals (configure-proxy-script cfg :url "http://wpad/wpad.dat")
                 'unsupported-operation))
    (configure-proxy-script cfg :url "http://pac.example/x.pac"
                            :fetch (lambda (u)
                                     (ok (string= "http://pac.example/x.pac" u))
                                     "PAC"))
    (ok (string= "PAC" (proxy-config-script-text cfg)))
    (ok (null (proxy-config-proxy cfg)))))
