(defsystem "http-protocol"
  :version "0.1.0"
  :description "CLOS HTTP client protocol for cl-stack (generics + Content-Encoding + facade)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("trivial-gray-streams" "blackbird" "cl-cookie" "quri" "cl-base64")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "octet-stream")
               (:file "buffered-stream")
               (:file "content-encoding")
               (:file "types")
               (:file "body")
               (:file "auth")
               (:file "cookies")
               (:file "protocol")
               (:file "facade"))
  :in-order-to ((test-op (test-op "http-protocol/tests"))))

(defsystem "http-protocol/tests"
  :depends-on ("http-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "protocol-test")
               (:file "body-stream-test")
               (:file "facade-test")
               (:file "auth-test")
               (:file "cookies-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))

(defsystem "http-protocol/conformance"
  :description "Content-Encoding backend conformance suite (Rove)"
  :depends-on ("http-protocol" "rove")
  :pathname "tests/conformance"
  :serial t
  :components ((:file "package")
               (:file "suite")))
