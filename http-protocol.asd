(defsystem "http-protocol"
  :version "0.1.0"
  :description "CLOS HTTP client protocol for cl-stack (generics + Content-Encoding + facade)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("trivial-gray-streams" "blackbird" "cl-cookie" "quri" "cl-base64"
               "babel" "bordeaux-threads")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "octet-stream")
               (:file "buffered-stream")
               (:file "content-encoding")
               (:file "content-disposition")
               (:file "types")
               (:file "timeout")
               (:file "retry")
               (:file "proxy")
               (:file "pool")
               (:file "pooled-stream")
               (:file "body")
               (:file "multipart")
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
               (:file "multipart-test")
               (:file "content-disposition-test")
               (:file "facade-test")
               (:file "auth-test")
               (:file "cookies-test")
               (:file "timeout-retry-test")
               (:file "proxy-test")
               (:file "pool-test"))
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
