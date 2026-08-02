(defsystem "http-protocol"
  :version "0.1.0"
  :description "CLOS HTTP client protocol for cl-stack (Content-Encoding first)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("chipz" "salza2")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "content-encoding"))
  :in-order-to ((test-op (test-op "http-protocol/tests"))))

(defsystem "http-protocol/tests"
  :depends-on ("http-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "content-encoding-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
