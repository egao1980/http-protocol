(defpackage #:http-protocol/tests
  (:use #:cl #:rove #:http-protocol #:http)
  (:shadowing-import-from #:http #:get #:delete #:trace #:stream))
