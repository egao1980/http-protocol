(defpackage #:http-protocol
  (:use #:cl)
  (:export #:http-error
           #:http-protocol-error
           #:unsupported-content-coding
           #:unsupported-content-coding-coding
           ;; Content-Encoding (HTTP) — not mime:decode-content (CTE)
           #:decode-content-coding
           #:encode-content-coding
           #:decode-content-codings
           #:encode-content-codings
           #:content-coding-supported-p
           #:available-content-codings
           #:default-accept-encoding
           #:parse-content-encoding
           #:normalize-content-coding))
