(defpackage #:http-protocol
  (:use #:cl)
  (:export #:http-error
           #:http-protocol-error
           #:unsupported-content-coding
           #:unsupported-content-coding-coding
           ;; Content-Encoding protocol (HTTP) — not mime:decode-content (CTE)
           #:decode-content-coding
           #:encode-content-coding
           #:decode-content-codings
           #:encode-content-codings
           #:make-decoding-stream
           #:make-encoding-stream
           #:make-octet-input-stream
           #:slurp-octets
           #:coerce-to-octets
           #:content-coding-supported-p
           #:available-content-codings
           #:default-accept-encoding
           #:parse-content-encoding
           #:normalize-content-coding
           ;; Backend system map (for soft-load / Accept-Encoding)
           #:*content-coding-systems*))
