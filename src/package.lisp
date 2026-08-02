(defpackage #:http-protocol
  (:use #:cl)
  (:export #:http-error
           #:http-protocol-error
           #:unsupported-content-coding
           #:unsupported-content-coding-coding
           #:unsupported-operation
           #:unsupported-operation-operation
           #:http-connection-error
           #:http-timeout-error
           #:http-tls-error
           #:http-redirect-error
           #:http-canceled
           #:http-status-error
           #:http-status-error-response
           #:http-status-error-status
           #:http-client-error
           #:http-server-error
           ;; Content-Encoding
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
           #:*content-coding-systems*
           ;; Client protocol types
           #:http-backend
           #:http-backend-p
           #:http-client
           #:http-client-p
           #:http-client-backend
           #:http-client-base-url
           #:http-client-headers
           #:http-client-timeout
           #:http-client-max-redirects
           #:http-client-proxy
           #:http-client-verify
           #:http-request
           #:http-request-p
           #:make-http-request
           #:http-request-method
           #:http-request-url
           #:http-request-headers
           #:http-request-content
           #:http-request-params
           #:http-request-timeout
           #:http-request-max-redirects
           #:http-request-accept-encoding
           #:http-request-content-encoding
           #:http-request-decompress
           #:http-request-force-binary
           #:http-request-want-stream
           #:http-request-raise-for-status
           #:http-response
           #:http-response-p
           #:response-status
           #:response-headers
           #:response-body
           #:response-url
           #:response-http-version
           #:response-request
           #:response-header
           #:backend-name
           #:*http-backend*
           #:*http-client*
           #:with-http-backend
           #:with-http-client
           #:make-http-client
           #:send
           #:send-async
           #:cancel-request
           #:raise-for-status))

(defpackage #:http
  (:use #:cl #:http-protocol)
  (:shadow #:get #:delete)
  (:export #:request
           #:request-async
           #:get
           #:get-async
           #:head
           #:head-async
           #:options
           #:options-async
           #:post
           #:post-async
           #:put
           #:put-async
           #:patch
           #:patch-async
           #:delete
           #:delete-async
           #:with-client
           ;; Re-export common response accessors for DX
           #:response-status
           #:response-headers
           #:response-body
           #:response-header
           #:response-url
           #:*http-backend*
           #:*http-client*))
