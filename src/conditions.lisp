(in-package #:http-protocol)

(define-condition http-error (error)
  ((message :initarg :message :reader http-error-message :initform nil))
  (:report (lambda (c s)
             (format s "~@[~A~]" (http-error-message c)))))

(define-condition http-protocol-error (http-error) ())

(define-condition unsupported-content-coding (http-protocol-error)
  ((coding :initarg :coding :reader unsupported-content-coding-coding))
  (:report (lambda (c s)
             (format s "Unsupported Content-Encoding: ~S~@[ — ~A~]"
                     (unsupported-content-coding-coding c)
                     (http-error-message c)))))

(define-condition unsupported-operation (http-protocol-error)
  ((operation :initarg :operation :reader unsupported-operation-operation :initform nil))
  (:report (lambda (c s)
             (format s "Unsupported HTTP operation~@[ ~S~]~@[ — ~A~]"
                     (unsupported-operation-operation c)
                     (http-error-message c)))))

(define-condition http-version-not-available (unsupported-operation)
  ((requested :initarg :requested :reader http-version-not-available-requested
              :initform :http/2)
   (negotiated :initarg :negotiated :reader http-version-not-available-negotiated
               :initform nil))
  (:default-initargs :operation 'http-version)
  (:report (lambda (c s)
             (format s "HTTP version ~S not available~@[ (negotiated ~S)~]~@[ — ~A~]"
                     (http-version-not-available-requested c)
                     (http-version-not-available-negotiated c)
                     (http-error-message c)))))

(define-condition proxy-tls-unsupported (unsupported-operation)
  ((proxy :initarg :proxy :initform nil :reader proxy-tls-unsupported-proxy
          :documentation "The https:// proxy URL, without userinfo
           (REDACT-PROXY-URL). The report redacts it again.")
   (backend :initarg :backend :initform nil :reader proxy-tls-unsupported-backend
            :documentation "Name of the backend that refused, or NIL."))
  (:default-initargs :operation :https-proxy)
  (:documentation
   "The backend cannot speak TLS to an https:// proxy (PROXY-KIND :HTTPS), so
    it refuses the request instead of sending the request line and
    Proxy-Authorization to the proxy in cleartext (#53). Signalled before any
    octet goes to the proxy. Backends establish USE-VALUE (another proxy URL,
    or NIL for a direct connection) where they can take one.")
  (:report (lambda (c s)
             (format s "~@[~A: ~]cannot speak TLS to the https:// proxy~@[ ~A~]; ~
                        refusing to send the request and its proxy credentials ~
                        in cleartext~@[ — ~A~]"
                     (proxy-tls-unsupported-backend c)
                     (redact-proxy-url (proxy-tls-unsupported-proxy c))
                     (http-error-message c)))))

(define-condition http-connection-error (http-error) ())

(define-condition http-incomplete-response (http-protocol-error http-connection-error)
  ((octets :initarg :octets :initform nil
           :reader http-incomplete-response-octets
           :documentation "Response octets received before the connection
ended (0: none at all), or NIL when the backend does not count them."))
  (:default-initargs :message "incomplete HTTP response")
  (:documentation
   "The connection ended before the response was complete: no status line,
    cut-off headers, or a body shorter than its framing (Content-Length,
    chunked) promised. A transport failure, so HTTP-RETRY retries it, on the
    READ budget, like a dropped connection. It is also an HTTP-PROTOCOL-ERROR,
    which is what backends signalled before it had a type: existing handlers
    still match.")
  (:report (lambda (c s)
             (format s "~A~@[ after ~D response octet~:P~]"
                     (or (http-error-message c) "incomplete HTTP response")
                     (http-incomplete-response-octets c)))))

(define-condition http-timeout-error (http-error) ())
(define-condition http-tls-error (http-error) ())
(define-condition http-redirect-error (http-error) ())
(define-condition http-canceled (http-error) ())

(define-condition http-status-error (http-error)
  ((response :initarg :response :reader http-status-error-response)
   (status :initarg :status :reader http-status-error-status)))

(define-condition http-client-error (http-status-error) ())
(define-condition http-server-error (http-status-error) ())
