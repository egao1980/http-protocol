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
