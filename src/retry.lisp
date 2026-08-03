(in-package #:http-protocol)

;;; Retry / backoff policy (urllib3 Retry shape). Protocol-level CLOS;
;;; backends consult RETRY-SHOULD-RETRY-P / RETRY-DELAY-SECONDS between attempts.

(defclass http-retry ()
  ((total :initarg :total :accessor retry-total :initform 3
          :documentation "Max retries after the first attempt (0 = no retry).")
   (connect :initarg :connect :accessor retry-connect :initform nil
            :documentation "Max connect-failure retries (NIL → TOTAL).")
   (read :initarg :read :accessor retry-read :initform nil
         :documentation "Max read/timeout retries (NIL → TOTAL).")
   (status :initarg :status :accessor retry-status
           :initform '(429 500 502 503 504)
           :documentation "Retry these response statuses (list) or NIL to disable.")
   (allowed-methods :initarg :allowed-methods :accessor retry-allowed-methods
                    :initform '(:head :get :put :delete :options :trace)
                    :documentation "Methods safe/idempotent enough to retry.")
   (backoff-factor :initarg :backoff-factor :accessor retry-backoff-factor
                   :initform 0.5
                   :documentation "{factor} * (2 ** (attempt - 1)) seconds.")
   (backoff-max :initarg :backoff-max :accessor retry-backoff-max :initform 120.0)
   (respect-retry-after :initarg :respect-retry-after
                        :accessor retry-respect-retry-after :initform t))
  (:documentation "Retry policy. NIL / 0 / integer coerce via COERCE-RETRY."))

(defun http-retry-p (x) (typep x 'http-retry))

(defun make-http-retry (&rest args &key &allow-other-keys)
  (apply #'make-instance 'http-retry args))

(defun coerce-retry (x)
  "NIL → no retries; integer N → TOTAL N; HTTP-RETRY → itself; T → defaults."
  (cond
    ((http-retry-p x) x)
    ((null x) (make-http-retry :total 0))
    ((eq x t) (make-http-retry))
    ((and (integerp x) (>= x 0)) (make-http-retry :total x))
    (t (error 'http-protocol-error
              :message (format nil "cannot coerce retry: ~S" x)))))

(defun effective-retry (request client)
  (coerce-retry (or (and request (http-request-retry request))
                    (and client (http-client-retry client))
                    0)))

(defun %normalize-method (method)
  (intern (string-upcase (string method)) :keyword))

(defun %retry-after-seconds (response)
  (let* ((raw (and response (response-header response :retry-after)))
         (n (and raw (ignore-errors (parse-integer raw :junk-allowed t)))))
    (and n (max 0 n))))

(defgeneric retry-should-retry-p (retry attempt method &key status condition response)
  (:documentation
   "True if another attempt should be made after ATTEMPT (1-based completed tries).")
  (:method ((retry http-retry) attempt method &key status condition response)
    (declare (ignore response))
    (let* ((method (%normalize-method method))
           (max-total (retry-total retry)))
      (cond
        ((zerop max-total) nil)
        ((>= (1- attempt) max-total) nil)
        ((not (member method (retry-allowed-methods retry) :test #'eq)) nil)
        (condition
         (typecase condition
           (http-timeout-error
            (< (1- attempt) (or (retry-read retry) max-total)))
           (http-connection-error
            (< (1- attempt) (or (retry-connect retry) max-total)))
           (http-tls-error
            (< (1- attempt) (or (retry-connect retry) max-total)))
           (t nil)))
        ((and status (retry-status retry)
              (member status (retry-status retry) :test #'eql))
         t)
        (t nil)))))

(defgeneric retry-delay-seconds (retry attempt &key response)
  (:documentation "Seconds to sleep before the next attempt (ATTEMPT is 1-based).")
  (:method ((retry http-retry) attempt &key response)
    (let* ((after (and (retry-respect-retry-after retry)
                       (%retry-after-seconds response)))
           (factor (retry-backoff-factor retry))
           (expo (max 0 (1- attempt)))
           (backoff (min (retry-backoff-max retry)
                         (* factor (expt 2 expo)))))
      (float (or after backoff) 1.0d0))))
