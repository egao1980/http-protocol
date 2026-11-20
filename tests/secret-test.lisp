(in-package #:http-protocol/tests)

;;; Secret header values never print (issue http-backend-async#54).

(defparameter +token+ "sk-test-0123456789-very-secret")

(defun %token-free-p (string)
  (not (search +token+ string)))

(defun %printed (x)
  (format nil "~A ~S ~@[~A~]" x x (with-output-to-string (s) (describe x s))))

(deftest secret-prints-redacted
  (let ((s (make-secret +token+)))
    (ok (secret-p s))
    (ok (string= +token+ (reveal s)))
    (ok (string= "plain" (reveal "plain")))
    (ok (eq s (make-secret s)))
    (ok (null (make-secret nil)))
    (ok (%token-free-p (%printed s)))
    (ok (%token-free-p (prin1-to-string (list :headers (list (cons "authorization" s))))))))

(deftest authorization-header-value-keeps-secret
  (let ((v (authorization-header-value (list :bearer (make-secret +token+)))))
    (ok (secret-p v))
    (ok (string= (concatenate 'string "Bearer " +token+) (reveal v)))
    (ok (%token-free-p (prin1-to-string v))))
  (let ((v (authorization-header-value (list :basic "u" (make-secret +token+)))))
    (ok (secret-p v))
    (ok (string= "Basic " (subseq (reveal v) 0 6))))
  ;; plain in → plain out (existing callers unchanged)
  (ok (stringp (authorization-header-value '(:bearer "x")))))

(deftest protect-and-redact-headers
  (let* ((alist (list (cons "Authorization" +token+) (cons "accept" "*/*")))
         (prot (protect-headers alist)))
    (ok (secret-p (cdr (assoc "authorization" prot :test #'string-equal))))
    (ok (string= "*/*" (cdr (assoc "accept" prot :test #'string-equal))))
    (ok (%token-free-p (prin1-to-string prot)))
    (ok (equal alist (reveal-headers prot)))
    (ok (%token-free-p (prin1-to-string (redact-headers alist))))
    (ok (string= "*/*" (cdr (assoc "accept" (redact-headers alist) :test #'string-equal)))))
  (let ((plist (list :x-api-key +token+ :accept "a")))
    (ok (%token-free-p (prin1-to-string (protect-headers plist))))
    (ok (%token-free-p (prin1-to-string (redact-headers plist))))
    (ok (equal "a" (getf (redact-headers plist) :accept))))
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "cookie" ht) +token+)
    (ok (string= "[redacted]" (gethash "cookie" (redact-headers ht))))))

(deftest request-and-client-print-without-headers
  (let* ((req (make-http-request :url "http://h/x"
                                 :headers (list (cons "authorization" +token+))
                                 :auth (list :bearer +token+)))
         (client (make-http-client (make-instance 'mock-backend)
                                   :auth (list :bearer +token+))))
    (ok (search "http://h/x" (prin1-to-string req)))
    (ok (%token-free-p (prin1-to-string req)))
    (ok (%token-free-p (prin1-to-string client)))))

;;; A backend that keeps SECRETs sees them; any other backend gets them revealed.

(defvar *seen-headers* nil)
(defvar *seen-auth* nil)

(defclass secret-capture-backend (mock-backend) ())
(defclass secret-keeping-backend (mock-backend) ())
(defmethod backend-keeps-secrets-p ((b secret-keeping-backend)) t)

(defmethod send :after ((backend mock-backend) client request &key)
  (declare (ignore client))
  (setf *seen-headers* (http-request-headers request)
        *seen-auth* (http-request-auth request)))

(deftest legacy-backend-gets-plain-values
  (let ((*http-backend* (make-instance 'secret-capture-backend))
        (*http-client* nil))
    (request :get "http://h/"
             :headers (list (cons "authorization" (make-secret +token+)))
             :auth (list :bearer (make-secret +token+)))
    (ok (string= +token+ (cdr (assoc "authorization" *seen-headers*
                                     :test #'string-equal))))
    (ok (equal (list :bearer +token+) *seen-auth*))))

(deftest secret-keeping-backend-gets-secrets
  (let ((*http-backend* (make-instance 'secret-keeping-backend))
        (*http-client* nil))
    (request :get "http://h/"
             :headers (list (cons "authorization" (make-secret +token+))))
    (ok (secret-p (cdr (assoc "authorization" *seen-headers*
                              :test #'string-equal))))))

;;; A failing request: neither the condition nor the backtrace frames show it.

(defclass failing-backend (http-backend) ()
  (:default-initargs :name "failing"))
(defmethod backend-keeps-secrets-p ((b failing-backend)) t)
(defmethod send ((backend failing-backend) client request &key)
  (declare (ignore client))
  (error 'http-connection-error
         :message (format nil "connect failed for ~A" request)))

(deftest failing-request-condition-and-backtrace-are-token-free
  (let ((*http-backend* (make-instance 'failing-backend))
        (*http-client* nil)
        (report nil)
        (bt nil))
    (handler-case
        (handler-bind ((http-error
                         (lambda (c)
                           (setf report (format nil "~A ~S" c c)
                                 bt (with-output-to-string (s)
                                         (uiop:print-backtrace :stream s
                                                               :count 60))))))
          (request :post "http://h/v1/chat"
                   :headers (list (cons "authorization"
                                        (make-secret
                                         (concatenate 'string "Bearer " +token+)))
                                  (cons "content-type" "application/json"))
                   :auth (list :bearer (make-secret +token+))))
      (http-error () nil))
    (ok (stringp report))
    (ok (plusp (length bt)))
    ;; SBCL names the frames; ECL prints a bare trace
    #+sbcl (ok (search "REQUEST" (string-upcase bt)))
    (ok (%token-free-p report))
    (ok (%token-free-p bt))))
