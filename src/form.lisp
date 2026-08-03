(in-package #:http-protocol)

;;; HTML-form interop (not HTTP core RFC): query :params + urlencoded :form-data.
;;; Typed :data (de)serdes lives in serdes.lisp; multipart in multipart.lisp.

(defun %form-field-name (name)
  (ctypecase name
    (string name)
    (symbol (string-downcase (symbol-name name)))
    (character (string name))))

(defun %form-field-value (value)
  "Coerce a form/query value for quri:url-encode-params."
  (ctypecase value
    (null nil)
    (string value)
    ((vector (unsigned-byte 8)) value)
    (number value)
    (symbol (string-downcase (symbol-name value)))
    (character (string value))
    (t (princ-to-string value))))

(defun normalize-form-alist (alist)
  "Alist ((name . value)…) → string/number/octets pairs for quri."
  (mapcar (lambda (pair)
            (cons (%form-field-name (car pair))
                  (%form-field-value (cdr pair))))
          alist))

(defun encode-urlencoded (data &key (space-to-plus t))
  "Encode DATA alist as application/x-www-form-urlencoded octets (WHATWG: + for space)."
  (babel:string-to-octets
   (quri:url-encode-params (normalize-form-alist data)
                           :space-to-plus space-to-plus)
   :encoding :utf-8))

(defun apply-request-params (url params)
  "Merge PARAMS alist into URL query via quri. Existing query keys are kept; PARAMS append.
   Returns a rendered URI string. PARAMS NIL → URL unchanged."
  (if (null params)
      url
      (let* ((uri (quri:uri url))
             (existing (or (ignore-errors (quri:uri-query-params uri)) '()))
             (extra (normalize-form-alist params)))
        (setf (quri:uri-query-params uri) (append existing extra))
        (quri:render-uri uri))))

(defun finalize-request-url! (request)
  "Apply REQUEST's :params into its URL (destructive). Clears params after merge
   so repeated SEND is idempotent. Returns the effective URL string."
  (check-type request http-request)
  (let ((params (http-request-params request)))
    (when params
      (setf (http-request-url request)
            (apply-request-params (http-request-url request) params)
            (http-request-params request) nil)))
  (http-request-url request))
