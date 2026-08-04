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

(defun %proper-list-p (x)
  (and (listp x) (null (cdr (last x)))))

(defun %multi-form-values-p (value)
  "T when VALUE is a proper list of scalar form values (requests/httpx multi-value)."
  (and (consp value)
       (%proper-list-p value)
       (every (lambda (v)
                (or (null v)
                    (stringp v)
                    (numberp v)
                    (characterp v)
                    (symbolp v)
                    (typep v '(vector (unsigned-byte 8)))))
              value)))

(defun normalize-form-alist (alist)
  "Alist ((name . value)…) → string/number/octets pairs for quri.

   requests/httpx parity:
   - NIL values are omitted (Python `None` dropped from query/form).
   - A proper list of scalars expands to repeated keys
     (`key2: [v2, v3]` → `key2=v2&key2=v3`)."
  (loop for (name . value) in alist
        for n = (%form-field-name name)
        nconc (cond
                ((null value) nil)
                ((%multi-form-values-p value)
                 (loop for v in value
                       unless (null v)
                       collect (cons n (%form-field-value v))))
                (t (list (cons n (%form-field-value value)))))))

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

(defun apply-client-base-url! (client request)
  "If REQUEST URL is scheme-less, merge against CLIENT's :base-url (httpx shape).
   Destructive. No-op when base is NIL or URL already absolute. Returns URL."
  (check-type request http-request)
  (let ((base (and client (http-client-base-url client)))
        (url (http-request-url request)))
    (when (and base url (plusp (length (string url))))
      (let ((u (quri:uri url)))
        (unless (quri:uri-scheme u)
          (setf (http-request-url request)
                (quri:render-uri (quri:merge-uris u (quri:uri base))))))))
  (http-request-url request))

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
