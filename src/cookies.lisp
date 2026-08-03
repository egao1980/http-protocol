(in-package #:http-protocol)

;;; Cookie-jar helpers over cl-cookie (requests Session persistence shape).

(defun ensure-cookie-jar (jar)
  "Return JAR or a fresh empty cookie-jar when JAR is NIL."
  (or jar (cl-cookie:make-cookie-jar)))

(defun %alist-cookies (alist origin-host origin-path)
  "Turn ((name . value) ...) into cl-cookie:cookie list for ORIGIN-HOST/PATH."
  (loop for pair in alist
        for name = (string (if (consp pair) (car pair) pair))
        for value = (if (consp pair)
                        (if (stringp (cdr pair))
                            (cdr pair)
                            (princ-to-string (cdr pair)))
                        "")
        collect (cl-cookie:make-cookie
                 :name name
                 :value value
                 :origin-host origin-host
                 :path (or origin-path "/")
                 :sanity-check nil)))

(defun resolve-cookie-jar (client request &key url)
  "Cookie jar for this exchange.

   - REQUEST :cookies is a cookie-jar → use it (and merge alist extras? no).
   - REQUEST :cookies is an alist → merge into CLIENT jar (requests-style).
   - else → CLIENT jar (created if missing).

   Side-effect: may set CLIENT's jar slot when it was NIL."
  (let* ((req-cookies (http-request-cookies request))
         (jar (cond
                ((typep req-cookies 'cl-cookie:cookie-jar) req-cookies)
                (t
                 (let ((j (ensure-cookie-jar (http-client-cookie-jar client))))
                   (unless (http-client-cookie-jar client)
                     (setf (http-client-cookie-jar client) j))
                   j)))))
    (when (and (listp req-cookies) (not (null req-cookies))
               (not (typep req-cookies 'cl-cookie:cookie-jar)))
      (let* ((uri (quri:uri (or url (http-request-url request))))
             (host (or (quri:uri-host uri) ""))
             (path (or (quri:uri-path uri) "/")))
        (cl-cookie:merge-cookies jar (%alist-cookies req-cookies host path))))
    jar))

(defun cookie-header-value (jar url)
  "Cookie request-header value for URL from JAR, or NIL if none match."
  (let* ((uri (quri:uri url))
         (host (or (quri:uri-host uri) ""))
         (path (or (quri:uri-path uri) "/"))
         (securep (string-equal (or (quri:uri-scheme uri) "http") "https"))
         (cookies (cl-cookie:cookie-jar-host-cookies
                   jar host path :securep securep)))
    (when cookies
      (cl-cookie:write-cookie-header cookies))))

(defun inject-cookie-header (headers jar url)
  "Return HEADERS alist with Cookie from JAR for URL (replaces existing Cookie)."
  (let ((value (cookie-header-value jar url)))
    (let ((headers (remove "cookie" headers :key #'car :test #'string-equal)))
      (if value
          (acons "cookie" value headers)
          headers))))

(defun %header-set-cookie-values (headers)
  "Collect Set-Cookie header string(s) from alist or EQUAL hash-table."
  (cond
    ((hash-table-p headers)
     (let ((v (or (gethash "set-cookie" headers)
                  (gethash "Set-Cookie" headers))))
       (cond ((null v) nil)
             ((listp v) v)
             (t (list v)))))
    (t
     (loop for pair in headers
           when (and (consp pair)
                     (string-equal (car pair) "set-cookie"))
             collect (cdr pair)))))

(defun merge-response-cookies (jar url headers)
  "Parse Set-Cookie from HEADERS into JAR. Returns list of new cookies."
  (let* ((uri (quri:uri url))
         (host (or (quri:uri-host uri) ""))
         (path (or (quri:uri-path uri) "/"))
         (parsed nil))
    (dolist (sc (%header-set-cookie-values headers))
      (handler-case
          (push (cl-cookie:parse-set-cookie-header sc host path) parsed)
        (error ()
          ;; Ignore malformed Set-Cookie (browsers drop them).
          nil)))
    (setf parsed (nreverse parsed))
    (when (and jar parsed)
      (cl-cookie:merge-cookies jar parsed))
    parsed))
