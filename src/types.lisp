(in-package #:http-protocol)

;;; Value types for the HTTP client protocol (brief § Protocol surface).

(defclass http-backend ()
  ((name :initarg :name :reader backend-name :initform "unknown")))

(defun http-backend-p (x) (typep x 'http-backend))

(defclass http-client ()
  ((backend :initarg :backend :reader http-client-backend)
   (base-url :initarg :base-url :accessor http-client-base-url :initform nil)
   (headers :initarg :headers :accessor http-client-headers :initform nil)
   (cookie-jar :initarg :cookie-jar :accessor http-client-cookie-jar
               :initform nil
               :documentation "cl-cookie:cookie-jar (requests Session jar). Lazy-created by backends.")
   (timeout :initarg :timeout :accessor http-client-timeout :initform nil)
   (max-redirects :initarg :max-redirects :accessor http-client-max-redirects :initform 5)
   (proxy :initarg :proxy :accessor http-client-proxy :initform nil)
   (verify :initarg :verify :accessor http-client-verify :initform t)
   (defaults :initarg :defaults :accessor http-client-defaults :initform nil
             :documentation "Plist of extra backend-specific defaults.")))

(defun http-client-p (x) (typep x 'http-client))

(defclass http-request ()
  ((method :initarg :method :accessor http-request-method :initform :get)
   (url :initarg :url :accessor http-request-url)
   (headers :initarg :headers :accessor http-request-headers :initform nil)
   (content :initarg :content :accessor http-request-content :initform nil)
   (params :initarg :params :accessor http-request-params :initform nil)
   (timeout :initarg :timeout :accessor http-request-timeout :initform nil)
   (max-redirects :initarg :max-redirects :accessor http-request-max-redirects :initform nil)
   (cookies :initarg :cookies :accessor http-request-cookies :initform nil
            :documentation "Per-request cookies: alist ((name . value)…) merged into jar, or a cookie-jar.")
   (accept-encoding :initarg :accept-encoding :accessor http-request-accept-encoding
                    :initform :default
                    :documentation "T/:default → available-content-codings; NIL → omit; list/string → use.")
   (content-encoding :initarg :content-encoding :accessor http-request-content-encoding
                     :initform nil
                     :documentation "Opt-in request body coding (:gzip/:deflate/:br/:zstd).")
   (decompress :initarg :decompress :accessor http-request-decompress :initform t)
   (force-binary :initarg :force-binary :accessor http-request-force-binary :initform t)
   (want-stream :initarg :want-stream :accessor http-request-want-stream :initform nil)
   (raise-for-status :initarg :raise-for-status :accessor http-request-raise-for-status
                     :initform nil)
   (extras :initarg :extras :accessor http-request-extras :initform nil
           :documentation "Plist of backend-specific overrides.")))

(defun http-request-p (x) (typep x 'http-request))

(defun make-http-request (&rest args &key &allow-other-keys)
  (apply #'make-instance 'http-request args))

(defclass http-response ()
  ((status :initarg :status :reader response-status)
   (headers :initarg :headers :reader response-headers
            :documentation "EQUAL hash-table, lowercase string keys (dexador shape).")
   (body :initarg :body :reader response-body)
   (url :initarg :url :reader response-url :initform nil)
   (http-version :initarg :http-version :reader response-http-version :initform nil)
   (cookies :initarg :cookies :reader response-cookies :initform nil
            :documentation "cl-cookie:cookie list set by this response (Set-Cookie).")
   (history :initarg :history :reader response-history :initform nil
            :documentation "Prior HTTP-RESPONSE objects in a redirect chain.")
   (request :initarg :request :reader response-request :initform nil)))

(defun http-response-p (x) (typep x 'http-response))

(defun response-header (response name)
  "Lookup header NAME (string or keyword) in RESPONSE."
  (let* ((key (string-downcase (string name)))
         (ht (response-headers response)))
    (gethash key ht)))

(defvar *http-backend* nil
  "Current HTTP-BACKEND. Required for facade one-shots.")

(defvar *http-client* nil
  "Optional default HTTP-CLIENT for facade one-shots.")

(defmacro with-http-backend ((backend) &body body)
  `(let ((*http-backend* ,backend))
     ,@body))

(defmacro with-http-client ((client) &body body)
  `(let ((*http-client* ,client))
     ,@body))
