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
   (auth :initarg :auth :accessor http-client-auth :initform nil
         :documentation "Default :auth — (:basic u p) | (:bearer tok) | string.")
   (timeout :initarg :timeout :accessor http-client-timeout :initform nil
            :documentation "HTTP-TIMEOUT | number (total) | plist | NIL.")
   (retry :initarg :retry :accessor http-client-retry :initform nil
          :documentation "HTTP-RETRY | integer total | T | NIL (no retry).")
   (max-redirects :initarg :max-redirects :accessor http-client-max-redirects :initform 5)
   (proxy :initarg :proxy :accessor http-client-proxy :initform nil
          :documentation "HTTP-PROXY-CONFIG | URL string | scheme/host alist | NIL → *default-proxy-config*.")
   (pool :initarg :pool :accessor http-client-pool :initform t
         :documentation
         "HTTP-CONNECTION-POOL | T (shared *default-connection-pool*) | NIL.
          Concrete pools are backend subclasses of HTTP-CONNECTION-POOL.")
   (verify :initarg :verify :accessor http-client-verify :initform t)
   (defaults :initarg :defaults :accessor http-client-defaults :initform nil
             :documentation "Plist of extra backend-specific defaults.")))

(defun http-client-p (x) (typep x 'http-client))

;;; Upload / download file value — stream + metadata. No filesystem here
;;; (urllib3/httpx layer). Path open/save + MIME guess → requests-like lib later.

(defclass http-file ()
  ((filename :initarg :filename :accessor http-file-filename :initform nil
             :documentation "Suggested filename for Content-Disposition (string or NIL).")
   (content-type :initarg :content-type :accessor http-file-content-type
                 :initform "application/octet-stream")
   (content-length :initarg :content-length :accessor http-file-content-length
                   :initform nil
                   :documentation "Octet length when known (enables Content-Length); else NIL.")
   (content :initarg :content :accessor http-file-content
            :documentation "Binary input stream, octet vector, or UTF-8 string.")
   (field-name :initarg :field-name :accessor http-file-field-name :initform nil
               :documentation "Optional multipart field name when not supplied by alist key.")))

(defun http-file-p (x) (typep x 'http-file))

(defun make-http-file (content &key filename content-type content-length field-name)
  "Build an HTTP-FILE. CONTENT must be a stream, octet vector, or string."
  (check-type content (or stream string vector))
  (make-instance 'http-file
                 :content content
                 :filename filename
                 :content-type (or content-type "application/octet-stream")
                 :content-length content-length
                 :field-name field-name))

(defclass http-request ()
  ((method :initarg :method :accessor http-request-method :initform :get)
   (url :initarg :url :accessor http-request-url)
   (headers :initarg :headers :accessor http-request-headers :initform nil)
   (content :initarg :content :accessor http-request-content :initform nil
            :documentation "Raw body: octets / string / binary input stream / http-file.")
   (data :initarg :data :accessor http-request-data :initform nil
         :documentation "Form fields alist. Alone → urlencoded; with :files → multipart.")
   (files :initarg :files :accessor http-request-files :initform nil
          :documentation "Multipart files: alist ((name . http-file|stream|octets)…) or list of http-file.")
   (params :initarg :params :accessor http-request-params :initform nil
           :documentation "Query alist merged into URL via quri before SEND.")
   (timeout :initarg :timeout :accessor http-request-timeout :initform nil
            :documentation "Overrides client; HTTP-TIMEOUT | number | plist.")
   (retry :initarg :retry :accessor http-request-retry :initform nil
          :documentation "Overrides client; HTTP-RETRY | integer | T | NIL.")
   (max-redirects :initarg :max-redirects :accessor http-request-max-redirects :initform nil)
   (proxy :initarg :proxy :accessor http-request-proxy :initform nil
          :documentation "Overrides client proxy config for this request.")
   (cookies :initarg :cookies :accessor http-request-cookies :initform nil
            :documentation "Per-request cookies: alist ((name . value)…) merged into jar, or a cookie-jar.")
   (auth :initarg :auth :accessor http-request-auth :initform nil
         :documentation "(:basic u p) | (:bearer tok) | string; overrides client auth.")
   (range :initarg :range :accessor http-request-range :initform nil
          :documentation "(start end) | (start) | string → Range header.")
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
   (body :initarg :body :reader response-body
         :documentation "Octets, string, stream, or http-file when wrapped.")
   (url :initarg :url :reader response-url :initform nil)
   (http-version :initarg :http-version :reader response-http-version :initform nil)
   (cookies :initarg :cookies :reader response-cookies :initform nil
            :documentation "cl-cookie:cookie list set by this response (Set-Cookie).")
   (history :initarg :history :reader response-history :initform nil
            :documentation "Prior HTTP-RESPONSE objects in a redirect chain.")
   (request :initarg :request :reader response-request :initform nil)))

(defun response-as-http-file (response &key filename content-type)
  "Wrap RESPONSE body as an HTTP-FILE (stream when :want-stream was used).

   FILENAME defaults to Content-Disposition (RFC 6266 §4.3 / RFC 8187)."
  (make-http-file (or (response-body response) #())
                  :filename (or filename
                                (content-disposition-filename
                                 (response-header response "content-disposition")))
                  :content-type (or content-type
                                    (response-header response "content-type")
                                    "application/octet-stream")
                  :content-length
                  (let ((cl (response-header response "content-length")))
                    (when cl (ignore-errors (parse-integer cl :junk-allowed t))))))

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
