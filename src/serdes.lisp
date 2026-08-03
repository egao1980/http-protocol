(in-package #:http-protocol)

;;; Typed request/response :data (de)serialization.
;;; FORM-DATA = HTML form (urlencoded / multipart); :DATA = application payload.

(defvar *data-serializers* nil
  "Alist (type . fn). FN: (data) → string|octets. Dynamic; see WITH-DATA-SERIALIZER.")

(defvar *data-deserializers* nil
  "Alist (type . fn). FN: (octets) → value. Dynamic; see WITH-DATA-DESERIALIZER.")

(defvar *json-encoder* nil
  "Fallback encoder for :json when *DATA-SERIALIZERS* has no :json entry.
   (fn data) → string|octets.")

(defvar *json-decoder* nil
  "Fallback decoder for :json when *DATA-DESERIALIZERS* has no :json entry.
   (fn octets|string) → value.")

(defun lookup-data-serializer (type)
  (or (cdr (assoc type *data-serializers* :test #'equal))
      (when (eq type :json) *json-encoder*)))

(defun lookup-data-deserializer (type)
  (or (cdr (assoc type *data-deserializers* :test #'equal))
      (when (eq type :json) *json-decoder*)))

(defmacro with-data-serializer ((type function) &body body)
  "Bind FUNCTION as the serializer for TYPE around BODY.
   Example: (with-data-serializer (:json #'my-encode) (http:post url :data obj :data-type :json))"
  `(let ((*data-serializers*
          (acons ,type ,function *data-serializers*)))
     ,@body))

(defmacro with-data-deserializer ((type function) &body body)
  "Bind FUNCTION as the deserializer for TYPE around BODY.
   Example:
     (with-data-deserializer (:json #'my-parse)
       (response-data (http:get url) :json))"
  `(let ((*data-deserializers*
          (acons ,type ,function *data-deserializers*)))
     ,@body))

(defmacro with-data-codec ((type &key encoder decoder) &body body)
  "Bind both serializer and deserializer for TYPE.
   (with-data-codec (:json :encoder #'enc :decoder #'dec) …)"
  (let ((forms body))
    (when encoder
      (setf forms `((with-data-serializer (,type ,encoder) ,@forms))))
    (when decoder
      (setf forms `((with-data-deserializer (,type ,decoder) ,@forms))))
    `(progn ,@forms)))

(defun %media-type (content-type)
  "Lowercase type/subtype without parameters, or NIL."
  (when (and content-type (plusp (length content-type)))
    (let* ((s (string-downcase (string-trim '(#\Space #\Tab) content-type)))
           (semi (position #\; s)))
      (if semi (string-trim '(#\Space #\Tab) (subseq s 0 semi)) s))))

(defun %body-as-octets (body)
  (cond
    ((null body) #())
    ((typep body '(vector (unsigned-byte 8))) body)
    ((stringp body) (babel:string-to-octets body :encoding :utf-8))
    ((streamp body) (slurp-octets body))
    ((http-file-p body) (%body-as-octets (http-file-content body)))
    (t (error 'http-protocol-error
              :message (format nil "cannot decode body of type ~S" (type-of body))))))

(defun %body-as-string (body)
  (babel:octets-to-string (%body-as-octets body) :encoding :utf-8))

(defun %encoded-to-octets (encoded)
  (etypecase encoded
    (string (babel:string-to-octets encoded :encoding :utf-8))
    ((vector (unsigned-byte 8)) encoded)))

(defun infer-encode-data-type (data content-type)
  "Pick a :data-type keyword from DATA and/or Content-Type."
  (let ((mt (%media-type content-type)))
    (cond
      ((or (equal mt "application/json") (equal mt "text/json")) :json)
      ((equal mt "application/x-www-form-urlencoded") :urlencoded)
      ((and mt (or (eql (mismatch mt "text/") 5)
                   (equal mt "application/xml")
                   (equal mt "application/javascript")))
       :text)
      ((typep data '(vector (unsigned-byte 8))) :octets)
      ((stringp data) :text)
      ((hash-table-p data) :json)
      ((and (listp data) (every #'consp data)) :urlencoded)
      ((or (listp data) (vectorp data)) :json)
      (t :octets))))

(defun infer-decode-data-type (content-type)
  (let ((mt (%media-type content-type)))
    (cond
      ((null mt) :octets)
      ((or (equal mt "application/json") (equal mt "text/json")) :json)
      ((equal mt "application/x-www-form-urlencoded") :urlencoded)
      ((or (eql (mismatch mt "text/") 5)
           (equal mt "application/xml")
           (equal mt "application/javascript"))
       :text)
      (t :octets))))

(defgeneric encode-http-data (data type content-type)
  (:documentation
   "Serialize DATA as TYPE for an HTTP request body.

    TYPE — keyword (:auto :text :octets :urlencoded :json …) or media-type string.
    CONTENT-TYPE — request Content-Type header, or NIL (encoder may supply one).

    Returns (values wire content-type content-length).
    Extension types: register via WITH-DATA-SERIALIZER / *DATA-SERIALIZERS*."))

(defgeneric decode-http-data (body type content-type &key headers)
  (:documentation
   "Deserialize response BODY as TYPE.

    TYPE — keyword (:auto :text :octets :urlencoded :json …) or media-type string.
    CONTENT-TYPE — response Content-Type (may be NIL).
    Extension types: WITH-DATA-DESERIALIZER / *DATA-DESERIALIZERS*."))

(defmethod encode-http-data (data (type (eql :auto)) content-type)
  (encode-http-data data (infer-encode-data-type data content-type) content-type))

(defmethod encode-http-data ((data string) (type (eql :text)) content-type)
  (let* ((octets (babel:string-to-octets data :encoding :utf-8))
         (ct (or content-type "text/plain; charset=utf-8")))
    (values octets ct (length octets))))

(defmethod encode-http-data (data (type (eql :octets)) content-type)
  (let* ((octets (etypecase data
                   (null #())
                   ((vector (unsigned-byte 8)) data)
                   (string (babel:string-to-octets data :encoding :utf-8))
                   (stream (slurp-octets data))))
         (ct (or content-type "application/octet-stream")))
    (values octets ct (length octets))))

(defmethod encode-http-data ((data list) (type (eql :urlencoded)) content-type)
  (let* ((octets (encode-urlencoded data))
         (ct (or content-type "application/x-www-form-urlencoded")))
    (values octets ct (length octets))))

(defun %encode-via-registry (data type content-type default-ct)
  (let ((fn (lookup-data-serializer type)))
    (unless fn
      (error 'unsupported-operation
             :operation (list :encode type)
             :message
             (format nil "no serializer for ~S — use WITH-DATA-SERIALIZER or *JSON-ENCODER*"
                     type)))
    (let* ((octets (%encoded-to-octets (funcall fn data)))
           (ct (or content-type default-ct)))
      (values octets ct (length octets)))))

(defmethod encode-http-data (data (type (eql :json)) content-type)
  (%encode-via-registry data :json content-type "application/json"))

(defmethod encode-http-data (data (type symbol) content-type)
  "Extension / registered types (not covered by more specific eql methods)."
  (%encode-via-registry data type content-type "application/octet-stream"))

(defmethod encode-http-data (data (type string) content-type)
  (encode-http-data data
                    (infer-encode-data-type data (or content-type type))
                    (or content-type type)))

(defmethod decode-http-data (body (type (eql :auto)) content-type &key headers)
  (declare (ignore headers))
  (decode-http-data body (infer-decode-data-type content-type) content-type))

(defmethod decode-http-data (body (type (eql :octets)) content-type &key headers)
  (declare (ignore content-type headers))
  (%body-as-octets body))

(defmethod decode-http-data (body (type (eql :text)) content-type &key headers)
  (declare (ignore content-type headers))
  (%body-as-string body))

(defmethod decode-http-data (body (type (eql :urlencoded)) content-type &key headers)
  (declare (ignore content-type headers))
  (quri:url-decode-params (%body-as-string body) :lenient t))

(defun %decode-via-registry (body type)
  (let ((fn (lookup-data-deserializer type)))
    (unless fn
      (error 'unsupported-operation
             :operation (list :decode type)
             :message
             (format nil "no deserializer for ~S — use WITH-DATA-DESERIALIZER or *JSON-DECODER*"
                     type)))
    (funcall fn (%body-as-octets body))))

(defmethod decode-http-data (body (type (eql :json)) content-type &key headers)
  (declare (ignore content-type headers))
  (%decode-via-registry body :json))

(defmethod decode-http-data (body (type symbol) content-type &key headers)
  (declare (ignore content-type headers))
  (%decode-via-registry body type))

(defmethod decode-http-data (body (type string) content-type &key headers)
  (decode-http-data body
                    (infer-decode-data-type (or content-type type))
                    (or content-type type)
                    :headers headers))

(defun request-header-content-type (request)
  (cdr (assoc "content-type" (http-request-headers request) :test #'string-equal)))

(defun response-data (response &optional (type :auto) &key content-type headers)
  "Deserialize RESPONSE body via DECODE-HTTP-DATA.
   TYPE defaults to :auto (from Content-Type)."
  (check-type response http-response)
  (decode-http-data (response-body response)
                    type
                    (or content-type (response-header response "content-type"))
                    :headers (or headers (response-headers response))))
