(in-package #:http-protocol)

;;; Request/response body stream helpers (cl-stack#71).
;;; Goal: O(buffer) peak memory, not O(body).

(defun prepare-request-content (content &key coding
                                (buffer-size *http-stream-buffer-size*))
  "Normalize request CONTENT for a backend wire send.

   Returns (values wire-content content-encoding-header-or-nil).
   WIRE-CONTENT is NIL, an octet vector, or a binary input stream.

   When CODING is non-NIL, CONTENT is encoded via ENCODE-CONTENT-CODING
   (stream → pull encoding stream when the coding backend supports it;
   gzip/deflate via salza2 may still materialize — coding-backend limit).

   Plain streams are wrapped in MAKE-BUFFERED-BINARY-INPUT-STREAM so backends
   that read byte-at-a-time still batch syscalls."
  (labels ((bufferize (s)
             (if (typep s 'buffered-binary-input-stream)
                 s
                 (make-buffered-binary-input-stream
                  s :buffer-size buffer-size)))
           (unwrap (x)
             (if (http-file-p x) (http-file-content x) x))
           (as-wire (x)
             (let ((x (unwrap x)))
               (etypecase x
                 (null nil)
                 (stream (bufferize x))
                 ((or string vector) (coerce-to-octets x))))))
    (if (null coding)
        (values (as-wire content) nil)
        (let* ((c (normalize-content-coding coding))
               (header (string-downcase (symbol-name c)))
               (raw (unwrap content))
               (src (etypecase raw
                      (null (make-octet-input-stream
                             (make-array 0 :element-type '(unsigned-byte 8))))
                      (stream raw)
                      ((or string vector)
                       (make-octet-input-stream (coerce-to-octets raw)))))
               (encoded (encode-content-coding c src)))
          (values (if (streamp encoded)
                      (bufferize encoded)
                      encoded)
                  header)))))

(defun %strip-body-headers (headers)
  (let ((n (make-hash-table :test #'equal)))
    (maphash (lambda (k v) (setf (gethash k n) v)) headers)
    (remhash "content-encoding" n)
    (remhash "content-length" n)
    n))

(defun wrap-response-body-stream (stream headers &key (decompress t)
                                   (buffer-size *http-stream-buffer-size*))
  "Wrap a wire response STREAM for the application.

   When DECOMPRESS is true, apply Content-Encoding via decoding Gray streams
   (no full-body vector). Always outer-wrap with a fixed BUFFER-SIZE buffer.

   Returns (values app-stream headers*)."
  (check-type stream stream)
  (let* ((ce (gethash "content-encoding" headers))
         (codings (and decompress (parse-content-encoding ce)))
         (decoded (if codings
                      (decode-content-codings codings stream)
                      stream))
         (app (if (typep decoded 'buffered-binary-input-stream)
                  decoded
                  (make-buffered-binary-input-stream
                   decoded :buffer-size buffer-size
                   :close-source-p t)))
         (headers* (if codings (%strip-body-headers headers) headers)))
    (values app headers*)))

(defun body-stream (response &key (buffer-size *http-stream-buffer-size*))
  "Binary input stream over RESPONSE body.

   If the body is already a stream, return it (buffer-wrap if bare).
   If the body is an HTTP-FILE, stream its content.
   If the body is an octet vector, wrap with MAKE-OCTET-INPUT-STREAM
   (materialized case — prefer :WANT-STREAM T on the request for large bodies)."
  (let ((body (response-body response)))
    (when (http-file-p body)
      (setf body (http-file-content body)))
    (cond
      ((null body)
       (make-octet-input-stream
        (make-array 0 :element-type '(unsigned-byte 8))))
      ((streamp body)
       (if (or (typep body 'buffered-binary-input-stream)
               (typep body 'octet-input-stream))
           body
           (make-buffered-binary-input-stream body :buffer-size buffer-size
                                              :close-source-p nil)))
      ((vectorp body)
       (make-octet-input-stream body))
      ((stringp body)
       (make-octet-input-stream (coerce-to-octets body)))
      (t
       (error 'http-protocol-error
              :message (format nil "response body is not streamable: ~A"
                               (type-of body)))))))
