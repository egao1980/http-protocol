(in-package #:http-protocol/tests)

(deftest buffered-stream-roundtrip
  (let* ((raw (make-array 200000 :element-type '(unsigned-byte 8)
                          :initial-element 7))
         (buf-size 4096))
    (with-open-stream (src (make-octet-input-stream raw))
      (with-open-stream (in (make-buffered-binary-input-stream
                             src :buffer-size buf-size))
        (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0))
              (chunk (make-array buf-size :element-type '(unsigned-byte 8))))
          (loop for n = (read-sequence chunk in)
                while (plusp n)
                do (loop for i below n do (vector-push-extend (aref chunk i) out)))
          (ok (= (length raw) (length out)))
          (ok (equalp raw (coerce out '(simple-array (unsigned-byte 8) (*))))))))))

(deftest buffered-stream-read-byte
  (with-open-stream (src (make-octet-input-stream #(1 2 3)))
    (with-open-stream (in (make-buffered-binary-input-stream src :buffer-size 2))
      (ok (= 1 (read-byte in)))
      (ok (= 2 (read-byte in)))
      (ok (= 3 (read-byte in)))
      (ok (eq :eof (read-byte in nil :eof))))))

(deftest buffered-stream-over-live-pipe-does-not-wait-for-full-buffer
  "Duplex bodies: a 5-octet read must return once 5 octets exist, even though
   the pipe is still open and the 64K buffer is nowhere near full."
  (let* ((pipe (make-http-body-pipe))
         (in (make-buffered-binary-input-stream pipe :close-source-p nil))
         (hdr (make-array 5 :element-type '(unsigned-byte 8)))
         (got nil))
    (write-body-pipe pipe #(0 0 0 0 3 10 1 65))
    (let ((th (bt:make-thread (lambda () (setf got (read-sequence hdr in))))))
      (loop repeat 100 until got do (sleep 0.01))
      (unless got
        (ignore-errors (bt:destroy-thread th)))
      (ok (eql 5 got) "read-sequence returned with the pipe still open")
      (ok (equalp #(0 0 0 0 3) hdr))
      ;; The rest of the first write is already buffered.
      (ok (= 3 (read-sequence hdr in :end 3)))
      (ok (equalp #(10 1 65) (subseq hdr 0 3)))
      (close-body-pipe pipe)
      (ok (eq :eof (read-byte in nil :eof))))))

(deftest prepare-request-content-stream-no-coding
  (with-open-stream (src (make-octet-input-stream #(9 8 7)))
    (multiple-value-bind (wire ce)
        (prepare-request-content src)
      (ok (null ce))
      (ok (typep wire 'buffered-binary-input-stream))
      (ok (equalp #(9 8 7) (slurp-octets wire))))))

(deftest prepare-request-content-vector-stays-vector
  (let ((v (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))))
    (multiple-value-bind (wire ce)
        (prepare-request-content v)
      (ok (null ce))
      (ok (equalp v wire)))))

(deftest wrap-response-identity
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "content-type" ht) "application/octet-stream")
    (with-open-stream (src (make-octet-input-stream #(4 5 6)))
      (multiple-value-bind (app headers*)
          (wrap-response-body-stream src ht :decompress t :buffer-size 2)
        (ok (typep app 'buffered-binary-input-stream))
        (ok (string= "application/octet-stream"
                     (gethash "content-type" headers*)))
        (ok (equalp #(4 5 6) (slurp-octets app)))))))

(deftest body-stream-from-octets
  (let ((res (make-instance 'http-response
                            :status 200
                            :headers (make-hash-table :test #'equal)
                            :body #(10 11))))
    (with-open-stream (in (body-stream res))
      (ok (equalp #(10 11) (slurp-octets in))))))

(deftest copy-stream-counts
  (uiop:with-temporary-file (:stream out :pathname path
                             :element-type '(unsigned-byte 8)
                             :direction :io)
    (declare (ignore path))
    (with-open-stream (src (make-octet-input-stream #(1 2 3 4 5)))
      (ok (= 5 (copy-stream src out :buffer-size 2))))
    (file-position out 0)
    (let ((got (make-array 5 :element-type '(unsigned-byte 8))))
      (ok (= 5 (read-sequence got out)))
      (ok (equalp #(1 2 3 4 5) got)))))

(deftest body-pipe-write-then-read
  (let ((p (make-http-body-pipe)))
    (write-body-pipe p #(1 2 3))
    (write-body-pipe p #(4 5))
    (close-body-pipe p)
    (ok (http-body-pipe-p p))
    (ok (http-body-pipe-listen p))
    (let ((out (make-array 5 :element-type '(unsigned-byte 8))))
      (ok (= 5 (http-body-pipe-read-available p out 0 5)))
      (ok (equalp #(1 2 3 4 5) out)))
    (ok (http-body-pipe-eof-p p))))

(deftest body-pipe-gray-roundtrip
  (let ((p (make-http-body-pipe)))
    (write-sequence #(9 8 7) p)
    (close-body-pipe p)
    (ok (equalp #(9 8 7) (slurp-octets p)))))

(deftest body-pipe-not-bufferized
  (let ((p (make-http-body-pipe)))
    (multiple-value-bind (wire ce)
        (prepare-request-content p)
      (ok (null ce))
      (ok (eq p wire)))))

(deftest body-pipe-on-data
  (let* ((n 0)
         (p (make-http-body-pipe :on-data (lambda () (incf n)))))
    (write-body-pipe p #(1))
    (close-body-pipe p)
    (ok (= 2 n))))

(deftest binary-body-streams-report-octet-element-type
  "STREAM-ELEMENT-TYPE answers (unsigned-byte 8) for every binary body stream,
   so a consumer can ask before choosing READ-CHAR or READ-BYTE (an OpenAI
   error body arrived as a buffered stream and hit NO-APPLICABLE-METHOD)."
  (flet ((octets-p (s) (equal '(unsigned-byte 8) (stream-element-type s))))
    (with-open-stream (src (make-octet-input-stream #(1 2 3)))
      (ok (octets-p src))
      (with-open-stream (in (make-buffered-binary-input-stream src :close-source-p nil))
        (ok (octets-p in))
        (ok (= 1 (read-byte in)))))
    (with-open-stream (s (make-multipart-form-stream (list (list :name "a" :content "x"))))
      (ok (octets-p s)))
    (with-open-stream (s (make-pooled-body-stream (make-octet-input-stream #(1))))
      (ok (octets-p s)))))

(defun %drain-octets (stream)
  (loop for b = (read-byte stream nil nil) while b collect b))

(deftest binary-body-streams-answer-standard-stream-functions
  "#50 audit: each binary body stream answers the standard stream functions a
   consumer may call on a body. SBCL's Gray defaults cover OPEN-STREAM-P and
   the direction predicates; there STREAM-ELEMENT-TYPE, LISTEN and CLEAR-INPUT
   had no binary method and signalled NO-APPLICABLE-METHOD."
  (dolist (make (list (lambda () (make-octet-input-stream #(1 2 3)))
                      (lambda () (make-buffered-binary-input-stream
                                  (make-octet-input-stream #(1 2 3))
                                  :buffer-size 2))
                      (lambda () (make-pooled-body-stream
                                  (make-octet-input-stream #(1 2 3))))
                      (lambda () (make-multipart-form-stream
                                  (list (list :name "a" :content "x"))))))
    (let ((s (funcall make)))
      (testing (string-downcase (symbol-name (type-of s)))
        (ok (equal '(unsigned-byte 8) (stream-element-type s)))
        (ok (input-stream-p s))
        (ok (not (output-stream-p s)))
        (ok (open-stream-p s))
        (ok (null (clear-input s)) "CLEAR-INPUT is a no-op")
        (ok (listen s) "an octet is ready before EOF")
        (ok (plusp (length (%drain-octets s))) "CLEAR-INPUT dropped nothing")
        (ok (not (listen s)) "LISTEN is false at EOF")
        (close s)
        (ok (not (open-stream-p s)))
        (ok (not (listen s)) "LISTEN is false once closed"))))
  (let ((p (make-http-body-pipe)))
    (ok (equal '(unsigned-byte 8) (stream-element-type p)))
    (ok (and (input-stream-p p) (output-stream-p p)))
    (ok (null (clear-input p)))
    (close p)
    (ok (not (open-stream-p p)))))

(deftest error-body-reads-through-buffered-stream
  "#50: a 401 from api.anthropic.com arrives as a buffered body stream. The
   provider asks its element type, reads octets and gets the error JSON (it
   crashed with NO-APPLICABLE-METHOD on STREAM-ELEMENT-TYPE instead)."
  (let* ((json "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"invalid x-api-key\"}}")
         (headers (let ((ht (make-hash-table :test #'equal)))
                    (setf (gethash "content-type" ht) "application/json")
                    ht))
         (res (make-instance 'http-response
                             :status 401 :headers headers
                             :body (wrap-response-body-stream
                                    (make-octet-input-stream
                                     (babel:string-to-octets json :encoding :utf-8))
                                    headers)))
         (body (response-body res)))
    (ok (typep body 'buffered-binary-input-stream))
    ;; llm-protocol-anthropic / -openai %COERCE-BODY-STRING
    (ok (not (subtypep (stream-element-type body) 'character)))
    (ok (string= json (babel:octets-to-string
                       (coerce (%drain-octets body) '(vector (unsigned-byte 8)))
                       :encoding :utf-8)))
    (close body)))
