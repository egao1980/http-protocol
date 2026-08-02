(in-package #:http-protocol/tests)

(defun %bytes (s)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

(deftest normalize-and-parse
  (ok (eq (normalize-content-coding "gzip") :gzip))
  (ok (eq (normalize-content-coding :x-gzip) :gzip))
  (ok (eq (normalize-content-coding "BR") :br))
  (ok (equal (parse-content-encoding "gzip, deflate, br")
             '(:gzip :deflate :br)))
  (ok (equal (parse-content-encoding "gzip;q=1.0, identity;q=0.5")
             '(:gzip :identity))))

(deftest identity
  (let ((raw (%bytes "plain")))
    (ok (equalp raw (decode-content-coding :identity raw)))
    (ok (equalp raw (encode-content-coding :identity raw))))
  (with-open-stream (in (make-octet-input-stream (%bytes "stream")))
    (ok (eq in (decode-content-coding :identity in)))))

(deftest unsupported-without-backend
  ;; :compress is never a backend; :gzip errors if chipz backend not loaded.
  (ok (signals (decode-content-coding :compress (%bytes "x"))
               'unsupported-content-coding)))

(deftest coding-system-map
  (ok (equal (cdr (assoc :gzip *content-coding-systems*)) "http-encoding-chipz"))
  (ok (equal (cdr (assoc :br *content-coding-systems*)) "http-encoding-brotli"))
  (ok (equal (cdr (assoc :zstd *content-coding-systems*)) "http-encoding-zstd")))
