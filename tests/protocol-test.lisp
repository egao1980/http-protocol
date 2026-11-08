(in-package #:http-protocol/tests)

(defun %bytes (s)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code s))

(deftest coerce-to-octets-utf8
  (ok (equalp (babel:string-to-octets "hi" :encoding :utf-8)
              (coerce-to-octets "hi")))
  (let ((em (string (code-char 8212))))
    (ok (equalp (babel:string-to-octets em :encoding :utf-8)
                (coerce-to-octets em)))
    (ok (signals (map '(simple-array (unsigned-byte 8) (*)) #'char-code em)
                 'type-error))))

(deftest normalize-and-parse
  (ok (eq (normalize-content-coding "gzip") :gzip))
  (ok (eq (normalize-content-coding :x-gzip) :gzip))
  (ok (eq (normalize-content-coding "BR") :br))
  (ok (eq (normalize-content-coding "x-snappy") :snappy))
  (ok (equal (parse-content-encoding "gzip, deflate, br")
             '(:gzip :deflate :br)))
  (ok (equal (parse-content-encoding "zstd, snappy")
             '(:zstd :snappy)))
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
  (ok (equal (cdr (assoc :zstd *content-coding-systems*)) "http-encoding-zstd"))
  (ok (equal (cdr (assoc :snappy *content-coding-systems*)) "http-encoding-snappy")))

(deftest missing-coding-is-probed-once
  "A miss used to be re-probed (ASDF lookup + LOAD-SYSTEM) on every request."
  (let ((http-protocol::*coding-availability* (make-hash-table :test #'eq))
        (probes 0)
        (orig (fdefinition 'http-protocol::%probe-coding)))
    (unwind-protect
         (progn
           (setf (fdefinition 'http-protocol::%probe-coding)
                 (lambda (c) (declare (ignore c)) (incf probes) nil))
           (ng (content-coding-supported-p :snappy))
           (ng (content-coding-supported-p :snappy))
           (ok (= 1 probes)))
      (setf (fdefinition 'http-protocol::%probe-coding) orig))))
