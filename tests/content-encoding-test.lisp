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

(deftest gzip-round-trip
  (let* ((raw (%bytes "hello gzip content-encoding"))
         (enc (encode-content-coding :gzip raw))
         (dec (decode-content-coding :gzip enc)))
    (ok (not (equalp raw enc)))
    (ok (equalp raw dec))))

(deftest deflate-round-trip
  (let* ((raw (%bytes "hello deflate/zlib"))
         (enc (encode-content-coding :deflate raw))
         (dec (decode-content-coding :deflate enc)))
    (ok (equalp raw dec))))

(deftest identity-and-multi
  (let ((raw (%bytes "plain")))
    (ok (equalp raw (decode-content-coding :identity raw)))
    (ok (equalp raw (encode-content-coding :identity raw))))
  (let* ((raw (%bytes "layered"))
         (enc (encode-content-codings '(:gzip) raw))
         (dec (decode-content-codings '(:gzip) enc)))
    (ok (equalp raw dec))))

(deftest accept-encoding-defaults
  (let ((hdr (default-accept-encoding)))
    (ok (search "gzip" hdr))
    (ok (search "deflate" hdr)))
  (ok (content-coding-supported-p :gzip))
  (ok (content-coding-supported-p :deflate))
  (ok (not (content-coding-supported-p :compress))))

(deftest unsupported-coding
  (ok (signals (decode-content-coding :compress (%bytes "x"))
               'unsupported-content-coding)))

(deftest brotli-round-trip-when-available
  (when (content-coding-supported-p :br)
    (let* ((raw (%bytes "hello brotli"))
           (enc (encode-content-coding :br raw :quality 4))
           (dec (decode-content-coding :br enc)))
      (ok (equalp raw dec)))))

(deftest zstd-round-trip-when-available
  (when (content-coding-supported-p :zstd)
    (let* ((raw (%bytes "hello zstd"))
           (enc (encode-content-coding :zstd raw :level 3))
           (dec (decode-content-coding :zstd enc)))
      (ok (equalp raw dec)))))
