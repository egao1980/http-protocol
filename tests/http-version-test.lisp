(in-package #:http-protocol/tests)

(deftest normalize-http-version-coerces
  (ok (eq :auto (normalize-http-version nil)))
  (ok (eq :http/1.1 (normalize-http-version "HTTP/1.1")))
  (ok (eq :http/1.1 (normalize-http-version 1.1)))
  (ok (eq :http/2 (normalize-http-version "h2")))
  (ok (eq :http/2 (normalize-http-version :http/2)))
  (ok (signals (normalize-http-version "http/3") 'http-protocol-error)))

(deftest alpn-protocols-for-version-offers
  (ok (equal '("h2" "http/1.1") (alpn-protocols-for-version :auto)))
  (ok (equal '("h2" "http/1.1") (alpn-protocols-for-version :http/2)))
  (ok (equal '("http/1.1") (alpn-protocols-for-version :http/1.1))))

(deftest http-version-from-alpn-maps
  (ok (eq :http/2 (http-version-from-alpn "h2")))
  (ok (eq :http/1.1 (http-version-from-alpn "http/1.1")))
  (ok (null (http-version-from-alpn nil))))

(deftest effective-http-version-inherits
  (let* ((backend (make-instance 'http-backend :name "t"))
         (client (make-http-client backend :http-version :http/2))
         (req (make-http-request :url "https://ex.test/")))
    (ok (eq :http/2 (effective-http-version client req)))
    (setf (http-request-http-version req) :http/1.1)
    (ok (eq :http/1.1 (effective-http-version client req)))))

(deftest ensure-http-version-available-enforces
  (ok (eq :http/2 (ensure-http-version-available :http/2 :http/2)))
  (ok (eq :http/1.1 (ensure-http-version-available :auto :http/1.1)))
  (ok (signals (ensure-http-version-available :http/2 :http/1.1)
               'http-version-not-available)))
