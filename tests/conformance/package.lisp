(defpackage #:http-protocol/conformance
  (:use #:cl #:rove #:http-protocol)
  (:export #:*test-codings*
           #:run-for-codings))

(in-package #:http-protocol/conformance)

(defvar *test-codings* '()
  "Coding keywords the loaded backend under test implements.
   Backend /tests sets this before (rove:run …).")

(defun run-for-codings (codings)
  "Set *TEST-CODINGS* and run this conformance system."
  (setf *test-codings* codings)
  (rove:run (asdf:find-system "http-protocol/conformance")))
