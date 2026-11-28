(in-package #:http-protocol/tests)

(deftest http-file-roundtrip-multipart
  "Multiple http-file parts streamed into one multipart body."
  (let* ((f1 (make-http-file (make-octet-input-stream (coerce-to-octets "one"))
                             :filename "a.txt"
                             :content-type "text/plain"
                             :content-length 3
                             :field-name "photos"))
         (f2 (make-http-file (make-octet-input-stream (coerce-to-octets "two"))
                             :filename "b.txt"
                             :content-type "text/plain"
                             :content-length 3
                             :field-name "photos"))
         (req (make-http-request :url "http://x"
                                 :form-data '(("title" . "hi"))
                                 :files (list f1 f2))))
    (multiple-value-bind (stream extra clen)
        (prepare-request-body req)
      (ok (streamp stream))
      (ok (search "multipart/form-data"
                  (cdr (assoc "content-type" extra :test #'string-equal))))
      (ok (integerp clen))
      (let* ((octets (slurp-octets stream))
             (text (map 'string #'code-char octets)))
        (ok (= clen (length octets)))
        (ok (search "name=\"title\"" text))
        (ok (search "hi" text))
        (ok (search "name=\"photos\"" text))
        (ok (search "filename=\"a.txt\"" text))
        (ok (search "filename=\"b.txt\"" text))
        (ok (search "one" text))
        (ok (search "two" text))))))

(deftest http-file-alist-files
  (let* ((f (make-http-file (coerce-to-octets "x")
                            :filename "x.bin"
                            :content-length 1))
         (req (make-http-request :url "http://x"
                                 :files `(("upload" . ,f)))))
    (multiple-value-bind (stream extra clen)
        (prepare-request-body req)
      (declare (ignore extra))
      (ok (streamp stream))
      (ok (= clen (length (slurp-octets stream)))))))

(deftest http-file-as-content
  (let* ((f (make-http-file (coerce-to-octets "abc")
                            :content-type "text/plain"
                            :content-length 3))
         (req (make-http-request :url "http://x" :content f)))
    (multiple-value-bind (wire extra clen)
        (prepare-request-body req)
      (ok (equalp (coerce-to-octets "abc") wire))
      (ok (string= "text/plain"
                   (cdr (assoc "content-type" extra :test #'string-equal))))
      (ok (= 3 clen)))))

(deftest response-as-http-file-wraps
  (let* ((ht (make-hash-table :test #'equal))
         (_ (setf (gethash "content-type" ht) "image/png"
                  (gethash "content-length" ht) "4"
                  (gethash "content-disposition" ht)
                  "attachment; filename=\"x.png\""))
         (res (make-instance 'http-response
                             :status 200
                             :headers ht
                             :body (coerce-to-octets "PNG!")))
         (file (response-as-http-file res)))
    (declare (ignore _))
    (ok (http-file-p file))
    (ok (string= "x.png" (http-file-filename file)))
    (ok (string= "image/png" (http-file-content-type file)))
    (ok (= 4 (http-file-content-length file)))
    (ok (equalp (coerce-to-octets "PNG!")
                (slurp-octets (body-stream
                               (make-instance 'http-response
                                              :status 200
                                              :headers ht
                                              :body file)))))))

(deftest prepare-body-stream-content
  (with-open-stream (src (make-octet-input-stream (coerce-to-octets "abc")))
    (let ((req (make-http-request :url "http://x" :content src)))
      (multiple-value-bind (wire extra clen)
          (prepare-request-body req)
        (declare (ignore extra clen))
        (ok (streamp wire))
        (ok (equalp (coerce-to-octets "abc") (slurp-octets wire)))))))

;;; http-protocol#44: name / filename cannot inject; lengths are octets.

(defun %mp-text (req)
  "→ (values decoded-text octet-count content-length)."
  (multiple-value-bind (stream extra clen) (prepare-request-body req)
    (declare (ignore extra))
    (let ((octets (slurp-octets stream)))
      (values (encoding-protocol:decode octets :encoding :utf-8)
              (length octets)
              clen))))

(defun %header-lines (text)
  "Lines of TEXT split on CRLF."
  (loop with start = 0
        for pos = (search (format nil "~C~C" #\Return #\Newline) text :start2 start)
        collect (subseq text start (or pos (length text)))
        while pos
        do (setf start (+ pos 2))))

(deftest multipart-crlf-and-quotes-cannot-inject
  (let* ((evil-name (format nil "f\"; filename=\"x.exe~C~CX-Injected: 1" #\Return #\Newline))
         (evil-file (format nil "a.txt\"~C~CContent-Type: text/html~C~C~C~Cpwned"
                            #\Return #\Newline #\Return #\Newline #\Return #\Newline))
         (req (make-http-request :url "http://x"
                                 :form-data `((,evil-name . "v"))
                                 :files `(("upload" . (:content "data" :filename ,evil-file))))))
    (multiple-value-bind (text n clen) (%mp-text req)
      (let ((lines (%header-lines text)))
        (ok (= n clen) "Content-Length matches the octets")
        (ok (notany (lambda (l) (search "X-Injected" l :end2 (min 10 (length l)))) lines)
            "no forged header line")
        (ok (= 1 (count-if (lambda (l) (string= l "Content-Type: application/octet-stream")) lines))
            "only the real Content-Type line")
        (ok (notany (lambda (l) (string= l "Content-Type: text/html")) lines))
        (ok (notany (lambda (l) (string= l "pwned")) lines) "no forged body")
        (let ((disp (remove-if-not (lambda (l) (search "Content-Disposition:" l)) lines)))
          (ok (= 2 (length disp)) "one disposition per part")
          (ok (search "name=\"f%22; filename=%22x.exe%0D%0AX-Injected: 1\"" (first disp)))
          (ok (search "filename=\"a.txt%22%0D%0AContent-Type: text/html%0D%0A%0D%0Apwned\""
                      (second disp))))))))

(deftest multipart-content-type-crlf-refused
  (let ((req (make-http-request
              :url "http://x"
              :files `(("f" . (:content "x" :filename "a"
                               :content-type ,(format nil "text/plain~C~CX: y"
                                                      #\Return #\Newline)))))))
    (ok (signals (prepare-request-body req) 'http-protocol-error))))

(deftest multipart-non-ascii-filename
  (let* ((name "résumé – 履歴書.pdf")
         (req (make-http-request :url "http://x"
                                 :form-data '(("título" . "ñ"))
                                 :files `(("file" . (:content "%PDF" :filename ,name
                                                     :content-type "application/pdf"))))))
    (multiple-value-bind (text n clen) (%mp-text req)
      (ok (integerp clen))
      (ok (= n clen) "Content-Length counts UTF-8 octets, not characters")
      (ok (> n (length text)) "fixture really is multi-byte")
      (ok (search (format nil "filename=\"~A\"" name) text) "UTF-8 filename (RFC 7578 4.2)")
      ;; mime-protocol 0.3.6 (mime-protocol#21): RFC 7578 4.2 has no filename*
      ;; in form-data, and a differing one is refused on parse.
      (ng (search "filename*" text) "no filename*, as browsers send it")
      (ok (search "name=\"título\"" text)))))

(defun %crlf ()
  (format nil "~C~C" #\Return #\Newline))

(defun %octet-vector (&rest octets)
  (make-array (length octets) :element-type '(unsigned-byte 8) :initial-contents octets))

(defun %multipart-round-trip (data files &rest keys)
  "MAKE-MULTIPART-BODY of DATA and FILES, parsed back with mime-protocol →
   (values part-contents boundary content-type)."
  (multiple-value-bind (stream ct clen boundary)
      (apply #'make-multipart-body data files keys)
    (let* ((octets (slurp-octets stream))
           (entity (mime-protocol:parse-mime
                    (concatenate '(vector (unsigned-byte 8))
                                 (coerce-to-octets
                                  (format nil "Content-Type: ~A~A~A" ct (%crlf) (%crlf)))
                                 octets)
                    :decode nil)))
      (ok (eql clen (length octets)) "Content-Length matches the octets")
      (values (mapcar #'mime-protocol:mime-content (mime-protocol:mime-parts entity))
              boundary
              ct))))

(defun %bchar-p (char)
  "RFC 2046 bcharsnospace."
  (or (char<= #\0 char #\9) (char<= #\a char #\z) (char<= #\A char #\Z)
      (find char "'()+_,-./:=?")))

(deftest multipart-boundary-not-from-global-random-state
  "Every fresh SBCL image starts with the same CL:*RANDOM-STATE*: a boundary
   drawn from it would be the same string in each process."
  (let ((seed (make-random-state nil)))
    (flet ((draw ()
             (let ((*random-state* (make-random-state seed)))
               (http-protocol::make-multipart-boundary))))
      (ok (string/= (draw) (draw)) "same CL:*RANDOM-STATE*, different boundaries"))
    (let* ((*random-state* (make-random-state seed))
           (before (make-random-state *random-state*)))
      (http-protocol::make-multipart-boundary)
      (ok (= (random (expt 2 62) before) (random (expt 2 62) *random-state*))
          "drawing a boundary leaves CL:*RANDOM-STATE* alone"))))

(deftest multipart-boundary-fresh-image-seeding
  "A fresh image has no boundary state yet (start-up, or a saved image whose
   dump hook cleared it): it seeds its own, so two fresh images get different
   first boundaries even under the same CL:*RANDOM-STATE*."
  (let ((seed (make-random-state nil)))
    (flet ((first-boundary ()
             (let ((http-protocol::*boundary-random-state* nil)
                   (*random-state* (make-random-state seed)))
               (http-protocol::make-multipart-boundary))))
      (ok (string/= (first-boundary) (first-boundary)))))
  (ok (member 'http-protocol::%reset-boundary-random-state
              #+sbcl sb-ext:*save-hooks* #-sbcl uiop:*image-dump-hook*)
      "a saved image does not keep the state it was saved with"))

(deftest multipart-boundary-format
  "RFC 2046 5.1.1: 1-70 bchars; at least 128 random bits (32 hex digits)."
  (let* ((boundaries (loop repeat 200 collect (http-protocol::make-multipart-boundary)))
         (prefix "----cl-stack-"))
    (ok (every (lambda (b) (<= 1 (length b) 70)) boundaries) "at most 70 characters")
    (ok (every (lambda (b) (every #'%bchar-p b)) boundaries) "bchars only, no space")
    (ok (every (lambda (b)
                 (and (eql 0 (search prefix b))
                      (<= 32 (- (length b) (length prefix)))
                      (every (lambda (c) (digit-char-p c 16)) (subseq b (length prefix)))))
               boundaries)
        "32 or more random hex digits")
    (ok (= 200 (length (remove-duplicates boundaries :test #'string=))) "all distinct")))

(deftest multipart-boundary-collision-picks-another
  "RFC 2046 5.1.1: a boundary that occurs in a part is not used."
  (let* ((fixed "----cl-stack-fixed")
         (evil (format nil "a~A--~A~AContent-Type: text/html~A~Apwned"
                       (%crlf) fixed (%crlf) (%crlf) (%crlf)))
         (octets (coerce-to-octets (format nil "x~A--~A--~A" (%crlf) fixed (%crlf)))))
    (multiple-value-bind (contents boundary ct)
        (%multipart-round-trip `(("title" . "hi"))
                               `(("text" . (:content ,evil :filename "a.txt"))
                                 ("bin" . (:content ,octets :filename "b.bin")))
                               :boundary fixed)
      (ok (string/= fixed boundary) "a new boundary")
      (ok (search (format nil "boundary=~A" boundary) ct) "Content-Type names it")
      (ok (= 3 (length contents)) "three parts, none forged")
      (ok (equalp (coerce-to-octets "hi") (first contents)))
      (ok (equalp (coerce-to-octets evil) (second contents)) "string part intact")
      (ok (equalp octets (third contents)) "octet part intact"))
    (multiple-value-bind (contents boundary)
        (%multipart-round-trip `(("f" . ,(format nil "--~A" fixed))) nil :boundary fixed)
      (ok (string/= fixed boundary) "a form field is checked too")
      (ok (equalp (coerce-to-octets (format nil "--~A" fixed)) (first contents))))
    (multiple-value-bind (contents boundary)
        (%multipart-round-trip `(("f" . "plain")) nil :boundary fixed)
      (ok (string= fixed boundary) "a boundary that does not occur is kept")
      (ok (equalp (coerce-to-octets "plain") (first contents))))))

(deftest multipart-generated-boundary-collision
  "When the drawn boundary occurs in a part, another one is drawn."
  (let ((global (make-random-state nil))
        (state (make-random-state t)))
    (flet ((call-fresh (fn)
             (let ((*random-state* (make-random-state global))
                   (http-protocol::*boundary-random-state* (make-random-state state)))
               (funcall fn))))
      (let* ((drawn (call-fresh #'http-protocol::make-multipart-boundary))
             (content (format nil "x~A--~A~Ay" (%crlf) drawn (%crlf))))
        (multiple-value-bind (contents boundary)
            (call-fresh (lambda ()
                          (%multipart-round-trip
                           nil `(("f" . (:content ,content :filename "a"))))))
          (ok (string/= drawn boundary) "the colliding draw is skipped")
          (ok (equalp (list (coerce-to-octets content)) contents)))))))

(deftest multipart-boundary-must-be-rfc-2046
  "A caller's :BOUNDARY must be 1-70 bchars, not ending in a space."
  (dolist (bad (list "" (make-string 71 :initial-element #\a) "ends-in-space "
                     (format nil "a~Cb" #\Return) "semi;colon" "quo\"te" "ü"))
    (ok (signals (make-multipart-body '(("a" . "b")) nil :boundary bad)
          'http-protocol-error)
        (format nil "~S refused" bad)))
  (let ((special "it's (a) b+c_d,e-f./g:h=i?j k"))
    (multiple-value-bind (contents boundary ct)
        (%multipart-round-trip '(("a" . "b")) nil :boundary special)
      (ok (string= special boundary))
      (ok (search (format nil "boundary=\"~A\"" special) ct) "quoted in Content-Type")
      (ok (equalp (list (coerce-to-octets "b")) contents)))))

(defun %bad-contents ()
  (list (vector 1 2)
        (make-array 2 :element-type '(signed-byte 8) :initial-element -1)
        (make-array 2 :element-type 'fixnum :initial-element 300)
        (vector #\a #\b)))

(deftest http-file-content-must-be-octets
  (dolist (bad (append (%bad-contents) (list 42 :symbol #p"/tmp/x")))
    (ok (signals (make-http-file bad :filename "a") 'http-protocol-error)
        (format nil "~S refused" bad)))
  (ok (http-file-p (make-http-file (%octet-vector 1 2))))
  (ok (http-file-p (make-http-file (make-array 2 :element-type '(unsigned-byte 8)
                                                 :adjustable t :fill-pointer 1))))
  (ok (http-file-p (make-http-file "text")))
  (ok (http-file-p (make-http-file (make-octet-input-stream (%octet-vector 1))))))

(deftest multipart-part-content-must-be-octets
  (dolist (bad (%bad-contents))
    (ok (signals (build-multipart-parts nil `(("f" . ,bad))) 'http-protocol-error)
        "a raw :files value")
    (ok (signals (build-multipart-parts nil `(("f" . (:content ,bad :filename "a"))))
          'http-protocol-error)
        "a :files plist")
    (ok (signals (build-multipart-parts `(("field" . ,bad)) nil) 'http-protocol-error)
        "a form field")
    (ok (signals (make-multipart-body nil `(("f" . ,bad))) 'http-protocol-error)
        "MAKE-MULTIPART-BODY")
    (ok (signals (prepare-request-body
                  (make-http-request :url "http://x" :files `(("f" . ,bad))))
          'http-protocol-error)
        "PREPARE-REQUEST-BODY"))
  (ok (signals (http-protocol::%part-body-length (list :content (vector 1 2))) 'type-error)
      "%PART-BODY-LENGTH has no case for a non-octet vector"))

(deftest multipart-string-content-is-utf-8
  "A string part is sent as UTF-8, as before."
  (let ((text "ñ€"))
    (multiple-value-bind (contents)
        (%multipart-round-trip `(("t" . ,text))
                               `(("f" . ,(make-http-file text :filename "u.txt"))
                                 ("g" . ,text)))
      (ok (equalp (list (%octet-vector 195 177 226 130 172)
                        (%octet-vector 195 177 226 130 172)
                        (%octet-vector 195 177 226 130 172))
                  contents)))))

(deftest response-as-http-file-empty-body
  "No body → an empty octet vector, which MAKE-HTTP-FILE accepts."
  (let ((file (response-as-http-file
               (make-instance 'http-response :status 204
                                             :headers (make-hash-table :test #'equal)
                                             :body nil))))
    (ok (http-file-p file))
    (ok (typep (http-file-content file) '(vector (unsigned-byte 8))))
    (ok (zerop (length (http-file-content file))))))
