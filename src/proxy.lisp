(in-package #:http-protocol)

;;; Proxy configuration (CLOS). Logic ported from egao1980/dexador
;;; feature/proxy-env-support (PR fukamachi/dexador#202): env seeding,
;;; scheme/host alist, NO_PROXY with IPv4/IPv6/CIDR + hostname suffix.

(defun getenv-nonempty (&rest names)
  (loop for name in names
        thereis (let ((v (uiop:getenv name)))
                  (and v (plusp (length v)) v))))

(defun make-environment-proxy-alist (https http all)
  (unless all
    (cond
      ((and https (null http)) (setf http https))
      ((and http (null https)) (setf https http))))
  (remove nil (list (and https (cons "https" https))
                    (and http (cons "http" http))
                    (and all (cons "*" all)))))

(defun environment-proxy-alist ()
  (make-environment-proxy-alist
   (getenv-nonempty "https_proxy" "HTTPS_PROXY")
   (getenv-nonempty "http_proxy" "HTTP_PROXY")
   (getenv-nonempty "all_proxy" "ALL_PROXY")))

(defun environment-no-proxy ()
  (getenv-nonempty "no_proxy" "NO_PROXY"))

(defclass http-proxy-config ()
  ((proxy :initarg :proxy :accessor proxy-config-proxy
          :initform nil
          :documentation
          "Manual static proxy:
             NIL
             | URL string (single hop)
             | list of URL strings (proxy chain for every scheme)
             | scheme/host alist whose values are a URL or a list of URLs (chain).
           Example chain: (\"socks5h://127.0.0.1:9050\" \"http://corp:8080\").")
   (no-proxy :initarg :no-proxy :accessor proxy-config-no-proxy
             :initform nil
             :documentation
             "NO_PROXY: comma/space string or list of globs/patterns
              (\"*\", \"*.corp\", \".example.com\", CIDR, IP). See HOST-BYPASSED-P.")
   (use-system-proxy :initarg :use-system-proxy :accessor proxy-config-use-system-proxy
                     :initform t
                     :documentation "Allow LOAD-PROXY-SYSTEM when resolving.")
   (system-automatic-p :initarg :system-automatic-p
                       :accessor proxy-config-system-automatic-p
                       :initform nil
                       :documentation
                       "When T and no static PROXY/script hit, RESOLVE-PROXY → :SYSTEM
                        (backend uses OS resolution — WinHTTP AUTOMATIC).")
   (script-url :initarg :script-url :accessor proxy-config-script-url :initform nil
               :documentation "Manual PAC URL (CONFIGURE-PROXY-SCRIPT), not system WPAD.")
   (script-text :initarg :script-text :accessor proxy-config-script-text :initform nil
                :documentation "Cached manual PAC body."))
  (:documentation
   "Proxy policy. Populate via CONFIGURE-PROXY / CONFIGURE-PROXY-SCRIPT /
    LOAD-PROXY-SYSTEM, then resolve with RESOLVE-PROXY-CHAIN / PROXY-NEXT-HOP
    (methods on this class)."))

(defun http-proxy-config-p (x) (typep x 'http-proxy-config))

(defun make-http-proxy-config (&key (proxy nil proxy-p)
                                 (no-proxy nil no-proxy-p)
                                 (use-system-proxy t)
                                 (system-automatic-p nil)
                                 (script-url nil)
                                 (script-text nil)
                                 (system t))
  "Build HTTP-PROXY-CONFIG.
   :PROXY / :NO-PROXY → manual. :SYSTEM T (default when no manual proxy) → LOAD-PROXY-SYSTEM."
  (let ((cfg (make-instance 'http-proxy-config
                            :proxy (if proxy-p proxy nil)
                            :no-proxy (if no-proxy-p no-proxy nil)
                            :use-system-proxy use-system-proxy
                            :system-automatic-p system-automatic-p
                            :script-url script-url
                            :script-text script-text)))
    (when (and system (not proxy-p) (null script-url) (null script-text))
      (load-proxy-system cfg))
    (when (and proxy-p (not no-proxy-p) system)
      ;; Manual proxy still inherits system no_proxy when omitted.
      (setf (proxy-config-no-proxy cfg)
            (or (proxy-config-no-proxy cfg) (environment-no-proxy))))
    cfg))

(defvar *default-proxy-config* nil
  "Default proxy config. Lazily LOAD-PROXY-SYSTEM.")

(defun ensure-default-proxy-config ()
  (or *default-proxy-config*
      (setf *default-proxy-config*
            (load-proxy-system
             (make-http-proxy-config :system nil)))))

(defun strip-ipv6-brackets (host)
  "Strip RFC 2732 brackets: \"[::1]\" → \"::1\"."
  (if (and (stringp host) (plusp (length host)) (char= (char host 0) #\[))
      (let ((close (position #\] host)))
        (if close (subseq host 1 close) host))
      host))

(defun format-host-port (host port)
  "HOST:PORT with IPv6 hosts in RFC 2732 brackets."
  (let ((host (strip-ipv6-brackets host)))
    (if (find #\: host)
        (format nil "[~A]:~A" host port)
        (format nil "~A:~A" host port))))

(defun %parse-ipv6-bytes (string)
  "Expand IPv6 literal → 16-byte vector, or NIL."
  (let* ((s (strip-ipv6-brackets string))
         (dbl (search "::" s)))
    (labels ((hextets (part)
               (if (zerop (length part))
                   nil
                   (mapcar (lambda (h)
                             (and (<= 1 (length h) 4)
                                  (every (lambda (c)
                                           (digit-char-p c 16))
                                         h)
                                  (parse-integer h :radix 16)))
                           (uiop:split-string part :separator ":"))))
             (ok (list) (and list (every #'integerp list))))
      (multiple-value-bind (left right)
          (if dbl
              (values (hextets (subseq s 0 dbl))
                      (hextets (subseq s (+ dbl 2))))
              (values (hextets s) nil))
        (when (and (or (null left) (ok left))
                   (or (null dbl) (null right) (ok right)))
          (let* ((left (or left '()))
                 (right (or right '()))
                 (n (+ (length left) (length right)))
                 (fill (- 8 n)))
            (when (and (<= 0 fill) (or dbl (zerop fill)) (<= n 8))
              (let ((out (make-array 16 :element-type '(unsigned-byte 8)))
                    (pos 0))
                (flet ((put (v)
                         (setf (aref out pos) (ldb (byte 8 8) v)
                               (aref out (1+ pos)) (ldb (byte 8 0) v))
                         (incf pos 2)))
                  (dolist (v left) (put v))
                  (dotimes (_ fill) (put 0))
                  (dolist (v right) (put v)))
                out))))))))

(defun parse-ip-address (string)
  "Parse IPv4/IPv6 literal → (values address-integer total-bits) or NIL."
  (let ((string (strip-ipv6-brackets string)))
    (if (find #\: string)
        (let ((bytes (%parse-ipv6-bytes string)))
          (when bytes
            (values (reduce (lambda (acc byte) (logior (ash acc 8) byte))
                            bytes :initial-value 0)
                    128)))
        (let ((parts (uiop:split-string string :separator ".")))
          (when (= (length parts) 4)
            (loop with address = 0
                  for part in parts
                  for byte = (and (<= 1 (length part) 3)
                                  (every #'digit-char-p part)
                                  (parse-integer part))
                  unless (and byte (<= byte 255))
                    do (return nil)
                  do (setf address (logior (ash address 8) byte))
                  finally (return (values address 32))))))))

(defun ip-in-network-p (address total-bits pattern)
  (let ((slash (position #\/ pattern)))
    (when slash
      (multiple-value-bind (network network-bits)
          (parse-ip-address (subseq pattern 0 slash))
        (let ((prefix (ignore-errors (parse-integer (subseq pattern (1+ slash))))))
          (and network
               (eql total-bits network-bits)
               (integerp prefix)
               (<= 0 prefix network-bits)
               (let ((shift (- total-bits prefix)))
                 (= (ash address (- shift))
                    (ash network (- shift))))))))))

(defun ip-matches-pattern-p (address total-bits pattern)
  (if (find #\/ pattern)
      (ip-in-network-p address total-bits pattern)
      (multiple-value-bind (pattern-address pattern-bits)
          (parse-ip-address
           (let ((colon (position #\: pattern)))
             ;; Single colon → IPv4:port; multiple → leave as IPv6.
             (if (and colon (not (find #\: pattern :start (1+ colon))))
                 (subseq pattern 0 colon)
                 pattern)))
        (and pattern-address
             (eql total-bits pattern-bits)
             (= address pattern-address)))))

(defun normalize-no-proxy (no-proxy)
  "Normalize NO-PROXY to a list of pattern strings.
   Accepts NIL, a comma/space-separated string, or a list of globs/patterns
   (dexador / curl NO_PROXY shape)."
  (cond
    ((null no-proxy) nil)
    ((listp no-proxy)
     (remove "" (mapcar (lambda (s) (string-trim '(#\Space #\Tab) (string s)))
                        no-proxy)
             :test #'string=))
    ((stringp no-proxy)
     (remove "" (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                        (uiop:split-string no-proxy :separator ", "))
             :test #'string=))
    (t (list (princ-to-string no-proxy)))))

(defun hostname-glob-match-p (host pattern)
  "Case-insensitive glob match: * = any run of chars, ? = one char."
  (let ((host (string-downcase host))
        (pattern (string-downcase pattern)))
    (labels ((rec (hi pat-i)
               (cond
                 ((and (>= hi (length host)) (>= pat-i (length pattern))) t)
                 ((>= pat-i (length pattern)) nil)
                 ((char= (char pattern pat-i) #\*)
                  (loop for i from hi to (length host)
                        thereis (rec i (1+ pat-i))))
                 ((>= hi (length host)) nil)
                 ((or (char= (char pattern pat-i) #\?)
                      (char= (char host hi) (char pattern pat-i)))
                  (rec (1+ hi) (1+ pat-i)))
                 (t nil))))
      (rec 0 0))))

(defun hostname-matches-pattern-p (host pattern)
  "Match HOST against a NO_PROXY hostname pattern (dexador#202 + globs).

   - \"*\" → all hosts
   - patterns with * or ? → glob (e.g. \"*.example.com\")
   - otherwise exact match or domain suffix (\"example.com\" / \".example.com\"
     matches \"api.example.com\"; suffix must align on a dot)
   - trailing :port in PATTERN is ignored"
  (let* ((pattern (string-trim '(#\Space #\Tab) pattern))
         ;; Hostname patterns are not IPv6 — first ':' starts a port suffix.
         (colon (position #\: pattern))
         (pattern (if colon (subseq pattern 0 colon) pattern)))
    (cond
      ((string= pattern "*") t)
      ((or (find #\* pattern) (find #\? pattern))
       (hostname-glob-match-p host pattern))
      (t
       (let ((pattern (string-left-trim "." pattern)))
         (or (string-equal host pattern)
             (let ((pl (length pattern))
                   (hl (length host)))
               (and (plusp pl)
                    (> hl pl)
                    (char= (char host (- hl pl 1)) #\.)
                    (string-equal pattern (subseq host (- hl pl)))))))))))

(defun host-bypassed-p (host no-proxy)
  "True if HOST matches NO-PROXY (string or list of globs/patterns).

   Patterns (per entry): \"*\", hostname globs (*.corp), domain suffix,
   IP literal, or CIDR (10.0.0.0/8, fd00::/8). List form preferred."
  (when (and host no-proxy)
    (let ((host (strip-ipv6-brackets host))
          (patterns (normalize-no-proxy no-proxy)))
      (multiple-value-bind (address total-bits) (parse-ip-address host)
        (some (lambda (pattern)
                (and (plusp (length pattern))
                     (or (string= pattern "*")
                         (if address
                             (ip-matches-pattern-p address total-bits pattern)
                             (hostname-matches-pattern-p host pattern)))))
              patterns)))))

(defun proxy-chain-p (x)
  "True if X is a non-empty list of proxy URL strings (a hop chain), not an alist."
  (and (consp x) (stringp (car x)) (every #'stringp x)))

(defun %as-proxy-chain (value)
  "Normalize a proxy value to a list of URL strings (possibly empty)."
  (cond
    ((null value) nil)
    ((stringp value) (list value))
    ((proxy-chain-p value) (copy-list value))
    (t (list (princ-to-string value)))))

(defun normalize-proxy (proxy)
  "Normalize PROXY to alist of key → chain (list of URL strings).
   String → ((\"*\" . (url))). Bare chain list → ((\"*\" . chain)).
   Alist values coerced to chains."
  (cond
    ((null proxy) nil)
    ((stringp proxy) (list (cons "*" (list proxy))))
    ((proxy-chain-p proxy) (list (cons "*" (copy-list proxy))))
    ((listp proxy)
     (mapcar (lambda (cell)
               (cons (car cell) (%as-proxy-chain (cdr cell))))
             proxy))
    (t (list (cons "*" (%as-proxy-chain proxy))))))

(defun proxy-chain-for-target (scheme host proxy-alist)
  "Most specific key wins: scheme://host, scheme, then \"*\"/\"all\".
   Returns a list of proxy URL strings, or NIL."
  (let* ((scheme (and scheme (string-downcase scheme)))
         (host (and host (strip-ipv6-brackets host)))
         (host-key (and scheme host (format nil "~A://~A" scheme host)))
         (bracketed-key (and scheme host (find #\: host)
                             (format nil "~A://[~A]" scheme host))))
    (copy-list
     (cdr (or (and host-key (assoc host-key proxy-alist :test #'string-equal))
              (and bracketed-key (assoc bracketed-key proxy-alist :test #'string-equal))
              (and scheme (assoc scheme proxy-alist :test #'string-equal))
              (assoc "*" proxy-alist :test #'string-equal)
              (assoc "all" proxy-alist :test #'string-equal))))))

(defun proxy-for-uri (uri proxy-alist)
  "First hop URL for URI from PROXY-ALIST (compat). Prefer PROXY-CHAIN-FOR-TARGET."
  (let* ((u (if (typep uri 'quri:uri) uri (quri:uri uri)))
         (chain (proxy-chain-for-target (quri:uri-scheme u) (quri:uri-host u)
                                        (normalize-proxy proxy-alist))))
    (first chain)))

(defun proxy-url-hop-pair (proxy-url)
  "PROXY-URL string → (values (SCHEME . HOST) PORT URL)."
  (multiple-value-bind (scheme host port)
      (parse-proxy-uri proxy-url)
    (values (cons scheme host) port proxy-url)))

(defun %hop-pair-equal (a b)
  "Compare (scheme . host) pairs case-insensitively."
  (and (consp a) (consp b)
       (string-equal (car a) (car b))
       (string-equal (cdr a) (cdr b))))

;;; ---------------------------------------------------------------------------
;;; Three configuration modes:
;;;   1. CONFIGURE-PROXY        — manual URL / alist
;;;   2. CONFIGURE-PROXY-SCRIPT — manual PAC (user-supplied)
;;;   3. LOAD-PROXY-SYSTEM      — env vars + Windows registry + OS PAC/WPAD
;;; ---------------------------------------------------------------------------

(defgeneric configure-proxy (config &key proxy no-proxy)
  (:documentation
   "1. Manual static proxy. PROXY = URL | chain (list of URLs) | scheme/host alist
    (alist values may themselves be chains). NO-PROXY optional.
    Clears SYSTEM-AUTOMATIC-P / manual script slots.")
  (:method ((config http-proxy-config) &key (proxy nil proxy-p)
                                         (no-proxy nil no-proxy-p))
    (when proxy-p
      (setf (proxy-config-proxy config) proxy
            (proxy-config-system-automatic-p config) nil
            (proxy-config-script-url config) nil
            (proxy-config-script-text config) nil))
    (when no-proxy-p
      (setf (proxy-config-no-proxy config) no-proxy))
    config))

(defgeneric configure-proxy-script (config &key url text fetch)
  (:documentation
   "2. Manual PAC script (user-supplied, not OS WPAD).

    Provide :TEXT, or :URL with :FETCH (lambda (url) → string) — typically
    usocket/async GET. Clears static PROXY; evaluation via EVALUATE-PROXY-SCRIPT.")
  (:method ((config http-proxy-config) &key url text fetch)
    (setf (proxy-config-proxy config) nil
          (proxy-config-system-automatic-p config) nil)
    (when url
      (setf (proxy-config-script-url config) url))
    (cond
      (text
       (setf (proxy-config-script-text config) text))
      ((or url (proxy-config-script-url config))
       (let ((u (or url (proxy-config-script-url config))))
         (unless fetch
           (error 'unsupported-operation
                  :operation 'configure-proxy-script
                  :message "CONFIGURE-PROXY-SCRIPT with :URL needs :FETCH or :TEXT"))
         (setf (proxy-config-script-url config) u
               (proxy-config-script-text config) (funcall fetch u))))
      (t
       (error 'http-protocol-error
              :message "CONFIGURE-PROXY-SCRIPT needs :TEXT or :URL")))
    config))

(defgeneric load-proxy-system (config &key fetch)
  (:documentation
   "3. System proxy discovery (all OS-provided sources):
      - unix-like env: https_proxy / http_proxy / all_proxy / no_proxy
      - Windows registry / WinINet (specialize — or set SYSTEM-AUTOMATIC-P)
      - OS PAC / WPAD (specialize; optional :FETCH for script download)

    Does not override an already-set manual PROXY.
    Only runs when PROXY-CONFIG-USE-SYSTEM-PROXY is true.")
  (:method ((config http-proxy-config) &key fetch)
    (declare (ignore fetch))
    (unless (proxy-config-use-system-proxy config)
      (return-from load-proxy-system config))
    ;; Env is portable "system" config (same class as registry on Windows).
    (unless (proxy-config-proxy config)
      (setf (proxy-config-proxy config) (environment-proxy-alist)))
    (unless (proxy-config-no-proxy config)
      (setf (proxy-config-no-proxy config) (environment-no-proxy)))
    ;; Platform hook: Windows specialization fills registry / sets
    ;; SYSTEM-AUTOMATIC-P / SCRIPT-URL from WinINet. Default: done.
    (load-proxy-system-platform config)
    config))

(defgeneric load-proxy-system-platform (config &key)
  (:documentation
   "Platform part of LOAD-PROXY-SYSTEM (registry / WPAD). Default no-op.
    Specialize on Windows to set SYSTEM-AUTOMATIC-P or concrete PROXY.")
  (:method ((config http-proxy-config) &key)
    (declare (ignore config))
    nil))

(defgeneric evaluate-proxy-script (config uri &key script)
  (:documentation
   "Evaluate PAC FindProxyForURL for URI → proxy URL string or NIL (DIRECT).

    Used for manual scripts (CONFIGURE-PROXY-SCRIPT). Default: unsupported —
    on Windows prefer LOAD-PROXY-SYSTEM + SYSTEM-AUTOMATIC-P.")
  (:method ((config http-proxy-config) uri &key script)
    (declare (ignore uri script))
    (error 'unsupported-operation
           :operation 'evaluate-proxy-script
           :message "PAC evaluation not implemented; use system automatic proxy or a static proxy")))

(defgeneric coerce-proxy-config (x)
  (:documentation "Normalize X → HTTP-PROXY-CONFIG.")
  (:method ((x http-proxy-config)) x)
  (:method ((x null))
    (make-http-proxy-config :proxy nil :no-proxy nil :system nil))
  (:method ((x string))
    (make-http-proxy-config
     :proxy x
     :no-proxy (proxy-config-no-proxy (ensure-default-proxy-config))
     :system nil))
  (:method ((x list))
    (make-http-proxy-config
     :proxy x
     :no-proxy (proxy-config-no-proxy (ensure-default-proxy-config))
     :system nil)))

(defgeneric resolve-proxy-chain (config scheme host &key port uri)
  (:documentation
   "Method on HTTP-PROXY-CONFIG: ordered proxy hop URLs toward SCHEME://HOST.

    Returns:
      - NIL or () — direct (no proxy; also NO_PROXY hit)
      - list of proxy URL strings — chain (nearest hop first)
      - (:SYSTEM) — OS automatic; backend must honor

    URI, when supplied, is used for PAC evaluation; SCHEME/HOST are authoritative
    for alist lookup and NO_PROXY.")
  (:method ((config http-proxy-config) scheme host &key port uri)
    (declare (ignore port))
    (let* ((host (and host (strip-ipv6-brackets host)))
           (u (cond (uri (if (typep uri 'quri:uri) uri (quri:uri uri)))
                    ((and scheme host)
                     (quri:make-uri :scheme scheme :host host :path "/"))
                    (t nil))))
      (when (host-bypassed-p host (proxy-config-no-proxy config))
        (return-from resolve-proxy-chain nil))
      (let ((chain (proxy-chain-for-target
                    scheme host
                    (normalize-proxy (proxy-config-proxy config)))))
        (when chain
          (return-from resolve-proxy-chain chain)))
      (when (and u
                 (proxy-config-script-text config)
                 (proxy-config-script-url config))
        (let ((pac (evaluate-proxy-script
                    config u :script (proxy-config-script-text config))))
          (when pac
            (return-from resolve-proxy-chain (%as-proxy-chain pac)))))
      (when (and (proxy-config-use-system-proxy config)
                 (proxy-config-system-automatic-p config))
        (return-from resolve-proxy-chain (list :system)))
      nil)))

(defgeneric proxy-next-hop (config scheme host &key port after uri)
  (:documentation
   "Method on HTTP-PROXY-CONFIG: next hop toward request SCHEME/HOST as
    (PROXY-SCHEME . PROXY-HOST).

    Secondary values: PORT, PROXY-URL.
    AFTER = NIL (default) → first hop; or a previous (scheme . host) / URL
    to advance along a chain. NIL return = connect to HOST (direct / end of
    chain). Primary value :SYSTEM means OS automatic (no pair).")
  (:method ((config http-proxy-config) scheme host &key port after uri)
    (let ((chain (resolve-proxy-chain config scheme host :port port :uri uri)))
      (cond
        ((null chain) nil)
        ((equal chain '(:system))
         (if (or (null after) (eq after :system))
             (values :system nil nil)
             nil))
        (t
         (let* ((hops (mapcar (lambda (url)
                                (multiple-value-bind (pair pport purl)
                                    (proxy-url-hop-pair url)
                                  (list pair pport purl)))
                              chain))
                (start (cond
                         ((null after) 0)
                         ((stringp after)
                          (let ((p (position after chain :test #'string-equal)))
                            (and p (1+ p))))
                         ((consp after)
                          (let ((p (position-if
                                    (lambda (entry)
                                      (%hop-pair-equal (first entry) after))
                                    hops)))
                            (and p (1+ p))))
                         (t nil))))
           (when (and start (< start (length hops)))
             (destructuring-bind (pair pport purl) (nth start hops)
               (values pair pport purl)))))))))

(defgeneric resolve-proxy (config uri)
  (:documentation
   "Convenience on HTTP-PROXY-CONFIG: first hop for URI.
      string → proxy URL | NIL → direct | :SYSTEM → OS automatic.
    Prefer RESOLVE-PROXY-CHAIN / PROXY-NEXT-HOP for chaining.")
  (:method ((config http-proxy-config) uri)
    (let* ((u (if (typep uri 'quri:uri) uri (quri:uri uri)))
           (chain (resolve-proxy-chain config
                                       (quri:uri-scheme u)
                                       (quri:uri-host u)
                                       :port (quri:uri-port u)
                                       :uri u)))
      (cond
        ((null chain) nil)
        ((equal chain '(:system)) :system)
        (t (first chain)))))
  ;; Coerce non-config values so call sites can pass raw :proxy slots.
  (:method ((proxy string) uri)
    (resolve-proxy (coerce-proxy-config proxy) uri))
  (:method ((proxy list) uri)
    (resolve-proxy (coerce-proxy-config proxy) uri))
  (:method ((proxy null) uri)
    (declare (ignore uri))
    nil))

(defun effective-proxy-config (request client)
  "Request :PROXY overrides client; else ENSURE-DEFAULT-PROXY-CONFIG.
   Always returns an HTTP-PROXY-CONFIG — call RESOLVE-PROXY-CHAIN on it."
  (coerce-proxy-config
   (or (and request (http-request-proxy request))
       (and client (http-client-proxy client))
       (ensure-default-proxy-config))))

(defun normalize-proxy-scheme (scheme)
  "Canonical lowercase scheme string."
  (string-downcase (or scheme "http")))

(defun socks-proxy-scheme-p (scheme)
  "True for socks / socks4 / socks4a / socks5 / socks5h."
  (member (normalize-proxy-scheme scheme)
          '("socks" "socks4" "socks4a" "socks5" "socks5h")
          :test #'string=))

(defun http-proxy-scheme-p (scheme)
  (member (normalize-proxy-scheme scheme) '("http" "https") :test #'string=))

(defun proxy-kind (proxy-url)
  "Classify PROXY-URL → :HTTP | :SOCKS5 | :SOCKS4 | :SYSTEM | NIL."
  (cond
    ((eq proxy-url :system) :system)
    ((null proxy-url) nil)
    (t
     (let ((s (normalize-proxy-scheme
               (quri:uri-scheme (quri:uri (if (stringp proxy-url)
                                              proxy-url
                                              (princ-to-string proxy-url)))))))
       (cond
         ((member s '("socks" "socks5" "socks5h") :test #'string=) :socks5)
         ((member s '("socks4" "socks4a") :test #'string=) :socks4)
         ((http-proxy-scheme-p s) :http)
         (t :http))))))

(defun socks-remote-dns-p (scheme)
  "True when the proxy should resolve the hostname (socks5h / socks4a)."
  (member (normalize-proxy-scheme scheme) '("socks5h" "socks4a" "socks")
          :test #'string=))

(defun parse-proxy-uri (proxy-url)
  "Return (values scheme host port user password) for a proxy URL string.
   Schemes: http(s), socks / socks5 / socks5h (default port 1080),
   socks4 / socks4a (default 1080)."
  (let* ((u (quri:uri proxy-url))
         (scheme (normalize-proxy-scheme (or (quri:uri-scheme u) "http")))
         (host (strip-ipv6-brackets (quri:uri-host u)))
         (port (or (quri:uri-port u)
                   (cond ((string-equal scheme "https") 443)
                         ((socks-proxy-scheme-p scheme) 1080)
                         (t 80))))
         (user (quri:uri-userinfo u))
         (pass nil))
    (when user
      (let ((colon (position #\: user)))
        (if colon
            (setf pass (subseq user (1+ colon))
                  user (subseq user 0 colon))
            (setf pass nil))))
    (values scheme host port user pass)))
