(in-package #:http-protocol)

;;; Windows IE/WinINet Internet Settings (dexador#202).
;;; Precedence (same as resolve-system-proxy): environment > registry / PAC.
;;; Never clobber proxy/no-proxy already seeded from env.

(defparameter +windows-inet-settings-key+
  "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings")

(defun %windows-p ()
  #+(or win32 windows mswindows) t
  #-(or win32 windows mswindows) nil)

(defun parse-windows-proxy-server (value)
  "Parse IE ProxyServer REG_SZ → proxy alist.
   Forms: host:port | http://host:port | http=h:p;https=h:p | socks=h:p"
  (when (and value (plusp (length (string-trim '(#\Space #\Tab) value))))
    (let* ((v (string-trim '(#\Space #\Tab) value))
           (eq-p (find #\= v)))
      (if (not eq-p)
          (list (cons "*" (%ensure-proxy-url v)))
          (loop for part in (uiop:split-string v :separator ";")
                for s = (string-trim '(#\Space #\Tab) part)
                for =pos = (position #\= s)
                when (and =pos (plusp =pos))
                  collect (let* ((scheme (string-downcase (subseq s 0 =pos)))
                                 (target (string-trim '(#\Space #\Tab)
                                                      (subseq s (1+ =pos)))))
                            (cons (if (member scheme '("*" "all") :test #'string=)
                                      "*"
                                      scheme)
                                  (%ensure-proxy-url target scheme))))))))

(defun %ensure-proxy-url (target &optional scheme-hint)
  "Normalize host:port or URL → proxy URL string."
  (let ((t* (string-trim '(#\Space #\Tab) target)))
    (cond
      ((zerop (length t*)) t*)
      ((search "://" t*) t*)
      (t
       (let* ((hint (string-downcase (or scheme-hint "http")))
              (scheme (cond ((member hint '("socks" "socks5" "socks5h")
                                     :test #'string=)
                             (if (string= hint "socks") "socks5" hint))
                            (t "http"))))
         (format nil "~A://~A" scheme t*))))))

(defun parse-windows-proxy-override (value)
  "IE ProxyOverride → NO_PROXY string. Maps <local> → localhost,127.0.0.1,::1."
  (when (and value (plusp (length (string-trim '(#\Space #\Tab) value))))
    (let* ((parts (remove ""
                          (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                                  (uiop:split-string value :separator ";"))
                          :test #'string=))
           (expanded
             (mapcan (lambda (p)
                       (if (string-equal p "<local>")
                           (list "localhost" "127.0.0.1" "::1")
                           (list p)))
                     parts)))
      (when expanded
        (format nil "~{~A~^,~}" expanded)))))

(defun windows-reg-query (name &key (key +windows-inet-settings-key+))
  "Return (values value type-string) for NAME under KEY, or NIL."
  (unless (%windows-p)
    (return-from windows-reg-query nil))
  (let* ((out (uiop:run-program
               (list "reg" "query" key "/v" name)
               :output :string :error-output nil :ignore-error-status t))
         (line (find-if (lambda (l) (search name l :test #'char-equal))
                        (uiop:split-string out :separator '(#\Newline))))
         (parts (and line
                     (remove ""
                             (uiop:split-string
                              (string-trim '(#\Return #\Space #\Tab) line))
                             :test #'string=))))
    (when (and parts (<= 3 (length parts)))
      (values (format nil "~{~A~^ ~}" (cddr parts)) (second parts)))))

(defun windows-internet-settings ()
  "plist (:proxy-enable :proxy-server :proxy-override :auto-config-url)."
  (flet ((dword (name)
           (multiple-value-bind (v typ) (windows-reg-query name)
             (declare (ignore typ))
             (when v
               (or (ignore-errors (parse-integer (string-trim '(#\Space) v)
                                                 :junk-allowed t))
                   0)))))
    (list :proxy-enable (dword "ProxyEnable")
          :proxy-server (nth-value 0 (windows-reg-query "ProxyServer"))
          :proxy-override (nth-value 0 (windows-reg-query "ProxyOverride"))
          :auto-config-url (nth-value 0 (windows-reg-query "AutoConfigURL")))))

(defmethod load-proxy-system-platform ((config http-proxy-config) &key)
  "Fill from registry only when env left slots empty. PAC → SYSTEM-AUTOMATIC-P."
  (unless (and (%windows-p) (proxy-config-use-system-proxy config))
    (return-from load-proxy-system-platform nil))
  (let* ((settings (windows-internet-settings))
         (enable (eql (getf settings :proxy-enable) 1))
         (server (getf settings :proxy-server))
         (override (getf settings :proxy-override))
         (pac (getf settings :auto-config-url))
         (alist (and enable (parse-windows-proxy-server server)))
         (env-already (not (null (proxy-config-proxy config)))))
    ;; env > registry: never overwrite env-seeded proxy / no-proxy
    (when (and alist (not env-already))
      (setf (proxy-config-proxy config) alist))
    (when (and override (null (proxy-config-no-proxy config)))
      (setf (proxy-config-no-proxy config)
            (parse-windows-proxy-override override)))
    (when pac
      (setf (proxy-config-script-url config) pac))
    ;; Residual OS automatic (PAC/WPAD/WinHTTP) only when env+registry
    ;; left no concrete proxy. Env proxy ⇒ no :SYSTEM (env overrides).
    (when (null (proxy-config-proxy config))
      (setf (proxy-config-system-automatic-p config) t))
    nil))

(defmethod resolve-system-proxy-platform ((config http-proxy-config) uri)
  "After env miss: :SYSTEM so backends use WinHTTP AUTOMATIC / GetProxyForUrl."
  (declare (ignore uri))
  (when (and (%windows-p)
             (proxy-config-use-system-proxy config)
             (proxy-config-system-automatic-p config))
    :system))
