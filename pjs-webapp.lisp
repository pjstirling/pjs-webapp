(in-package #:pjs-webapp)

(defvar *hunchentoot-acceptor* nil)

;; ========================================================
;;
;; ========================================================

(defparameter +port+
  8181
  "The TCP port that will be bound for connections. Making your lisp setuid in order to bind port 80 would be foolish.")

;; ========================================================
;; nginx needs config from http://nginx.org/en/docs/http/ngx_http_proxy_module.html
;; ========================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +real-ip-header-name+
    "X-Real-IP"
    "The name of the header containing the real IP address of the request, when using a reverse proxy (this will require coordination with the configuration of your proxy). Set to NIL if you aren't running a reverse proxy (so Host-URLs will use the correct port)."))

;; ========================================================
;; a helper to push the check into compile-time
;; ========================================================

(defmacro %if-reverse-proxy (proxied raw)
  (if +real-ip-header-name+
      proxied
      ;; else
      raw))

;; ========================================================
;;
;; ========================================================

(defun web-init ()
  (unless *hunchentoot-acceptor*
    (setf hunchentoot:*show-lisp-errors-p* t)
    (setf *hunchentoot-acceptor*
	  (make-instance 'hunchentoot:easy-acceptor
			 :port +port+))
    (setf (hunchentoot:acceptor-access-log-destination *hunchentoot-acceptor*)
	  (asdf:system-relative-pathname "pjs-webapp" "access.log"))
    (setf (hunchentoot:acceptor-message-log-destination *hunchentoot-acceptor*)
	  (asdf:system-relative-pathname "pjs-webapp" "message.log"))
    (hunchentoot:start *hunchentoot-acceptor*)))

;; ========================================================
;;
;; ========================================================

(eval-when (:load-toplevel :execute)
  (web-init))

;; ========================================================
;;
;; ========================================================

(defun remote-addr* (&optional (request hunchentoot:*request*))
  (%if-reverse-proxy
   (or (hunchentoot:header-in +real-ip-header-name+ request)
       (error "missing real ip header"))
   ;; else no proxy
   (hunchentoot:remote-addr request)))

;; ========================================================
;;
;; ========================================================

(defparameter +utf8+ (flexi-streams:make-external-format :utf8))

;; ========================================================
;;
;; ========================================================

(defun utf8-byte-to-char (ch)
  (cond 
    ((<= #x41 ch #x5a)    ;; A-Z
     (char "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	   (- ch #x41)))
    ((<= #x61 ch #x7a)    ;; a-z
     (char "abcdefghijklmnopqrstuvwxyz"
	   (- ch #x61)))
    ((<= #x30 ch #x39)    ;; 0-9
     (char "0123456789"
	   (- ch #x30)))
    ((= ch #x2d) ;; minus
     #\-)
    ((= ch #x2e) ;; full-stop
     #\.)
    ((= ch #x5f) ;; underscore
     #\_)
    ((= ch #x7e) ;; tilde
     #\~)
    (t ;; else need to do escaped char
     nil)))

;; ========================================================
;; source: http://en.wikipedia.org/wiki/Percent-encoding
;; ========================================================

(defun url-encode (object)
  (let* ((object (if (stringp object)
		     object
		     ;; else
		     (princ-to-string object)))
	 (object (flexi-streams:string-to-octets object
						 :external-format +utf8+)))
    (with-output-to-string (s)
      (dovector (ch object)
	(let ((real-ch (utf8-byte-to-char ch)))
	  (if real-ch
	      (write-char real-ch s)
	      ;; else
	      (format s "%~2,'0x" ch)))))))

;; ========================================================
;;
;; ========================================================

(defmacro url-with-params (&environment env page &rest args)
  (if args
      `(join "?"
	     ,(macroexpand page env)
	     (join "&"
		   ,@(mapcar (lambda (arg)
			       `(when ,arg
				  (sconc ,(sconc (url-encode (symbol-name* arg))
						 "=")
					 (url-encode ,arg))))
			     args)))
      ;; else
      page))

;; ========================================================
;;
;; ========================================================

(defmacro host-url-with-params (page &rest args)
  `(url-with-params (sconc (if (local-network-host-p (remote-addr*))
			       "http://"
			       "https://")
			   (hunchentoot:host)
			   (%if-reverse-proxy
			    "/"
			    ;; else
			    ,(format nil ":~d/" +port+)) 
			   ,page)
		    ,@args))

;; ========================================================
;;
;; ========================================================

(defun local-network-host-p (host-addr)
  (let ((ip-prefix "192.168.0."))
    (and (stringp host-addr)
	 (or (string= host-addr "127.0.0.1")
	     (and (begins-with-p host-addr ip-prefix)
		  (< 0 (parse-integer host-addr
				      :start (length ip-prefix)
				      :junk-allowed nil)
		     256))))))

;; ========================================================
;;
;; ========================================================

(defun local-network-request-p ()
  (local-network-host-p (remote-addr*)))

;; ========================================================
;;
;; ========================================================

(defun hunchentoot-param-name (param)
  (if (listp param)
      (first param)
      ;; else
      param))

;; ========================================================
;;
;; ========================================================

(defmacro self-link-easy-handler ((name
				   &key uri
				     (acceptor-names t)
				     (default-parameter-type ''string)
				     (default-request-type :both))
				  (&rest params)
				  &body body)
  "HUNCHENTOOT:DEFINE-EASY-HANDLER augmented with lexical SELF-LINK and SELF-HREF that automate producing URLs that point at the current page, possibly with some of the parameters changed"
  (let ((param-names (mapcar #'hunchentoot-param-name params)))
    `(hunchentoot:define-easy-handler (,name
				       :uri ,uri
				       :acceptor-names ,acceptor-names
				       :default-parameter-type ,default-parameter-type
				       :default-request-type ,default-request-type)
	 ,params
       (macrolet ((self-link (text &rest params)
		    `(<:a :href (self-href ,@params)
		       (<:as-html ,text))))
	 (flet ((self-href (&rest params)
		  (let ,(mapcar (lambda (name)
				  `(,name ,name))
			 param-names)
		    (while params
		      (let ((name (pop params))
			    (value (pop params)))
			(case name
			  ,@ (mapcar (lambda (name)
				       `(,name
					 (setf ,name value)))
			      param-names)
			     (t
			      (error "unknown param ~a -> ~w" name value)))))
		    (host-url-with-params ,uri ,@param-names))))
	   ,@body)))))
