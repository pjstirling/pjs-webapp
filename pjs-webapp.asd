(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("hunchentoot" "parenscript")))

(asdf:defsystem #:pjs-webapp
  :serial t
  :depends-on (#:pjs-utils
	       #:pjs-logging
	       #:pjs-yaclml
	       #:pjs-sqlite
	       #:pjs-json

	       #:hunchentoot
	       #-sbcl
	       #:closer-mop)
  :components ((:file "package")
               (:file "pjs-webapp")
	       (:file "naive-load-form")
	       (:file "selects")
	       (:file "js")
	       (:file "sql-table-widget")))
