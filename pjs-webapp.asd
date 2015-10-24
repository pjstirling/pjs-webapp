(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("hunchentoot" "com.gigamonkeys.json" "parenscript")))

(asdf:defsystem #:pjs-webapp
  :serial t
  :depends-on (#:pjs-utils
	       #:pjs-logging
	       #:pjs-yaclml
	       #:pjs-sqlite

	       #:hunchentoot
	       #-sbcl
	       #:closer-mop)
  :components ((:file "package")
               (:file "pjs-webapp")
	       (:file "naive-load-form")
	       (:file "selects")
	       (:file "sql-table-widget")))
