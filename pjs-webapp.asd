(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "hunchentoot"))

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
	       (:file "sql-table-widget")))
