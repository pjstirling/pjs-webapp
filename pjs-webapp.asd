(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload "hunchentoot"))

(asdf:defsystem #:pjs-webapp
  :serial t
  :depends-on (#:pjs-utils #:pjs-logging #:pjs-yaclml #:hunchentoot #-sbcl #:closer-mop #:pjs-sqlite)
  :components ((:file "package")
               (:file "pjs-webapp")
	       (:file "sql-table-widget")))
