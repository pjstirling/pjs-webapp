(in-package #:pjs-webapp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass select-statement ()
    ((fields :initarg :fields
	     :reader select-statement-fields)
     (from :initarg :from
	   :reader select-statement-from)
     (where :initarg :where
	    :initform nil
	    :reader select-statement-where)
     (group-by :initarg :group-by
	       :initform nil
	       :reader select-statement-group-by)
     (order-by :initarg :order-by
	       :initform nil
	       :reader select-statement-order-by)
     (limit :initarg :limit
	    :initform nil
	    :reader select-statement-limit))))

;(pjs-debug-print-object:for-class select-statement)

(def-make-naive-load-form select-statement)

(defun parse-sql-expr (form)
  (typecase form
    ((or symbol
	 number
	 string)
     form)
    (cons
     (let ((sym (first form)))
       (case sym
	 (:null=
	  form)
	 (:exists
	  (list sym (parse-sql-select (second form))))
	 (t
	  (list* sym (mapcar #'parse-sql-expr (rest form)))))))
    (t
     (error "unhandled case in parse-expr ~w" form))))

(defun parse-sql-select (form)
  (unless (eq :select (first form))
    (error "bad form ~a" form))
  (let ((fields (second form))
	from
	where
	group-by
	order-by
	limit)
    (labels ((parse-from-form (form)
	       (if (symbolp form)
		   form
		   ;; else		   
		   (case (first form)
		     (:as
		      (list :as
			    (second form)
			    (parse-from-form (third form))))
		     (:select
		       (parse-sql-select form))
		     (:left-join
		      (list* :left-join
			     (with-collector (collect)
			       (pop form)
			       (collect (parse-from-form (pop form)))
			       (while form
				 (collect (parse-from-form (pop form))
				   (pop form))))))
		     (t
		      (error "bad table or sub-select ~w" form))))))
      (dolist (clause (cddr form))
	(case (first clause)
	  (:from
	   (setf from
		 (mapcar #'parse-from-form (rest clause))))
	  (:where
	   (setf where
		(parse-sql-expr (second clause))))
	  (:group-by
	   (setf group-by
		 (rest clause)))
	  (:order-by
	   (setf order-by
		 (rest clause)))
	  (:limit
	   (setf limit
		 (rest clause)))
	  (t
	   (error "bad clause ~w" clause))))
      (make-instance 'select-statement
		     :fields (mapcar #'parse-sql-expr fields)
		     :from from
		     :where where
		     :group-by (mapcar #'parse-sql-expr group-by)
		     :order-by (mapcar #'parse-sql-expr order-by)
		     :limit limit))))

(defun to-sql (stmt args)
  (check-type stmt select-statement)
  (labels ((expr (form)
	     (typecase form
	       (symbol
		(sql-name form))
	       (number
		(format nil "~a" form))
	       (string
		(with-output-to-string (*standard-output*)
		  (write-char #\')
		  (dovector (ch form)
		    (case ch
		      ((#\' #\\)
		       (write-char #\\)
		       (write-char ch))
		      (t
		       (write-char ch))))
		  (write-char #\')))
	       (cons
		(case (first form)
		  ((+ - * / = and or :like)
		   (apply #'join
			  (sconc " "
				 (symbol-name (first form))
				 " ")
			  (mapcar #'expr (rest form))))
		  (not
		   (sconc "NOT " (expr (second form))))
		  (null
		   (apply #'join
			  " AND "
			  (mapcar (lambda (form)
				    (sconc (expr form)
					   " IS NULL"))
				  (rest form))))
		  (:as
		   (destructuring-bind (name expr)
		       (rest form)
		     (sconc (expr expr) " AS " (sql-name name))))
		  (:distinct
		   (sconc "DISTINCT " (expr (second form))))
		  (:null=
		   (destructuring-bind (field index)
		       (rest form)
		     (sconc (sql-name field)
			    (if (nth index args)
				" = ?"
				;; else
				" IS NULL"))))
		  (:exists
		   (sconc "EXISTS " (expr (second form))))
		  (t
		   (sconc (symbol-name (first form))
			  "("
			  (apply #'join
				 ", "
				 (mapcar #'expr (rest form)))
			  ")"))))
	       (select-statement
		(sconc "(" (to-sql form args) ")"))
	       (t
		(error "unhandled type ~w ~a" form (type-of form)))))
	   (table-or-query (form)
	     (typecase form
	       (select-statement
		(sconc "("
		       (to-sql form args)
		       ")"))
	       (symbol
		(sql-name form))
	       (list
		(case (first form)
		  (:as
		   (sconc (table-or-query (third form))
			  " AS "
			  (sql-name (second form))))
		  (:left-join
		   (pop form)
		   (let ((result (table-or-query (pop form))))
		     (while form
		       (setf result
			     (sconc result
				    " LEFT JOIN "
				    (table-or-query (pop form))
				    " ON "
				    (expr (pop form)))))
		     result))))
	       (t
		(error "unhandled type ~w" form))))
	   (order-term (form)
	     (if (and (listp form)
		      (eq :desc (first form)))
		 (sconc (expr (second form))
			" DESC")
		 ;; else
		 (expr form))))
    (sconc "SELECT "
	   (apply #'join
		  ", "
		  (mapcar #'expr (select-statement-fields stmt)))
	   (awhen (select-statement-from stmt)
	     (sconc " FROM "
		    (apply #'join
			   ", "
			   (mapcar #'table-or-query it))))
	   (awhen (select-statement-where stmt)
	     (sconc " WHERE "
		    (expr it)))
	   (awhen (select-statement-group-by stmt)
	     (sconc " GROUP BY "
		    (apply #'join
			   ", "
			   (mapcar #'expr it))))
	   (awhen (select-statement-order-by stmt)
	     (sconc " ORDER BY "
		    (apply #'join ", "
			   (mapcar #'order-term it))))
	   (awhen (select-statement-limit stmt)
	     (sconc " LIMIT "
		    (if (listp it)
			(sconc (expr (first it))
			       " OFFSET "
			       (expr (second it)))
			;; else
			(expr it)))))))

(def-make-naive-load-form select-statement)
