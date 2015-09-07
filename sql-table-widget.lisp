(in-package #:pjs-webapp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql-table-views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sql-table-widget ()
  ((name :initarg :name
	 :reader sql-table-widget-name)
   (offset :initarg :offset
	   :reader sql-table-widget-offset)
   (per-page :initarg :per-page
	     :reader sql-table-widget-per-page)
   (sort-order :initarg :sort-order
	       :reader sql-table-widget-sort-order)
   (sorted-columns :initarg :sorted-columns
	    :initform (error "need columns!")
		   :reader sql-table-widget-sorted-columns)
   (unsorted-columns :initarg :unsorted-columns
	    :initform (error "need columns!")
	    :reader sql-table-widget-unsorted-columns)
   (count-expression :initarg :count-expression
		     :initform '((count *))
		     :reader sql-table-widget-count-expression)
   (query :initarg :query
	  :initform (error "need a query!")
	  :reader sql-table-widget-query)
   (query-args :initarg :query-args
	       :initform nil
	       :reader sql-table-widget-query-args)
   (filters :initarg :filters
	    :initform nil
	    :reader sql-table-widget-filters)
   (html-class :initarg :html-class
	       :initform nil
	       :reader sql-table-widget-html-class)))

(defclass sql-table-widget-column ()
  ((name :initarg :name
	 :initform (error "need a column name!")
	 :reader sql-table-widget-column-name)
   (code :initarg :code
     :reader sql-table-widget-column-code)
   (sort :initarg :sort
	 :initform nil
	 :reader sql-table-widget-column-sort)))

(defclass sql-table-widget-filter ()
  ((name :initarg :name
	 :accessor sql-table-widget-filter-name)
   (description :initarg :description
		:reader sql-table-widget-filter-description)
   (sql :initarg :sql
	:reader sql-table-widget-filter-sql)
   (enabled :initform nil
	    :accessor sql-table-widget-filter-enabled)))

(defclass combo-filter ()
  ((name :initarg :name
	 :reader combo-filter-name)
   (test-name :initarg :test
	      :reader combo-filter-test-name)
   (description :initarg :description
		:reader combo-filter-description)
   (sql :initarg :sql
	:reader combo-filter-sql)
   (values :initarg :values
	   :reader combo-filter-values)))

(defun table-param-p (param)
  (and (listp param)
       (eq (second param) :parameter-type)
       (listp (third param))
       (eq (car (third param))
	   'sql-table)))

(defun parse-table-widget-column (column stmt)
  (destructuring-bind (name text-info &optional link-info sort)
      column
    (labels ((scv (arg)
	       `(sqlite:statement-column-value ,stmt ,arg))
	     (format-code (info)
	       (cond
		 ((numberp info)
		  (scv info))
		 ((and (listp info)
		       (stringp (first info)))
		  `(format nil ,(first info) ,@ (mapcar #'scv (rest info))))
		 ((listp info)
		  `(,(first info) ,@ (mapcar #'scv (rest info))))
		 (t
		  (error "unhandled case ~w" info)))))
      (make-instance 'sql-table-widget-column
		     :name name
		     :sort sort
		     :code `(let ((text ,(format-code text-info)))
			      (unless (null-string-p text)
				,(if link-info
				     `(<:a :href ,(format-code link-info)
					(<:as-html text))
				     ;; else
				     `(<:as-html text))))))))

(defun parse-table-widget-filter (widget-name filter)
  (destructuring-bind (name class &rest options)
      filter
    (case class
      (combo-filter
       (apply #'make-instance 'combo-filter
	      :name (symb (symbol-name widget-name) "-" (symbol-name name))
	      :test (gensym)
	      options))
      (t
       (error "unhandled filter ~w" filter)))))

(defun parse-sql-table-widget-param (param stmt)
  (unless (table-param-p param)
    (error "bad argument ~w" param))
  (bind ((name (first param))
	 (options (cdr (third param)))
	 (options (while-c options
		    (let* ((option (pop options))
			   (value (pop options)))
		      (case option
			((:count-expression
			  :query
			  :query-args
			  :html-class)
			 (collect option value))
			(:columns
			 (multiple-value-bind (sorted unsorted)
			     (partition #'sql-table-widget-column-sort
					(mapcar (lambda (col)
					    (parse-table-widget-column col stmt))
						value))
			   (collect :sorted-columns sorted :unsorted-columns unsorted)))
			(:filters
			 (collect :filters)
			 (collect (mapcar (lambda (filter)
					    (parse-table-widget-filter name filter))
					  value)))
			(t
			 (error "bad option for table ~w: ~w" name option)))))))
    (apply #'make-instance
	   'sql-table-widget
	   :name name
	   :offset (symb name "-offset")
	   :per-page (symb name "-per-page")
	   :sort-order (symb name "-sort-order")
	   options)))

(defun sql-table-widget-params (widget)
  `((,(sql-table-widget-offset widget) :parameter-type 'integer)
    (,(sql-table-widget-per-page widget) :parameter-type 'integer)
    ,(sql-table-widget-sort-order widget)
    ,@ (mapcar (lambda (filter)
		 (typecase filter
		   (combo-filter
		    `(,(combo-filter-name filter)
		      :parameter-type 'integer))
		   (t
		    (error "unhandled filter type ~w" filter))))
	       (sql-table-widget-filters widget))))

(defun range (length)
  (dotimes-c (i length)
    (collect i)))

(defun build-sql-table-widget-thead-renderer (widget)
  (let ((sort-order (sql-table-widget-sort-order widget))
	(sorted (sql-table-widget-sorted-columns widget))
	(unsorted (sql-table-widget-unsorted-columns widget)))
    `(<:thead
       (<:tr
	 ,@(mapcar (lambda (index)
		     `(<:th
			(let ((sort-value (aref ,sort-order ,index)))
			  (<:as-html (name-for-column sort-value)
				     " ")
			  ,(if (= 0 index)
			       `(if (< sort-value 0)
				    (self-link "Up"
					       ',sort-order
					       (change-sort-order (- sort-value) ,sort-order))
				    ;; else
				    (<:as-html "Up"))
			       ;; else
			       `(self-link "Up"
					   ',sort-order
					   (change-sort-order (abs sort-value) ,sort-order)))
			  (<:as-html " ")
			  ,(if (= 0 index)
			       `(if (< sort-value 0)
				    (<:as-html "Down")
				    ;; else
				    (self-link "Down"
					       ',sort-order
					       (change-sort-order (- sort-value) ,sort-order)))
			       ;; else
			       `(self-link "Down"
					   ',sort-order
					   (change-sort-order (- (abs sort-value)) ,sort-order))))))
		   (range (length sorted)))
	 ,@ (mapcar (lambda (col)
		      `(<:th (<:as-html ,(sql-table-widget-column-name col))))
		    unsorted)))))

(defun change-sort-order (priority sort-order)
  (let ((result (make-array (length sort-order)))
	(index 1))
    (setf (aref result 0)
	  priority)
    (dovector (i sort-order)
      (unless (or (= i priority)
		  (= i (- priority)))
	(setf (aref result index)
	      i)
	(incf index)))
    result))

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

#+nil
(defmethod make-load-form ((obj select-statement) &optional env)
  (make-load-form-saving-slots obj :slot-names '(fields from where group-by order-by limit) :environment env))

;(pjs-debug-print-object:for-class select-statement)

(defgeneric make-naive-load-form (obj))

(defmethod make-naive-load-form ((obj symbol))
  (if (or (keywordp obj)
	  (eq obj nil)
	  (eq obj t))
      obj
      ;; else
      `',obj))

(defun proper-list-p (list)
  (and (consp list)
       (or (null (cdr list))
	   (proper-list-p (cdr list)))))

(defun safe-to-quote (form)
  (or (numberp form)
      (stringp form)
      (symbolp form)
      (and (proper-list-p form)
	   (every #'safe-to-quote form))))

(defmethod make-naive-load-form ((obj cons))
  (if (safe-to-quote obj)
      (list 'quote obj)
      ;;else
      (if (proper-list-p obj)
	  `(list ,@ (mapcar #'make-naive-load-form obj))
	  ;;else
	  `(cons ,(make-naive-load-form (car obj))
		 ,(make-naive-load-form (cdr obj))))))

(defmethod make-naive-load-form ((obj string))
  obj)

(defmethod make-naive-load-form ((obj number))
  obj)

(defmacro def-make-naive-load-form (klass)
  (let ((class (find-class klass)))
    (c2mop:finalize-inheritance class)
    (let ((slot-forms (mapcar (lambda (slot)
				(let ((name (c2mop:slot-definition-name slot))
				      (initarg (first (c2mop:slot-definition-initargs slot))))
				  `(when (slot-boundp obj ',name)
				     (collect ,initarg
				       (make-naive-load-form (slot-value obj ',name))))))
			       (c2mop:class-slots class))))
      `(defmethod make-naive-load-form ((obj ,klass))
	 (let ((slot-forms (with-collector (collect)
			     ,@slot-forms)))
	   `(make-instance ',',klass
			   ,@ slot-forms))))))

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
  (unless (typep stmt 'select-statement)
    (error "bad argument ~w" stmt))
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
		   (join (sconc " "
				(symbol-name (first form))
				" ")
			 (mapcar #'expr (rest form))))
		  (not
		   (sconc "NOT " (expr (second form))))
		  (null
		   (join " AND "
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
			  (join ", " (mapcar #'expr (rest form)))
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
	   (join ", "
		 (mapcar #'expr (select-statement-fields stmt)))
	   (awhen (select-statement-from stmt)
	     (sconc " FROM "
		    (join ", "
			  (mapcar #'table-or-query it))))
	   (awhen (select-statement-where stmt)
	     (sconc " WHERE "
		    (expr it)))
	   (awhen (select-statement-group-by stmt)
	     (sconc " GROUP BY "
		    (join ", "
			  (mapcar #'expr it))))
	   (awhen (select-statement-order-by stmt)
	     (sconc " ORDER BY "
		    (join ", "
			  (mapcar #'order-term it))))
	   (awhen (select-statement-limit stmt)
	     (sconc " LIMIT "
		    (if (listp it)
			(sconc (expr (first it))
			       " OFFSET "
			       (expr (second it)))
			;; else
			(expr it)))))))

(defun fix-sort-order (sort-order length)
  (declare (optimize (debug 3)))
  (let* ((*read-eval* nil)
	 (value (ignore-errors (read-from-string sort-order)))
	 (marked (make-array length
			     :element-type 'bit
			     :initial-element 0))
	 (result (make-array length))
	 (index 0))
    (flet ((default-result ()
	     (dotimes (i length)
	       (setf (aref result i)
		     (1+ i)))
	     result))
      (if (not (typep value `(simple-vector ,length)))
	  (default-result)
	  ;; else
	  (progn
	    (dovector (el value)
	      (when (numberp el)
		(let* ((el* (1- (abs el))))
		  (when (and (< -1 el* length)
			     ;; 0 is true in CL
			     (= (bit marked el*)
				0))
		    (setf (aref result index)
			  el)
		    (incf index)
		    (setf (bit marked el*)
			  1)))))
	    (dotimes (i length)
	      (unless (= (bit marked i)
			 1)
		(format t "marking missing ~A~%" (1+ i))
		(setf (aref result index)
		      (1+ i))
		(incf index)))
	    result)))))

(defun build-sql-table-widget-order-by (widget)
  (with-slots (sorted-columns sort-order)
      widget
    `(dovector-c (el ,sort-order)
       (case (abs el)
	 ,@(mapcar (lambda (index col)
		     (let ((sort (sql-table-widget-column-sort col)))
		       `(,(incf index)
			 (if (< el 0)
			     ,(if (listp sort)
				  `(collect ,@ (mapcar (lambda (el)
							 `'(:desc ,el))
						       sort))
				  ;; else
				  `(collect '(:desc ,sort)))
			     ;; else not negative
			     ,(if (listp sort)
				  `(collect ,@ (mapcar (lambda (el)
							 `',el)
						       sort))
				  ;; else
				  `(collect ',sort))))))
	    (range (length sorted-columns))
	    sorted-columns)))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(mapcar (lambda (name)
		   `(,name (gensym ,(sconc (symbol-name name) "-"))))
	  names)
     ,@body))

(defun merge-where (from-stmt &rest filter-args)
  (let ((result from-stmt))
    (while filter-args
      (let ((enabled (pop filter-args))
	    (expr (pop filter-args)))
	(when enabled
	  (if (null result)
	      (setf result expr)
	      ;; else
	      (setf result
		    `(and ,result ,expr))))))
    (parse-sql-expr result)))

(defun build-sql-table-widget-renderer (widget stmt)
  (with-gensyms (db navigator query-args-var select-statement select-sql count-query count-sql row-count)
    (with-slots (name offset per-page sort-order count-expression query query-args sorted-columns unsorted-columns filters html-class)
	widget
      (let* ((parsed-query (parse-sql-select query)))
	(with-slots (fields from where order-by group-by limit)
	    parsed-query
	  `(,name (,db)
		  (let* ((,per-page (or ,per-page 200))
			 (,offset (or ,offset 0))
			 ,@ (mapcar (lambda (filter)
				      (let ((filter-name (combo-filter-name filter))
					    (test-name (combo-filter-test-name filter)))
					`(,test-name (case ,filter-name
						       ,@ (mapcar (lambda (i value)
								    `(,i
								       ,(first value)))
							   (range (length (combo-filter-values filter)))
							   (combo-filter-values filter))
							  (t
							   nil)))))
				    filters)
			 (,query-args-var ,(when (or query-args filters)
					     `(with-collector (collect)
						,(when query-args
						   `(collect ,@query-args))
						,@ (mapcar (lambda (filter)
							     (let ((test-name (combo-filter-test-name filter)))
							       `(when ,test-name
								  (collect ,test-name))))
							   filters))))
			 (,sort-order (fix-sort-order ,sort-order ,(length sorted-columns)))
			 (,select-statement (make-instance 'select-statement
							   :fields ,(make-naive-load-form fields)
							   :from ,(make-naive-load-form from)
							   :where ,(when (or query-args filters)
								     (if (null filters)
									 `',where
									 ;; else
									 `(merge-where ',where
										       ,@ (apply #'append
												 (mapcar (lambda (filter)
													   `(,(combo-filter-test-name filter)
													     ',(combo-filter-sql filter)))
													 filters)))))
							   :order-by ,(build-sql-table-widget-order-by widget)
							   :group-by ',group-by
							   :limit (list ,per-page ,offset)))
			 (,select-sql (to-sql ,select-statement ,query-args-var))
			 (,count-query (make-instance 'select-statement
						      :fields ',count-expression
						      :from (select-statement-from ,select-statement)
						      :where (select-statement-where ,select-statement)
						      :group-by (select-statement-group-by ,select-statement)))
			 (,count-sql (to-sql ,count-query ,query-args-var))
			 (,row-count (apply #'sqlite:execute-one-row-m-v
					    ,db
					    ,count-sql
					   ,query-args-var)))
		    (with-sqlite-statements (,db (,stmt ,select-sql))
		      (labels ((,navigator ()
				 (unless (< ,row-count ,per-page)
				   (<:p
				     (do ((i 0 (incf i ,per-page))
					  (page 1 (incf page)))
					 ((< ,row-count i))
				       (if (= i ,offset)
					   (<:as-html page)
					   ;; else
					   (self-link page ',offset i))
				       (<:as-html " ")))))
			       ,@(when sorted-columns
				   `((name-for-column (index)
						      (case (abs index)
							,@ (mapcar (lambda (index col)
								     `(,(1+ index)
								       ,(sql-table-widget-column-name col)))
							    (range (length sorted-columns))
							    sorted-columns)))
				     (render-column (index)
						    (case (abs index)
						      ,@ (mapcar (lambda (index col)
								   `(,(1+ index)
								     ,(sql-table-widget-column-code col)))
							  (range (length sorted-columns))
							  sorted-columns))))))
			(<:div :class ,(or html-class
					    "sql-widget")
			  (<:p (<:as-html ,select-sql))
			  (<:p (<:as-html ,query-args-var))
			  ,@ (mapcar (lambda (filter)
				       (let ((values (combo-filter-values filter))
					     (name (combo-filter-name filter))
					     (test (combo-filter-test-name filter)))
					 `(<:p (<:as-html ,(combo-filter-description filter))
					    ,@ (apply #'append
						      (mapcar (lambda (i value)
								(let ((desc (second value)))
								  `((<:as-html " ")
								    (if (eql ,name ,i)
									(<:as-html ,desc)
									;; else
									(self-link ,desc ',name ,i)))))
							      (range (length values))
							      values))
					    (when ,test
					      (<:as-html " ")
					      (self-link "Disable filter" ',name nil)))))
				     filters)
			  (,navigator)
			  (<:table
			    ,(build-sql-table-widget-thead-renderer widget)
			    (<:tbody
			      (let ((index 0))
				(dolist (arg ,query-args-var)
				  (sqlite:bind-parameter ,stmt (incf index) arg)))
			      (while (sqlite:step-statement ,stmt)
				(<:tr
				  ,@ (mapcar (lambda (index)
					       `(<:td (render-column (aref ,sort-order ,index))))
					     (range (length sorted-columns)))
				  ,@ (mapcar (lambda (col)
					       `(<:td ,(sql-table-widget-column-code col)))
					     unsorted-columns)))))
			  (,navigator)))))))))))



(defmacro sql-easy-handler (name (&rest params) &body body)
  (bind ((:mv (table-params other-params)
	      (partition #'table-param-p params))
	 (stmt (gensym "STMT-"))
	 (parsed-tables (mapcar (lambda (param)
				  (parse-sql-table-widget-param param stmt))
				table-params)))
    `(self-link-easy-handler ,name
	 (,@other-params
	  ,@(apply #'append (mapcar #'sql-table-widget-params parsed-tables)))
       (flet ,(mapcar (lambda (widget)
			(build-sql-table-widget-renderer widget stmt))
	       parsed-tables)
	 ,@body))))

(defun expand-web-params (params)
  (dolist-c (param params)
    (if (table-param-p param)
	(dolist (param (sql-table-widget-params (parse-sql-table-widget-param param (gensym))))
	  (collect param))
	;; else
	(collect param))))
