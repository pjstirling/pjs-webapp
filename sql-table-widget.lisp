(in-package #:pjs-webapp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sql-table-views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sql-table-widget ()
  ((name :initarg :name
	 :reader sql-table-widget-name)
   (base-href :initarg :base-href
	      :reader sql-table-widget-base-href)
   (ajax-href :initarg :ajax-href
	      :reader sql-table-widget-ajax-href)
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
		     :code (bind ((:symbols text))
			     `(let ((,text ,(format-code text-info)))
				(unless (null-string-p ,text)
				  ,(if link-info
				       `(<:a :href ,(format-code link-info)
					  (<:as-html ,text))
				       ;; else
				       `(<:as-html ,text)))))))))

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

(defun parse-sql-table-widget-param (param base-href stmt)
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
	   :base-href base-href
	   :ajax-href (sconc base-href
			     (unless (ends-with-p base-href "/")
			       "/")
			     (symbol-name* name) "/ajax/")
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
    (princ-to-string result)))

(defun fix-sort-order (sort-order length)
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
		(setf (aref result index)
		      (1+ i))
		(incf index)))
	    result)))))

(defun build-sql-table-widget-order-by (widget)
  (declare (type sql-table-widget widget))
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

(defun column-headings-form (sorted-columns sort-order)
  (bind ((:symbols sort-value))
    `(flet ((name-for-column (index)
	      (case (abs index)
		,@(mapcar (lambda (index col)
			    `(,(1+ index)
			      ,(sql-table-widget-column-name col)))
		   (range (length sorted-columns))
		   sorted-columns))))
       (list ,@ (mapcar (lambda (index)
			  `(let ((,sort-value (aref ,sort-order ,index)))
			     (list (name-for-column ,sort-value)
				   ,(if (= index 0)
					`(if (< ,sort-value 0)
					     (change-sort-order (- ,sort-value) ,sort-order)
					     ;; else
					     nil)
					;; else
					`(change-sort-order (abs ,sort-value) ,sort-order))
				   ,(if (= index 0)
					`(if (< ,sort-value 0)
					     nil
					     ;; else
					     (change-sort-order (- ,sort-value) ,sort-order))
					;; else
					`(change-sort-order (- (abs ,sort-value)) ,sort-order)))))
			(range (length sorted-columns)))))))

(defun widget-vars (db query-args-var select-sql count-sql column-headings row-count widget)
  (bind ((:slots (per-page offset sort-order count-expression query query-args sorted-columns unsorted-columns filters)
		 widget)
	 (parsed-query (parse-sql-select query))
	 (:symbols select-statement count-statement)
	 (:slots (fields from where order-by group-by limit)
		 parsed-query))
    `((,per-page (or ,per-page 200))
      (,offset (or ,offset 0))
      ,@(mapcar (lambda (filter)
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
      ,@ (when sorted-columns
	   `((,column-headings ,(column-headings-form sorted-columns sort-order))))
      (,select-statement (make-instance 'select-statement
					:fields ,(make-naive-load-form fields)
					:from ,(make-naive-load-form from)
					:where ,(when (or query-args filters where)
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
      (,count-statement (make-instance 'select-statement
				       :fields ',count-expression
				       :from (select-statement-from ,select-statement)
				       :where (select-statement-where ,select-statement)
				       :group-by (select-statement-group-by ,select-statement)))
      (,count-sql (to-sql ,count-statement ,query-args-var))
      (,row-count (apply #'sqlite:execute-one-row-m-v
			 ,db
			 ,count-sql
			 ,query-args-var)))))

;; ====================================================
;;
;; ====================================================

(defun parenscriptify (form)
  (typecase form
    ((or string number)
     form)
    (vector
     `(ps:array ,@ (map 'list #'parenscriptify form)))
    (cons
     `(ps:array ,@ (mapcar #'parenscriptify form)))))

;; ====================================================
;;
;; ====================================================

(defun build-sql-table-widget-renderer (widget stmt other-params)
  (bind ((:symbols db navigator query-args-var select-sql count-sql row-count column-headings column-name up down)
	 (:slots (name base-href ajax-href offset per-page sort-order count-expression query query-args sorted-columns unsorted-columns filters html-class)
		 widget))
    `(,name (,db)
	    (let* ,(widget-vars db query-args-var select-sql count-sql column-headings row-count widget)
	      (with-sqlite-statements (,db (,stmt ,select-sql))
		(labels ((,navigator ()
			   (unless (< ,row-count ,per-page)
			     (<:p :class "nav"
			       (do ((i 0 (incf i ,per-page))
				    (page 1 (incf page)))
				   ((< ,row-count i))
				 (if (= i ,offset)
				     (<:as-html page)
				     ;; else
				     (self-link page ',offset i))
				 (<:as-html " ")))))
			 ,@(when sorted-columns
			     `((render-column (index)
					      (case (abs index)
						,@ (mapcar (lambda (index col)
							     `(,(1+ index)
							       ,(sql-table-widget-column-code col)))
						    (range (length sorted-columns))
						    sorted-columns))))))
		  (<:div :class ,(or html-class
				     "sql-widget")
		    :id ,(symbol-name* name)
		    (<:p (<:as-html ,select-sql))
		    (<:p (<:as-html ,query-args-var))
		    (<:p (<:as-html ,count-sql))
		    ,@ (mapcar (lambda (filter)
				 (with-slots (name test-name description values)
				     filter
				   `(<:p (<:as-html ,description)
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
				      (when ,test-name
					(<:as-html " ")
					(self-link "Disable filter" ',name nil)))))
			       filters)
		    (,navigator)
		    (<:table
		      (<:thead
			(<:tr
			  ,@ (when sorted-columns
			       `((dolist (col ,column-headings)
				   (destructuring-bind (,column-name ,up ,down)
				       col
				     (<:th
				       (<:as-html ,column-name)
				       " "
				       (if ,up
					   (self-link "Up"
						      ',sort-order ,up)
					   ;; else
					   (<:as-html "Up"))
				       " "
				       (if ,down
					   (self-link "Down"
						      ',sort-order ,down)
					   ;; else
					   "Down"))))))
			  ,@ (mapcar (lambda (col)
				       `(<:th (<:as-html ,(sql-table-widget-column-name col))))
				     unsorted-columns)))
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
		    (,navigator)
		    (<:script
		      (<:as-is (let (ps:*ps-print-pretty*)
				 (ps:ps* `(register-ajax-table (ps:create id ,,(symbol-name* name)
									  base-href ,,base-href
									  ajax-href ,,ajax-href
									  per-page ,,(symbol-name* per-page)
									  offset ,,(symbol-name* offset)
									  row-count ,,row-count
									  sort-order ,,(symbol-name* sort-order)
									  params (ps:create ,,@ (apply #'append
												       (mapcar (lambda (param)
														 (let ((sym (hunchentoot-param-name param)))
														   (list (symbol-name* sym) sym)))
													       (list* per-page offset sort-order
														      other-params)))))
							       ,,(if sorted-columns
								     `(parenscriptify ,column-headings)
								     ;; else
								     nil)))))))))))))

(defun insert-separator (sep list)
  (let (seen)
    (dolist-c (el list)
      (if seen
	  (collect sep)
	  ;; else
	  (setf seen t))
      (collect el))))

(defmacro dotimes-list ((i el list &key (start 0) (result nil resultp)) &body body)
  `(let ((,i ,start))
     (dolist (,el ,list ,@ (when resultp
			     (list result)))
       ,@body
       (incf ,i))))

(defun build-sql-table-widget-ajax-handler (page-name other-params authenticated-test db-context widget stmt)
  (bind ((:db (with-db db)
	      db-context)
	 (name (sql-table-widget-name widget))
	 (print-comma '(write-string ", "))
	 (:symbols query-arg query-args-var select-sql count-sql column-headings row-count index seen column-name up down)
	 (:slots (per-page offset sort-order sorted-columns unsorted-columns ajax-href)
		 widget))
    `(hunchentoot:define-easy-handler (,(symb page-name "-" name "-ajax") :uri ,ajax-href)
	 (,@(sql-table-widget-params widget)
	  ,@other-params)
       ;; it's more effort than I want to go to to decide if a given parameter is required for this widget
       ;; (in general you would need to macroexpand the params and then walk the forms for function calls etc),
       ;; so just mark them all as ignorable
       ,@(when other-params
	   `((declare (ignorable ,@(mapcar #'hunchentoot-param-name other-params)))))
       (hunchentoot:start-session)
       ,authenticated-test
       (setf (hunchentoot:content-type*)
	     "application/json; charset=utf-8")
       (hunchentoot:no-cache)
       (,with-db
	   (let* ,(widget-vars db query-args-var select-sql count-sql column-headings row-count widget)
	     (with-sqlite-statements (,db (,stmt ,select-sql))
	       (dotimes-list (,index ,query-arg ,query-args-var :start 1)
		 (sqlite:bind-parameter ,stmt ,index ,query-arg))
	       (with-output-to-string (*standard-output*)
		 (format t "{")
		 (format t "\"columnHeadings\" : [")
		 ,@(when sorted-columns
		     `((let (,seen)
			 (dolist (col ,column-headings)
			   (if ,seen
			       (format t ", ")
			       ;; else
			       (setf ,seen t))
			   (destructuring-bind (,column-name ,up ,down)
			       col
			     (format t "[~a, ~a, ~a]"
				     (com.gigamonkeys.json:json ,column-name)
				     (or (and ,up
					      (com.gigamonkeys.json:json ,up))
					 "null")
				     (or (and ,down
					      (com.gigamonkeys.json:json ,down))
					 "null")))))))
		 (format t "], ")
		 (format t "\"rowCount\" : ~d, " ,row-count)
		 (format t "\"rows\" : [")
		 ;; no optional trailing comma in json, so must conditionally insert separator
		 (let (,seen)
		   (while (sqlite:step-statement ,stmt)
		     (if ,seen
			 ,print-comma
			 ;; else
			 (setf ,seen t))
		     (labels ,(when sorted-columns
				`((render-column (index)
						 (pjs-yaclml:with-yaclml-output-to-string
						   (case (abs index)
						     ,@ (mapcar (lambda (index col)
								  `(,(1+ index)
								    ,(sql-table-widget-column-code col)))
							 (range (length sorted-columns))
							 sorted-columns))))))
		       (format t "[")
		       ,@(insert-separator print-comma
					   (mapcar (lambda (form)
						     `(write-string (com.gigamonkeys.json:json ,form)))
						   (append (mapcar (lambda (index)
								     `(render-column (aref ,sort-order ,index)))
								   (range (length sorted-columns)))
							   (mapcar (lambda (col)
								     `(pjs-yaclml:with-yaclml-output-to-string
									,(sql-table-widget-column-code col)))
								   unsorted-columns))))
		       (format t "]"))))
		 (format t "]")
		 (format t "}"))))))))

(defmacro sql-easy-handler ((name &key uri authenticated-test db-context)
			    (&rest params) &body body)
  (bind ((:mv (table-params other-params)
	      (partition #'table-param-p params))
	 (stmt (gensym "STMT-"))
	 (parsed-widgets (mapcar (lambda (param)
				   (parse-sql-table-widget-param param uri stmt))
				 table-params)))
    `(progn
       (self-link-easy-handler (,name :uri ,uri)
	   (,@other-params
	    ,@(apply #'append (mapcar #'sql-table-widget-params parsed-widgets)))
	 (flet ,(mapcar (lambda (widget)
			  (build-sql-table-widget-renderer widget stmt other-params))
		 parsed-widgets)
	   ,@body))
       ,@(when db-context
	   (mapcar (lambda (widget)
		     (build-sql-table-widget-ajax-handler name other-params authenticated-test db-context widget stmt))
		   parsed-widgets)))))

(defun expand-web-params (params)
  (dolist-c (param params)
    (if (table-param-p param)
	(dolist (param (sql-table-widget-params (parse-sql-table-widget-param param "" (gensym))))
	  (collect param))
	;; else
	(collect param))))

(defmacro define-widget-script-handler (name location)
  `(hunchentoot:define-easy-handler (,name :uri ,location)
       ()
     (setf (hunchentoot:content-type*)
	   "text/javascript; charset=utf-8")
     (multiple-value-bind (code time)
	 (pjs-webapp-js:sql-table-ajax-script)
       (setf (hunchentoot:header-out :last-modified)
	     (hunchentoot:rfc-1123-date time))
       (hunchentoot:handle-if-modified-since time)
       code)))
