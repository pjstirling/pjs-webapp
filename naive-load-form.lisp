(in-package #:pjs-webapp)

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

