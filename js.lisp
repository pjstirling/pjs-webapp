(defpackage #:pjs-webapp-js
  (:use #:cl #:ps)
  (:export #:sql-table-ajax-script))

(in-package #:pjs-webapp-js)

(defmacro js-func (name &body body)
  (let ((code (eval `(ps ,@body))))
    `(defun ,name ()
       (values ,code
	       ,(get-universal-time)))))

(defmacro+ps def-event-listener (target event &body body)
  `(chain ,target
	  (add-event-listener ,event
			      (lambda (evt)
				,@body
				nil))))

(defmacro+ps create-text-node (text)
  `(chain document (create-text-node ,text)))

(defmacro+ps create-element (type)
  `(chain document (create-element ,type)))

(js-func sql-table-ajax-script
  (defun first-element-child (node)
    (if (or (null node)
	    (undefined node))
	(progn
	  (alert "missing node in first-element-child")
	  nil)
	;; else
	(let ((child (@ node first-element-child)))
	  (if (or (null child)
		  (undefined child))
	      (progn
		(alert "missing child in first-element-child")
		nil)
	      ;; else
	      child))))

  (defun replace-with (el replacement)
    (when (null el)
      (alert "missing element to replace"))
    (let ((parent (@ el parent-node)))
      (when (null parent)
	(alert "missing element parent"))
      (chain parent (replace-child replacement el))))

  (defun params-to-url (state replacement-key replacement-value)
    (let ((result (@ state base-href))
	  (params (@ state params))
	  seen)
      (for-in (k params)
	      (let ((v (if (and replacement-key
				(= k replacement-key))
			   replacement-value
			   ;; else
			   (getprop params k))))
		(when v
		  (if seen
		      (setf result
			    (+ result "&"))
		      ;; else
		      (setf seen t))
		  (setf result
			(+ result
			   (encode-u-r-i-component k)
			   "="
			   (encode-u-r-i-component v))))))
      result))

  (defun build-replacement-link (state text replaced-param replaced-value)
    (with-slots (params)
	state
      (let ((link (create-element "a")))
	(chain link (append-child (create-text-node text)))
	(setf (@ link href)
	      (params-to-url state replaced-param replaced-value))
	(def-event-listener link "click"
	  (setf (getprop params replaced-param)
		replaced-value)
	  (fetch-data state)
	  (chain evt (prevent-default)))
	link)))
  
  (defun build-replacement-th (heading state)
    (with-slots (params sort-order)
	state
      (let ((th (create-element "th")))
	(destructuring-bind (name up down)
	    heading
	  (chain th (append-child (create-text-node name)))
	  (dolist (link (list (list "Up" up)
			      (list "Down" down)))
	    (let* ((text (aref link 0))
		   (href-param (aref link 1))
		   (el (if href-param
			   (build-replacement-link state text sort-order href-param)
			   ;; else
			   (create-text-node text))))
	      (chain th (append-child (create-text-node " ")))
	      (chain th (append-child el))))
	  th))))

  (defun replace-thead (state column-headings)
    (when column-headings
      (with-slots (widget-table)
	  state
	(let* ((thead  (chain widget-table (get-elements-by-tag-name "thead") 0))
	       (headers (chain thead (get-elements-by-tag-name "th"))))
	  (dotimes (i (@ column-headings length))
	    (let ((new-th (build-replacement-th (aref column-headings i)
						state)))
	      (replace-with (aref headers i)
			    new-th)))))))

  (defun replace-navs (state)
    (with-slots (widget-el row-count per-page offset params)
	state
      (let ((per-page-value (getprop params per-page))
	    (offset-value (getprop params offset))
	    (nav-paras (array)))
	(unless (< row-count per-page-value)
	  ;; get-elements-by-class-name returns a live collection that interacts badly
	  ;; with being mutated while iterated, so copy it
	  (dolist (nav (chain widget-el (get-elements-by-class-name "nav")))
	    (chain nav-paras (push nav)))
	  (dolist (nav nav-paras)
	    (let* ((new-p (create-element "p")))
	      ;; yay for dom shitty-ness:
	      ;; (setf (@ new-p class) ...) doesn't work because it's really (@ new-p class-list)
	      ;; (setf (@ new-p class-list) ...) doesn't work because class-list is a
	      ;; read-only attribute
	      (chain new-p (set-attribute "class" "nav"))
	      (do ((i 0 (incf i per-page-value))
		   (page 1 (incf page)))
		  ((< row-count i))
		(let ((new-el (if (= i offset-value)
				  (create-text-node page)
				  ;; else
				  (build-replacement-link state page offset i))))
		  (chain new-p (append-child new-el))
		  (chain new-p (append-child (create-text-node " ")))))
	      (replace-with nav new-p)))))
      nil))

  (defun replace-links (state column-headings)
    (replace-thead state column-headings)
    (replace-navs state))
  
  (defun update-table (state doc)
    (with-slots (rows column-headings row-count)
	doc
      (let* ((widget-table (@ state widget-table))
	     (tbody (chain widget-table (get-elements-by-tag-name "tbody") 0))
	     (children (@ tbody child-nodes)))
	;; replace the table head (to account for possible new sort order
	(replace-links state column-headings)
	;; remove the old rows
	(while (@ tbody first-child)
	  (chain tbody (remove-child (@ tbody first-child))))
	;; create new rows
	(dolist (row rows)
	  (let ((tr (chain document (create-element "tr"))))
	    (dolist (col row)
	      (let ((td (chain document (create-element "td"))))
		(setf (@ td inner-h-t-m-l)
		      col)
		(chain tr (append-child td))))
	    (chain tbody (append-child tr)))))))
  
  (defun build-replacement-div (state)
    (let ((broken-widget (chain document (create-element "div"))))
      (setf (@ broken-widget inner-h-t-m-l)
	    (lisp (pjs-yaclml:with-yaclml-output-to-string
		    (<:p "An error occured asking the server for fresh data. Sadface. "
		      (<:a :href "#" "Try reloading.")))))
      (chain broken-widget (set-attribute "id" (@ state id)))
      ;; if we built the inner-html in js on the client we could give the A tag an id instead
      (def-event-listener (first-element-child (first-element-child broken-widget))
	  "click"
	(replace-with broken-widget
		      (@ state widget-div))
	(fetch-data state))
      broken-widget))

  ;; ie11 doesn't support responseType="json"
  (defun fetch-data (state)
    (let ((xhr (new (-x-m-l-http-request))))
      (with-slots (widget-el params)
	  state
	(def-event-listener xhr "readystatechange"
	  (when (equal (@ this ready-state)
		       (@ this +done+))
	    (if (= (@ this status)
		   200)
		(update-table state (chain +json+ (parse (@ this response))))
		;; else
		(replace-with widget-el
			      (build-replacement-div state)))))
	(let ((fd (new (-form-data))))
	  (for-in (key params)
		  (chain fd (append key
				    (getprop params key))))
	  (chain xhr (open "POST" (@ state ajax-href)))
	  (setf (@ xhr response-type)
		"text")
	  (chain xhr (send fd))))))

  (defun register-ajax-table (state column-headings)
    (setf (@ state widget-el)
	  (chain document (get-element-by-id (@ state id))))
    (setf (@ state widget-table)
	  (chain (@ state widget-el)
		 (get-elements-by-tag-name "table") 0))
    (replace-links state column-headings)
    nil))
