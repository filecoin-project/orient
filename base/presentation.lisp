(in-package orient)

(def-suite presentation-suite)
(in-suite presentation-suite)

(defgeneric format-value (format value)
  (:method ((format t) (thing t))
    (princ-to-string (representation thing)))
  (:method ((format (eql :org)) (thing t))
    (princ-to-string (representation thing)))
  (:method ((format (eql :org)) (relation relation))
    ;; Remove the RELATION prefix and we should have a list of lists, to be formatted as an org-mode table.
    ;; AKA the world's fanciest CDR.
    (destructuring-bind (ignore attributes &rest rows)
	(representation relation)
      (declare (ignore ignore))
      `(,attributes ,@rows)))
  (:method ((format (eql :org)) (tuple wb-map))
    (format-value format (ensure-relation tuple))))

(defgeneric present-data (format thing system &rest keys)
  (:method ((format t) (thing t) (null null) &key)
    (format-value format thing))
  
  (:method ((format t) (thing null) (null null) &key)
    "NULL")

  (:method ((format (eql :org-html)) (list list) (system t) &rest keys)
    `((:table :border "none" :padding 0)      
      ,@(loop for row in list
	   for i from 0
	   collect `((:tr ,@(when (oddp i) '(:bgcolor :lightgrey)))
		     ,@(loop for elt in row collect `(:td ,(format-value format elt)))))))

  (:method ((format (eql :org)) (thing null) (system t) &key)
    (list))

  (:method ((format (eql :org)) (tuple wb-map) (system t) &rest keys &key attribute-order)
    (let ((attr-list (convert 'list (attributes tuple))))
	  (list attr-list
		(loop for attr in attr-list
		   collect (tref attr tuple)))))
  
  (:method ((format (eql :org)) (relation relation) (system t) &rest keys &key sort-by key)
    (let ((attr-list (convert 'list (attributes relation)))
	  (tuples (convert 'list (tuples relation))))
      (cons attr-list	    
	    (loop for tuple in (if sort-by (sort tuples sort-by :key key) tuples)				   
	       collect (loop for attr in attr-list
			  collect (tref attr tuple)))))))
  
(defun org-present (&rest args)
  (apply #'present-data :org args))

(defgeneric link (format uri text)    
  (:method ((format (eql :html-string)) (uri string) (text string))
    (format nil "<A HREF='~S'>~A</A>" uri text))
  (:method ((format (eql :org)) (uri string) (text string))
    (format nil "[[~A][~A]]" uri text)))

(defun project-commit-link (format)
  (multiple-value-bind (commit uri)
      (project-commit)
    (link format uri commit)))

(defun org-present-tuple (tuple system &key include-tmps)
  (let* ((all-attributes (sort (convert 'list (attributes tuple)) #'string< :key #'symbol-name))
	 (attrs-to-use (if include-tmps
			   all-attributes
			   (remove-if (lambda (attr)
					(let ((name (symbol-name attr)))
					  (eql (char name (1- (length name))) #\%)))
				      all-attributes))))
    (cons
     '("Parameter" "Value" "Description")
     (loop for attr in attrs-to-use
	collect (list attr (tref attr tuple) (or (lookup-description attr system) ""))))))

