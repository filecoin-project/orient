(in-package :orient)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

;;; Macro-writing macros come first.
(defmacro with-gensyms ((&rest vars) &body body)
  (let ((binding-form 
         (lambda (var)
                `(,var (gensym ,(format nil "~a-" (string var)))))))
    `(let (,@(mapcar binding-form vars))
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General/Util -- Could/should have own package.

(defmacro aif (condition then &optional else)
  `(let ((it ,condition))
     (if it
	 ,then
       ,else)))

(defmacro awhen (condition &body body)
  `(let ((it ,condition))
     (when it
       ,@body)))

(defmacro if-bind ((var condition) then &optional else)
  `(let ((,var ,condition))
     (if ,var
       ,then
       ,else)))

(defmacro when-bind ((var condition) &body body)
  `(let ((,var ,condition))
     (when ,var
       ,@body)))

;;;; Debugging

(defun expand-display (form)
  (with-gensyms (values)
    `(let ((,values (multiple-value-list ,form)))
       (format *debug-io* ,(format nil "~w => ~~w~%" form) (car ,values))
       (values-list ,values))))

(defvar *debug* t)
(defvar *break-on-display* nil)

(defvar *break-on-debugging* nil)
(defvar *silence-debug-warning* nil)

(defmacro dbg (&body body)
  `(cond (*debug* (progn (unless *silence-debug-warning*
                           (warn (format nil "~w" `(debugging ,',@body))))
                    ,@body))
         (t (when *break-on-debugging*
              (error "debugging form")))))

(defmacro display (&rest forms)
  `(if *debug*
     (multiple-value-prog1 (progn ,@(mapcar #'expand-display forms))
       (when *break-on-display* (break))
       (terpri *debug-io*))
     (progn ,@forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Interactive Interface
(defmacro with-construction ((system-form) &rest body)
  `(let ((*current-construction* ,system-form))
     ,@body))

(defmacro try-with (attribute value-form)
  `(set-construction-parameter ',attribute ,value-form))

(defmacro !> (&rest elements)
  `(%!> ,@(reverse elements)))

;; Helper for !>
(defmacro %!> (&rest elements)
  (if (cdr elements)
      `(,@(car elements) (%!> ,@(cdr elements)))
      `(,@(car elements))))

(defmacro sig ((&rest input) arrow (&rest output))
  (assert (eql arrow '->))
  `(make-signature ',input ',output))

(deftype tlambda-arrow () '(eql ->))
(deftype xlambda-arrow () '(eql =>))
(deftype literal-arrow () '(eql ~>))

(deftype transformation-arrow () '(or tlambda-arrow xlambda-arrow literal-arrow))

(defmacro transformation (((&rest input-lambda-list) arrow (&rest output) &key source name) eqmark implementation)
  (check-type arrow transformation-arrow)
  (check-type eqmark (eql ==))
  (let* ((input-lambda-list (remove-if-not #'symbolp input-lambda-list))
	 (output (remove-if-not #'symbolp output))
	 (input (process-input-list input-lambda-list)))
    (etypecase arrow
            ;; -> tlambda
      (tlambda-arrow `(let ((sig (make-signature ',input ',output)))
			(make-instance 'transformation
				       :name ',name
				       :signature sig
				       :implementation (tlambda ,input-lambda-list ,output ,implementation)
				       :source ,(if source
						    `(quote ,source)
						    `(quote ,implementation)))))
      ;; => xlambda
      (xlambda-arrow `(let ((sig (make-signature ',input ',output)))
			(make-instance 'transformation
				       :name ',name
				       :signature sig
				       :implementation (xlambda ,input-lambda-list ,output ,implementation))))
      ;; ~> literal implementation
      (literal-arrow `(let ((sig (make-signature ',input ',output)))
			(make-instance 'transformation
				       :name ',name
				       :signature sig
				       :implementation ,(make-instance 'implementation
								       :module (package-name
										(symbol-package implementation))
								       :name (symbol-name implementation))))))))

(defmacro deftoplevel (name (type) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (define-toplevel-thing ',name ',type (progn ,@body))))

(defmacro deftransformation (name ((&rest input) arrow (&rest output)) &body implementation)
  (check-type arrow transformation-arrow)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let* ((transformation (transformation ((,@input) ,arrow (,@output) :source (,name ,@input) :name ,name) == (progn ,@implementation)))
	    (implementation (transformation-implementation transformation)))
       ;; If transformation has a function implementation, associate it with transformation name as if by DEFUN,
       ;; and replace implementation with the symbol.
       ;; (when (functionp implementation)
       ;; 	 (setf (symbol-function ',name) implementation)
       ;; 	 (setf (transformation-implementation transformation) ',name))
       (deftoplevel ,name (:transformation) transformation))))

(defmacro component (transformations &key operation target args)
  `(make-instance 'component :transformations (list ,@transformations)
		  ,@(and target (list :target target))
		  ,@(and operation (list :operation operation))
		  ,@(and args (list :args args))))

(defmacro defcomponent (name (&rest transformations))
  `(deftoplevel ,name (:component) (make-instance 'component :transformations (list ,@transformations))))

;; Make a relation
;; Example: (relation (a b c) (1 2 3) (4 5 6))
(defmacro relation ((&rest attributes) &rest tuple-values)
  `(make-relation (list ,@(loop for values in tuple-values
			     collect `(make-tuple (list ,@(loop for attribute in attributes
							     for value in values
							     collect `(list ',attribute ,value))))))))

;; Make a tuple.
;; Example: (tuple (a 1) (b 2) (c 3))
(defmacro tuple (&rest parameters)
  `(make-tuple (list ,@(mapcar (lambda (param)
				 (destructuring-bind (attribute value) param
				   `(list ',attribute ,value)))
			       parameters))))

;; Make a relation from tuples.
;; Example: (rel (tuple (a 1) (b 2)) (tuple (a 3) (b 4)))
(defmacro rel (&rest tuple-forms)
  `(make-relation (list ,@tuple-forms)))

;; Make a tuple.
;; Example: (tpl (a b c) 1 2 3)
(defmacro tpl ((&rest attributes) values)
  `(make-tuple (list ,@(loop for attribute in attributes
			  for value in values
			  collect `(list ',attribute ,value)))))

(defmacro schema (description &rest parameters)
  `(make-instance 'schema
		  :description ,description
		  :parameters (list ,@(mapcar (lambda (parameter-spec)
						(destructuring-bind (name &optional description  type) parameter-spec
						  `(make-instance 'parameter :name ',name :description ,description :type ,type)))
					      parameters))))

(defmacro defschema (name description &rest parameters)
  `(deftoplevel ,name (:schema)
     (schema ,description ,@parameters)))

(defmacro sys ((&rest components))
  `(make-instance 'system :components (list ,@components)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Constraints

(defmacro defconstraint-system (name constraint-definitions &key schema)  
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (let ((system (constraint-system ,constraint-definitions)))
       (awhen ,schema
	 (setf (system-schema system) it))
       (deftoplevel ,name (:system) system))))

(defmacro constraint-system (constraint-definitions)
  `(make-instance 'system :components ,(expand-constraint-definitions constraint-definitions)))

;; TOOD: rename this to process-tuple-lambda-list
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun process-input-list (input)
    (let* ((all-pos (position '&all input))
	   (attrs (if all-pos
		      (subseq input 0 all-pos)
		      input))
	   (all-var (when all-pos
		      (nth (1+ all-pos) input))))
      (values attrs all-var))))

(test process-input-list
  (multiple-value-bind (attrs all-var) (process-input-list '(a b c &all all))
    (is (equal '(a b c) attrs))
    (is (eql 'all all-var))))

;; Convenience function for manipulating tuples.
(defmacro tfn ((&rest tuple-lambda-list) &body body)
  (multiple-value-bind (attrs all-var) (process-input-list tuple-lambda-list)
    (let ((tuple (or all-var (gensym "TUPLE"))))
      `(lambda (,tuple)
	 (symbol-macrolet
	     (,@(loop for in in attrs
		   collect `(,in (tref ',in ,tuple))))
	   ,@body)))))

;; Creates a function which take a data map of INPUT attributes and returns a data map of INPUT + OUTPUT attributes.
;; Code in BODY should return multiple values corresponding to the attributes of OUTPUT, which will be used to construct the resulting data map.
(defmacro tlambda ((&rest input) (&rest output) &body body)
  (multiple-value-bind (input-attrs all-var) (process-input-list input)
    (let ((tuple (or all-var (gensym "TUPLE")))
	  (new-tuple (gensym "NEW-TUPLE"))
	  (out (gensym "OUTPUT")))
      `(lambda (,tuple)
	 (symbol-macrolet
	     (,@(loop for in in input-attrs
		   collect `(,in (tref ',in ,tuple))))
	   (let ((,new-tuple ,tuple)
		 (,out (multiple-value-list (progn ,@body))))
	     (declare (ignorable ,out))
	     ,@(loop for attribute in output
		  collect `(adjoinf ,new-tuple ',attribute (pop ,out)))
	     ,new-tuple))))))

;; Creates a function which take a data map of INPUT attributes and returns a relation of INPUT + OUTPUT attributes.
;; Code in BODY should return a list of lists, one for each data map to be added to the resulting relation.
(defmacro xlambda ((&rest input) (&rest output) &body body)
  (let ((tuple (gensym "TUPLE"))
	(out (gensym "OUTPUT")))
    `(lambda (,tuple)
       (declare (ignorable ,tuple))
       (symbol-macrolet
	   (,@(loop for in in input
		 collect `(,in (tref ',in ,tuple))))
	 (let ((,out (progn ,@body)))
	   (declare (ignorable ,out))
	   (build-relation ,tuple ',output ,out))))))

;; Creates a function which take a data map of INPUT attributes and returns a relation of INPUT + OUTPUT attributes.
;; Code in BODY should return a relation -- whose heading must be correct.
(defmacro rlambda ((&rest input) (&rest output) &body body)
  (declare (ignore output))
  (multiple-value-bind (input-attrs all-var) (process-input-list input)
    (let ((tuple (or all-var (gensym "TUPLE"))))
      `(lambda (,tuple)
	 (symbol-macrolet
	     (,@(loop for in in input-attrs
		   collect `(,in (tref ',in ,tuple))))
	   (progn ,@body))))))

#+(or)
(test rlambda
  "Test rlambda."
  (apply-transformation (rlambda ((a b c &all tuple) (d))
			    (relation (rename ((z q)) tuple)))
			(relation (a b c z)
				  (1 2 3 9)))
  )

(defmacro where (((&rest tuple-lambda-list) &body body) relation-form)
  `(restrict (tfn (,@tuple-lambda-list) ,@body) ,relation-form))

(test where "Test WHERE macro."
      (is (same (relation (a b c)
			  (4 5 6))
		(where ((b) (= b 5))
		       (relation (a b c)
				 (1 2 3)
				 (4 5 6)
				 (7 8 9))))))

(defmacro rename ((&rest pairs) attributed)
  `(rename-attributes ',pairs ,attributed))

