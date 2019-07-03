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

(defmacro with-gensyms* (vars &body body)
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

(defvar *dval* nil "Value in which to stash debugging values via DBREAK.")

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

(defmacro dbreak (value-form)
  `(progn
     (setq *dval* ,value-form)
     (break)))

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
  (let* ((input-lambda-list (remove-if-not (lambda (x) ;; TODO: probably should do this filtering after PARSE-TUPLE-LAMBDA
					     (or (symbolp x)
						 (typep x '(cons symbol))))
					   input-lambda-list))
	 (reducer? (not (not (member '&acc input-lambda-list))))
	 (output (remove-if-not #'symbolp output))
	 (input (parse-tuple-lambda input-lambda-list)))
    (etypecase arrow
      ;; -> tlambda
      (tlambda-arrow `(let ((sig (make-signature ',input ',output ,reducer?)))
			(make-instance 'transformation
				       :name ',name
				       :signature sig
				       :implementation (tlambda ,input-lambda-list ,output ,implementation)
					       :source ,(if source
							    `(quote ,source)
							    `(quote ,implementation)))))
      ;; => xlambda
      (xlambda-arrow `(let ((sig (make-signature ',input ',output ,reducer?)))
			(make-instance 'transformation
				       :name ',name
				       :signature sig
				       :source ',source
				       :implementation (xlambda ,input-lambda-list ,output ,implementation))))
      ;; ~> literal implementation
      (literal-arrow `(let ((sig (make-signature ',input ',output ,reducer?)))
			(make-instance 'transformation
				       :name ',name
				       :signature sig
				       :source ',source
				       :implementation ,(make-instance 'implementation
								       :module (package-name
										(symbol-package implementation))
								       :name (symbol-name implementation))))))))

;; Like TRANSFORMATION but don't quote INPUT/OUTPUT, which must be bound in containing scope.
;; FIXME: refactor to share code with TRANSFORMATION.
(defmacro transformation* (((&rest input-lambda-list) arrow (&rest output) &key source name) eqmark implementation)
  (check-type arrow transformation-arrow)
  (check-type eqmark (eql ==))
  (let* ((input-lambda-list (remove-if-not #'symbolp input-lambda-list))
	 (reducer? (not (not (member '&acc input-lambda-list))))
	 (input (parse-tuple-lambda input-lambda-list)))
    (etypecase arrow
            ;; -> tlambda
      (tlambda-arrow `(let ((sig (make-signature (remove-if-not #'symbolp (list ,@input)) (remove-if-not #'symbolp (list ,@output)) ,reducer?)))
			(make-instance 'transformation
				       :name ',name
				       :signature sig
				       :implementation (%tlambda ,input-lambda-list ,output ,implementation)
				       :source (let ((substitutions (list ,@(loop for v in (append input output)
									       collect `(cons ',v ,v))))
						     (effective-source ,(if source
									    `(quote ,source)
									    `(quote ,implementation))))
						 (typecase effective-source
						   (list (cons (car effective-source) (sublis substitutions (cdr effective-source))))
						   ((or list symbol) (sublis substitutions effective-source))
						   (t effective-source))))))
      ;; => xlambda
       (xlambda-arrow `(let ((sig (make-signature (remove-if-not #'symbolp (list ,@input)) (remove-if-not #'symbolp (list ,@output)) ,reducer?)))
			(make-instance 'transformation
				       :name ',name
				       :source (list ',source ,@(remove-if-not #'symbolp input))
				       :signature sig
				       :implementation (%xlambda ,input-lambda-list ,output ,implementation))))

       ;; TODO: (or not)
       ;; ~> literal implementation
       #+(or)
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
	    ;;(implementation (transformation-implementation transformation))
	    )
       ;; If transformation has a function implementation, associate it with transformation name as if by DEFUN,
       ;; and replace implementation with the symbol.
       ;; (when (functionp implementation)
       ;; 	 (setf (symbol-function ',name) implementation)
       ;; 	 (setf (transformation-implementation transformation) ',name))
       (deftoplevel ,name (:transformation) transformation))))

(defmacro component (transformations &key operation target args)
  `(make-instance 'component :transformations (mapcar #'find-transformation (list ,@transformations))
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
       (setf (system-name system) ',name)
       (awhen ,schema
	 (setf (system-schema system) it))
       (deftoplevel ,name (:system) system))))

(defun parse-lambda (lambda-list marker-symbols)
  (let ((result (list (list 'vars))))
    (loop for sym in lambda-list
       if (member sym marker-symbols)
       do (progn (setf (cdar result) (nreverse (cdar result)))
		 (push (cons sym nil) result))
       else do (push sym (cdr (car result))))
    (setf (cdar result) (nreverse (cdar result)))
    result))

(test parse-lambda
  (is (equal (parse-lambda '(a b &key c d &optional e f) '(&key &optional))
	     '((&optional e f) (&key c d) (vars a b)))))

(defun parse-tuple-lambda (lambda-list)
  (let ((parsed (parse-lambda lambda-list '(&acc &all))))
    (values (cdr (assoc 'vars parsed))
	    (cdr (assoc '&acc parsed))
	    (cdr (assoc '&all parsed)))))

(test parse-tuple-lambda
  (is (equal (multiple-value-list (parse-tuple-lambda '(a b c &acc d (e 9) &all f)))
	     '((a b c) (d (e 9)) (f)))))

(defun arg-eval (arg)
  "Minimal evaluation of constraint args, so we can use literal symbols as values without interpreting them as variables to bind."
  (typecase arg
    ((cons (eql quote)) (cadr arg))
    (t arg)))

(defmacro tref (attribute tuple)
  "Get value of ATTRIBUTE in TUPLE."
  `(@ ,tuple (arg-eval ,attribute)))

;; Convenience function for manipulating tuples.
(defmacro tfn ((&rest tuple-lambda-list) &body body)
  (multiple-value-bind (attrs acc-attrs all-var) (parse-tuple-lambda tuple-lambda-list)
    (check-type acc-attrs null)
    (let ((tuple (or all-var (gensym "TUPLE"))))
      `(lambda (,tuple)
	 (symbol-macrolet
	     (,@(loop for in in attrs
		   collect `(,in (tref ',in ,tuple))))
	   ,@body)))))

;; Creates a function which take a data map of INPUT attributes and returns a data map of INPUT + OUTPUT attributes.
;; Code in BODY should return multiple values corresponding to the attributes of OUTPUT, which will be used to construct the resulting data map.
;; Essentially, tuple -> tuple
(defmacro tlambda ((&rest input) (&rest output) &body body)
  (multiple-value-bind (input-attrs acc-attrs all-var) (parse-tuple-lambda input)
    (let ((tuple (or all-var (gensym "TUPLE")))
	  (acc (gensym "ACC"))
	  (new-tuple (gensym "NEW-TUPLE"))
	  (out (gensym "OUTPUT")))
      `(lambda (,tuple ,acc)
	 (symbol-macrolet
	     (,@(loop for in in input-attrs
		   collect `(,in (tref ',in ,tuple)))
	      ,@(loop for acc-attr in acc-attrs
		   collect `(,acc-attr ,(etypecase acc-attr
					  (symbol
					   `(tref ',acc-attr ,acc))
					  ((cons symbol)
					   `(multiple-value-bind (val presentp)
						(and ,acc
						     (tref ',(car acc-attr) ,acc))
					      (if presentp
						  val
						  ,(cdr acc-attr))))))))
	   (let ((,new-tuple ,tuple)
		 (,out (multiple-value-list (progn ,@body))))
	     (declare (ignorable ,out))
	     ,@(loop for attribute in output
		  collect `(adjoinf ,new-tuple ',attribute (pop ,out)))
	     ,new-tuple))))))

;; Like TLAMBDA but with unquoted attributes. This means input/output names can be supplied at execution time (bound in lexical env).
(defmacro %tlambda ((&rest input) (&rest output) &body body)
  (multiple-value-bind (input-attrs all-var) (parse-tuple-lambda input)
    (let ((tuple (or all-var (gensym "TUPLE")))
	  (acc (gensym "ACC"))
	  (new-tuple (gensym "NEW-TUPLE"))
	  (out (gensym "OUTPUT"))
	  (var-pairs (loop for v in input
			collect (cons v (gensym (symbol-name v))))))
      `(lambda (,tuple ,acc)
	 (let (,@(mapcar (lambda (var-pair)
			   (list (cdr var-pair) (car var-pair)))
			 var-pairs))
	   (symbol-macrolet
	       (,@(loop for in in input-attrs
		     for v = (assoc in var-pairs)
		       when (symbolp (cdr v))
		     ;collect `(,in (tref ,(cdr (assoc in var-pairs)) ,tuple))))
		     collect `(,in (if (symbolp ,(cdr v))
				       (tref ,(cdr v) ,tuple)
				       ,(cdr v)))))
	     (let ((,new-tuple ,tuple)
		   (,out (multiple-value-list (progn ,@body))))
	       (declare (ignorable ,out))
	       ,@(loop for attribute in output
		    collect `(adjoinf ,new-tuple ,attribute (pop ,out)))
	       ,new-tuple)))))))

;; Creates a function which take a data map of INPUT attributes and returns a relation of INPUT + OUTPUT attributes.
;; Code in BODY should return a list of lists, one for each data map to be added to the resulting relation.
;; Essentially, tuple -> relation
(defmacro xlambda ((&rest input) (&rest output) &body body)
  (let ((tuple (gensym "TUPLE"))
	(acc (gensym "ACC"))
	(out (gensym "OUTPUT")))
    `(lambda (,tuple ,acc)
       (declare (ignorable ,tuple))
       (symbol-macrolet
	   (,@(loop for in in input
		 collect `(,in (tref ',in ,tuple))))
	 (let ((,out (progn ,@body)))
	   (declare (ignorable ,out))
	   (build-relation ,tuple ',output ,out))))))

;; Like XLAMBDA but with unquoted attributes. This means input/output names can be supplied at execution time (bound in lexical env).
(defmacro %xlambda ((&rest input) (&rest output) &body body)
  (let ((tuple (gensym "TUPLE"))
	(acc (gensym "ACC"))
	(out (gensym "OUTPUT"))
	(var-pairs (loop for v in input
		      collect (cons v (gensym (symbol-name v))))))
    `(lambda (,tuple ,acc)
       (declare (ignorable ,tuple))
       (let (,@(mapcar (lambda (var-pair)
			 (list (cdr var-pair) (car var-pair)))
		       var-pairs))
	 (symbol-macrolet
	     (,@(loop for in in input
		   collect `(,in (tref ,(cdr (assoc in var-pairs)) ,tuple))))
	   (let ((,out (progn ,@body)))
	     (declare (ignorable ,out))
	     (build-relation ,tuple (list ,@output) ,out)))))))

;; Creates a function which take a data map of INPUT attributes and returns a relation of INPUT + OUTPUT attributes.
;; Code in BODY should return a relation -- whose heading must be correct.
;; Essentially, tuple -> relation
(defmacro rlambda ((&rest input) (&rest output) &body body)
  (declare (ignore output))
  (multiple-value-bind (input-attrs all-var) (parse-tuple-lambda input)
    (let ((tuple (or all-var (gensym "TUPLE")))
	  (acc (gensym "ACC")))
      `(lambda (,tuple ,acc)
	 (symbol-macrolet
	     (,@(loop for in in input-attrs
		   collect `(,in (tref ',in ,tuple))))
	   (progn ,@body)))))) 

(test rlambda
  "Test rlambda."
  (is (same (apply-transformation (rlambda (a b c &all tuple) (q)
				    (relation (a b c z q)
					      (a b c 9 (degree tuple))))
				  (relation (a b c z)
					    (1 2 3 9)
					    (4 5 6 9)
					    (7 8 9 1)))
	    (relation (a b c q z)
		      (1 2 3 4 9)
		      (4 5 6 4 9)))))

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

