(defpackage :orient
  (:use :common-lisp :it.bese.FiveAm)
  (:export :apply-transformation :ask :attributes :component :tuple :tuples :tuple-pairs :defschema :deftransformation :deftransformation=
	   :tref :join :make-relation
	   :make-signature
	   :orient-tests :plan :plan-for :rel :relation :remove-attributes :remv :rename :same
	   :schema-parameters :schema-description :sig :signature :signature-input :signature-output :solve :solve-for :sys :system :system-data
	   :tpl :transformation
	   :where :-> :=== :== &all !>))

(in-package "ORIENT")
(def-suite orient-suite :description "Test the orient package.")
(in-suite orient-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros
(defmacro remv (attributes attributed)
  `(remove-attributes ',attributes ,attributed))

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

(defmacro transformation (((&rest input-lambda-list) arrow (&rest output)) eqmark implementation)
  (assert (eql arrow '->))
  (let ((input (process-input-list input-lambda-list)))
    (ecase eqmark
      (= `(let ((sig (make-signature ',input ',output)))
	    (make-instance 'transformation :signature sig :implementation (rlambda ,input-lambda-list ,output ,implementation))))
      (== `(let ((sig (make-signature ',input ',output)))
	     (make-instance 'transformation :signature sig :implementation (tlambda ,input-lambda-list ,output ,implementation))))
      (=== `(let ((sig (make-signature ',input ',output)))
	      (make-instance 'transformation :signature sig :implementation ,implementation))))))

;; Idea: encode the choice of transformation syntax in the arrow. e.g. -> vs =>, etc.
;; Uses TLAMBDA
(defmacro deftransformation (name ((&rest input) arrow (&rest output)) &body implementation)
  (assert (eql arrow '->))
  `(eval-when (:load-toplevel :execute)
     (progn (defparameter ,name (transformation ((,@input) -> (,@output)) == (progn ,@implementation))))))

;; Uses RLAMBDA
(defmacro deftransformation= (name ((&rest input) arrow (&rest output)) &body implementation)
  (assert (eql arrow '->))
  `(eval-when (:load-toplevel :execute)
     (progn (defparameter ,name (transformation ((,@input) -> (,@output)) = (progn ,@implementation))))))

(defmacro component (transformations)
  `(make-instance 'component :transformations (list ,@transformations)))

(defmacro defcomponent (name (&rest transformations))
  `(defparameter ,name (make-instance 'component :transformations (list ,@transformations))))

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

;; Make a relation. Shorthand for RELATION
;; Example: (rel (a b c) (1 2 3) (4 5 6))
(defmacro rel ((&rest attributes) &rest tuple-values)
  `(relation (,@attributes) ,@tuple-values))

;; Make a tuple.
;; Example: (tpl (a b c) 1 2 3)
(defmacro tpl ((&rest attributes) &rest values)
  `(make-tuple (list ,@(loop for attribute in attributes
			     for value in values
			  collect `(list ',attribute ,value)))))

(defmacro where (((&rest tuple-lambda-list) &body body) relation-form)
  `(restrict (tfn (,@tuple-lambda-list) ,@body) ,relation-form))

(test where "Test WHERE macro."
      (is (same (rel (a b c)
		     (4 5 6))
		(where ((b) (= b 5))
		       (rel (a b c)
			    (1 2 3)
			    (4 5 6)
			    (7 8 9))))))

(defmacro defschema (name description &rest parameters)
  `(defparameter ,name
     (make-instance 'schema
		    :description ,description
		    :parameters (list ,@(mapcar (lambda (parameter-spec)
						  (destructuring-bind (name description &optional type) parameter-spec
						    `(make-instance 'parameter :name ',name :description ,description :type ,(or type ""))))
						parameters)))))

(defmacro sys ((&rest components))
  `(make-instance 'system :components (list ,@components)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TOOD: rename this to process-tuple-lambda-list
(defun process-input-list (input)
  (let* ((all-pos (position '&all input))
	 (attrs (if all-pos
		    (subseq input 0 all-pos)
		    input))
	 (all-var (when all-pos
		    (nth (1+ all-pos) input))))
    (values attrs all-var)))

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
	   (let ((,new-tuple (make-tuple (tuple-pairs ,tuple)))
		 (,out (multiple-value-list (progn ,@body))))	   
	     ,@(loop for attribute in output
		  collect `(setf (tref ',attribute ,new-tuple) (pop ,out)))
	     ,new-tuple))))))

;; Creates a function which take a data map of INPUT attributes and returns a relation of INPUT + OUTPUT attributes.
;; Code in BODY should return a list of lists, one for each data map to be added to the resulting relation.
(defmacro xlambda ((&rest input) (&rest output) &body body)
  (let ((tuple (gensym "TUPLE"))
	(out (gensym "OUTPUT"))
	(supplied-pairs (gensym "PAIRS")))
    `(lambda (,tuple)
       (symbol-macrolet
	   (,@(loop for in in input
		collect `(,in (tref ',in ,tuple))))
	 (let ((,out (progn ,@body))
	       (,supplied-pairs (tuple-pairs ,tuple)))
	   (build-relation ,supplied-pairs ',output ,out))))))

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

(defmacro rename ((&rest pairs) attributed)
  `(rename-attributes ',pairs ,attributed))

;; 
(defclass tuple ()
  ((hash-table :initform (make-hash-table) :accessor tuple-hash-table)))

(defmethod print-object ((d tuple) (stream t))
  (format stream "<TUPLE ~S>" (sort (tuple-pairs d) #'string< :key #'car)))

(defun make-tuple (&optional pairs)
  (let* ((tuple (make-instance 'tuple))
	 (h (tuple-hash-table tuple)))
    (loop for (k v) in pairs do (setf (gethash k h) v))
    tuple))

(defmethod tuple-pairs ((tuple tuple) &key dotted)
  (loop
     for attribute being the hash-keys of (tuple-hash-table tuple)
     for val being the hash-values of (tuple-hash-table tuple)
     collect (if dotted
		 (cons attribute val)
		 (list attribute val))))

(defmethod tref ((attribute t) (tuple tuple))
  "Get value of ATTRIBUTE in TUPLE."
  (gethash attribute (tuple-hash-table tuple)))

(defmethod set-tref ((attribute t) (tuple tuple) (value t))
  "Set value of ATTRIBUTE to VALUE in TUPLE."
  (setf (gethash attribute (tuple-hash-table tuple)) value))

(defsetf tref set-tref)

(defmethod trem ((attribute t) (tuple tuple))
  "Remove ATTRIBUTE from TUPLE"
  (remhash attribute (tuple-hash-table tuple)))

(defclass relation () ())

(defclass simple-relation (relation)
  ((attributes :initarg :attributes :accessor attributes)
   (tuples :initarg :tuples :initform nil :accessor tuples)))

(defgeneric attributes (tuple)
  (:method ((d tuple))
    (loop for attribute being the hash-keys of (tuple-hash-table d)
       collect attribute))
  (:method ((r relation))
    (and (first (tuples r))
	 (attributes (first (tuples r))))))

(defun set-equal (a b &key (test #'eql)) (and (subsetp  a b :test test) (subsetp b a :test test)))

(defgeneric make-relation (tuples)
  (:documentation
   "Create relation from tuples, removing duplicates. Returns NIL if tuples don't have all have same attributes.")
  ;; Rather than return NIL, should mismatch be an error?
  (:method ((tuples list))
    (let* ((first (first tuples))
	   (attributes (and first (attributes first))))
      (and (every (lambda (x) (set-equal (attributes x) attributes))
		  (cdr tuples))
	   ;; TODO: implement and respect at least primary keys.
	   (make-instance 'simple-relation :tuples (remove-duplicates tuples :test #'same) :attributes attributes)))))

(defgeneric cardinality (relation)
  (:method ((r relation))
    (length (tuples r))))

(defgeneric degree (attributed)
  (:method ((tuple tuple))
    (hash-table-count (tuple-hash-table tuple)))
  (:method ((r relation))
    (length (attributes r))))

;; TODO: Pitiher name, but can't use REMOVE, since it's taken by CL.
(defgeneric remove-attributes (atributes attributed)
  (:method ((attributes list) (tuple tuple))
    (let ((new-tuple (duplicate tuple)))
      (loop for attr in attributes do (trem attr new-tuple))
      new-tuple))
  (:method ((attributes list) (r relation))
    (make-relation (mapcar (lambda (x) (remove-attributes attributes x)) (tuples r)))))

(defun classify-set-elements (a b)
  ;; TODO: This can be optimized to make only a single pass over each set.
  (let* ((shared (intersection a b))
	 (a-only (set-difference a shared))
	 (b-only (set-difference b shared))
	 (all (union a b)))
    (values a-only b-only shared all)))

;; Helper function so JOIN can avoid expensive creation of relations which will be immediately stripped for their contained tuples.
(defgeneric %join (relation-a relation-b)
  (:method ((a tuple) (b tuple))
    (let* ((a-attributes (attributes a))
	   (b-attributes (attributes b)))
      (let ((shared (intersection a-attributes b-attributes)))
	(let ((matchp (every (lambda (attr)
			       (same (tref attr a) (tref attr b)))
			     shared)))
	  (when matchp
	    (make-tuple (union (tuple-pairs a) (tuple-pairs b) :test (lambda (a b) (eql (car a) (car b))))))))))
  (:method ((a tuple) (b relation))
    (loop for tuple in (tuples b)
       for maybe-tuple = (join a tuple)
       when maybe-tuple
       collect maybe-tuple))
  (:method ((a relation) (b tuple))
    (%join b a))
  (:method ((a relation) (b relation))
    (reduce (lambda (acc tuple)
	      (nconc acc (%join tuple b)))
	    (tuples a)
	    :initial-value '())))

(defgeneric join- (a b)
  (:method ((a tuple) (b tuple))
    (%join a b))
  (:method ((a tuple) (b relation))
    (make-relation (%join a b)))
  (:method ((a relation) (b tuple))
    (join- b a))
  (:method ((a relation) (b relation))
    (make-relation (%join a b))))

(defun join (&rest things) (reduce #'join- things))

(defgeneric duplicate (thing)
  (:method ((d tuple))
    (make-tuple (tuple-pairs d)))
  (:method ((r relation))
    (make-relation (mapcar #'duplicate (tuples r)))))

(defgeneric rename-attributes (old-new-pairs attributed)
  (:method ((pairs list) (r relation))
    (make-relation (mapcar (lambda (tuple) (rename-attributes pairs tuple))
			   (tuples r))))
  (:method ((pairs list) (tuple tuple))
    (let ((new-tuple (make-tuple)))
      (loop for attr in (attributes tuple)
	 do (let ((pair (assoc attr pairs)))
	      (cond (pair
		     (setf (tref (cadr pair) new-tuple)
			   (tref (car pair) tuple)))
		    (t (setf (tref attr new-tuple) (tref attr tuple))))))
      new-tuple)))

(test rename-attributes "Test RENAME-ATTRIBUTES."
      (is (same (tuple (d 1) (e 2) (c 3))
		(rename-attributes '((a d) (b e))
				   (tuple (a 1) (b 2) (c 3)))))

      (is (same (rel (d e c) (1 2 3) (4 5 6))
		(rename-attributes '((a d) (b e))
				   (rel (a b c) (1 2 3) (4 5 6))))))

;; Example, filter tuples where b is not 5:
;; (restrict (tfn (b) (= b 5)) asdf)
(defgeneric restrict (tuple-predicate relation)
  (:method ((tpred function) (relation relation))
    (make-relation (remove-if-not tpred (tuples relation)))))

(test restrict "Test RESTRICT."
      (is (same (rel (a b c) (4 5 6))
		(restrict (tfn (b) (= b 5))
			  (rel (a b c) (1 2 3) (4 5 6) (7 8 9))))))

(defgeneric project (attributes attributed)
  (:method ((attributes list) (tuple tuple))
    (make-tuple (remove-if-not (lambda (attribute) (member attribute attributes)) (tuple-pairs tuple) :key #'car)))
  (:method ((attributes list) (relation relation))
    (make-relation (mapcar (lambda (tuple) (project attributes tuple)) (tuples relation)))))

(test project-tuple "Test PROJECT on tuple."
      (is (same (tuple (b 2) (c 3))
		(project '(b c) (tuple (a 1) (b 2) (c 3))))))

(test project-relation "Test PROJECT on relation."
      (is (same (relation (b c) (2 3))
		(project '(b c) (relation (a b c)
					  (1 2 3)
					  (9 2 3))))))

(defclass parameter ()
  ((name :initarg :name :initform (error "name missing") :accessor parameter-name)
   (description :initarg :description :accessor parameter-description)
   (type :initarg :type :initform nil :accessor parameter-type)))

(defclass schema ()
  ((description :initarg :description :initform nil :accessor schema-description)
   (parameters :initarg :parameters :initform '() :accessor schema-parameters)))

(defclass signature ()
  ((input :initarg :input :initform '() :accessor signature-input)
   (output :initarg :output :initform '() :accessor signature-output)))

(defun make-signature (input output)
  (make-instance 'signature :input input :output output))

(defun pruned-signature (sig)
  "Return a new signature, with output which is also input pruned, since this will be trivially provided."
  (let* ((input (signature-input sig))
	 (pruned-output (set-difference (signature-output sig) input)))
    (if pruned-output
	(make-signature input pruned-output)
	sig)))

(defmethod print-object ((sig signature) (stream t))
  (format stream "(SIG ~S -> ~S)" (signature-input sig) (signature-output sig)))

(defmethod sig-subset-p ((s signature) (other signature))
  "Returns true if s is a subset of other."
  (and (subsetp (signature-input s) (signature-input other))
       (subsetp (signature-output s) (signature-output other))))

(defun sig-equal (a b) (and (sig-subset-p a b) (sig-subset-p b a)))

(defmethod provides-p ((s signature) (name symbol))
  "Returns true if name is an output of signature."
  (member name (signature-output s)))

(defmethod provides ((output list) (s signature))
  "Returns the names in OUTPUT which are provided as output of signature, S."
  (intersection (signature-output s) output))

(defclass transformation ()
  ((signature :initarg :signature :initform (make-signature '() '()) :accessor transformation-signature)
   (implementation :initarg :implementation :initform nil :accessor transformation-implementation)))

(defmethod print-object ((trans transformation) (stream t))
  (format stream "(TRANSFORMATION ~S === ~S)" (transformation-signature trans) (transformation-implementation trans)))

(defun identity-transformation () (make-instance 'transformation))

(defclass component ()
  ((transformations :initarg :transformations :initform '() :accessor component-transformations)))

(defmethod print-object ((comp component) (stream t))
  (format stream "(COMPONENT ~S)" (component-transformations comp)))

(defclass problem ()
  ((signature :initarg :signature :initform (make-signature '() '()) :accessor problem-signature)))

(defclass system ()
  ((schema :initarg :schema :initform nil :accessor system-schema)
   (components :initarg :components :initform '() :accessor system-components)
   (data :initarg :data :initform '() :accessor system-data)))

(defmethod print-object ((sys system) (stream t))
  (format stream "(sys ~S :schema ~S)" (system-components sys) (system-schema sys)))

(defclass engine () ())

(defgeneric same (a b)
  (:method
      ;; Things of different type are never the same.
      ;; Things of types without specialization are the same if they are equal.
      ((a t) (b t))
    (and (equal (type-of a) (type-of b))
	 (equal a b)))
  (:method ((a tuple) (b tuple))
    (and (set-equal (attributes a) (attributes b))
	 (every (lambda (attr) (same (tref attr a) (tref attr b))) (attributes a))))
  (:method ((a signature) (b signature))
    (sig-equal a b))
  (:method ((a transformation) (b transformation))
    (and (same (transformation-signature a) (transformation-signature b))
	 (equal (transformation-implementation a) (transformation-implementation b))))
  (:method ((a component) (b component))
    ;; FIXME: use set-equal
    (and (subsetp (component-transformations a) (component-transformations b) :test #'same)
	 (subsetp (component-transformations b) (component-transformations a) :test #'same)))
  (:method ((a relation) (b relation))
    (set-equal (tuples a) (tuples b) :test #'same))
  (:method ((a list) (b list))
    (and (eql (length a) (length b))
	 (every #'same a b))))

(defgeneric satisfies-input-p (a b)
  ;; FIXME: Make type of A ensure ATTRIBUTES.
  (:method ((a t) (b t)) nil)
  (:method ((a t) (b transformation)) (satisfies-input-p a (transformation-signature b)))
  (:method ((a t) (b signature)) (subsetp (signature-input b) (attributes a))) ;; FIXME: new superclass of types with attributes.
  )

(defgeneric ensure-relation (potential-relation)
  (:method ((r relation)) r)
  (:method ((tuple tuple))
    (make-relation (list tuple)))
  (:method ((list list))
    (check-type list (cons tuple)) ;; Not exhaustive, but a good sanity check.
    (make-relation list)))

(defgeneric combine-potential-relations (a b)
  (:method ((a relation) (b relation))
    ;; FIXME: Check that headings are compatible.
    (make-relation (append (tuples a) (tuples b))))
  (:method ((a tuple) (b tuple))
    (make-relation (list a b)))
  (:method ((a tuple) (b list))
    (make-relation (cons a b)))
  (:method ((a list) (b tuple))
    (combine-potential-relations b a))
  (:method ((a list) (b list))
    ;; We assume aruguments to COMBINE-POTENTIAL-RELATIONS can be destructively modified.
    (make-relation (nconc a b)))
  (:method ((a tuple) (b relation))
    ;; FIXME: Check that headings are compatible.
    (make-relation (cons a (tuples b))))
  (:method ((a relation) (b tuple))
    (combine-potential-relations b a))
  (:method ((a list) (b relation))
    (make-relation (append a (tuples b))))
  (:method ((a relation) (b list))
    (combine-potential-relations b a)))

;; TODO: Transformation should fail if any output changes the value of an input.
(defgeneric apply-transformation (transformation tuple)
  (:method ((transformation transformation) (tuple tuple))
    (assert (satisfies-input-p tuple transformation))
    (apply-transformation (transformation-implementation transformation) tuple))
  (:method ((transformation transformation) (relation simple-relation))
    (reduce #'combine-potential-relations
	    (mapcar (lambda (tuple)
		      (apply-transformation transformation tuple))
		    (tuples relation))))
  (:method ((list list) (tuple tuple))
    (check-type list (cons transformation))
    (reduce (lambda (tuple transformation)
	      (apply-transformation transformation tuple))
	    list
	    :initial-value tuple))
  (:method ((f function) (tuple tuple))
    (funcall f tuple))
  (:method ((s symbol) (tuple tuple))
    (funcall s tuple)))

(defgeneric compose-signatures (a b)
  ;; TODO: Make type of TUPLE ensure signature.
  (:method ((signature signature) (tuple tuple))
    (let ((attributes (attributes tuple)))
      (make-signature (union (signature-input signature) attributes)
		      (union (signature-output signature) attributes))))
  (:method ((a signature) (b signature))
    (make-signature (union (signature-input a) (signature-input b))
		    (union (signature-output a) (signature-output b)))))

(defclass plan-profile () ((transformations-tried :initform 0 :accessor transformations-tried)))

(defmethod print-object ((p plan-profile) (stream t))
  (format stream "<PLAN-PROFILE; transformations-tried: ~d>" (transformations-tried p)))

(defvar *plan-profile*)

(defun permutations (elts)
  (if (cdr elts)
      (mapcan (lambda (elt) (mapcar (lambda (x) (cons elt x))
				    (permutations (remove elt elts))))
	      elts)
      (list elts)))

(defun transformation-provides-p (signature transformation)
  (provides (signature-output signature) (transformation-signature transformation)))

(defun component-provides-p (signature component)
  (some (lambda (transformation) (transformation-provides-p signature transformation))
	(component-transformations component)))

(defgeneric %plan (system element signature plan)
  (:method ((remaining-component-list list) (transformation transformation) (signature signature) (plan list))
    (incf (transformations-tried *plan-profile*))
    (let* ((tran-sig (transformation-signature transformation))
	   ;; Which of the still-needed output, if any, does the this transformation's signature provide?
	   (provided-output (provides (signature-output signature) tran-sig)))
      (unless provided-output
	;; If this transformation doesn't provide any needed output, fail early.
	(return-from %plan nil))
      ;; Otherwise, add the transformation to the plan and update the signature to satisfy.
      (let* ((new-plan (cons transformation plan))
	     ;; Input of the current transformation which aren't trivially provided must now be output of the
	     ;; remaining plan (to be provided before this step's transformation is applied).
	     (additional-output (set-difference (signature-input tran-sig) (signature-input signature)))
	     ;; Output which still need to be provided.
	     (remaining-output-needed  (set-difference (union (signature-output signature) additional-output) provided-output)))
	(if remaining-output-needed
	    ;; If there are still output which need to be satisfied, continue planning the component list.
	    (%plan remaining-component-list :component-list (make-signature (signature-input signature) remaining-output-needed) new-plan)
	    ;; Otherwise, return the new plan.
	    (list new-plan)))))
  (:method ((component-list list) (component component) (signature signature) (plan list))
    (let* ((candidates (remove-if-not (lambda (tr) (transformation-provides-p signature tr)) (component-transformations component)))
	   (all-candidate-orderings (permutations candidates)))
      (loop for ordering in all-candidate-orderings
	 append (remove nil (mapcan (lambda (transformation)
				      (let ((inner-plan (%plan component-list transformation signature plan)))
					inner-plan))
				    (component-transformations component))))))	
  (:method ((component-list list) (selector (eql :component-list)) (signature signature) (plan list))
    (let* ((candidates (remove-if-not (lambda (c) (component-provides-p signature c)) component-list))
	   (all-candidate-orderings (permutations candidates)))
      (remove nil (loop for ordering in all-candidate-orderings
		     append (mapcan
			     ;; If we want to only return one plan, we could shortcut and return on first non-NIL result here.
			     (lambda (component)
			       (%plan (remove component component-list) ;; Each component can only be used once.
				      component signature plan))
			     ordering)))))
  (:method ((system system) (start (eql :system)) (signature signature) (plan list))
    (%plan (system-components system) :component-list signature plan)))

(defgeneric plan (system signature)
  (:method ((system system) (signature signature))
    (let* ((*plan-profile* (make-instance 'plan-profile))
	   (all-plans (%plan system :system (pruned-signature signature) '())))
      ;; For now, ignore all but first plan.
      (values (first all-plans)
	      *plan-profile*))))

(defmethod defaulted-initial-data ((system system) (provided t))
  ;; TODO: allow merging of provided data.
  (or provided
      (and (system-data system)
	   (apply #'join (system-data system)))))

(defgeneric solve (system signature &optional initial-data)
  ;;(:method ((system system) (signature signature) (initial-tuple tuple))
  (:method ((system system) (signature signature)  &optional initial-data) ;; TODO: create and use common supertype for tuple and relation.
    (let ((plan (plan system signature)))
      (and plan
	   (satisfies-input-p initial-data signature)
	   (reduce (lambda (tuple-or-relation transformation)
		     (apply-transformation transformation tuple-or-relation))
		   plan
		   :initial-value (defaulted-initial-data system initial-data))))))

(defun solve-for (system output &optional initial-data)
  (let* ((defaulted (defaulted-initial-data system initial-data))
	 (sig (make-signature (attributes defaulted) output)))
    (solve system sig defaulted)))

(defun ask (system output &optional initial-data)
  "Like solve-for but only returns the requested attributes in response tuple."
  (project output (solve-for system output initial-data)))

(defun plan-for (system output &optional initial-data)
  (let ((sig (make-signature (attributes (defaulted-initial-data system initial-data)) output)))
    (plan system sig)))

(defun build-relation (from-pairs adding-attributes value-rows)
  (let ((tuples (loop for row in value-rows
		      collect (let ((base (make-tuple from-pairs)))
				(loop for attr in adding-attributes
				   for val in row
				   do (setf (tref attr base) val))
				base))))
    (make-relation tuples)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests / Examples

(test orient-tests
  "General tests planning and solving."
  (let* ((d1 (tuple (a 2) (b 3) (c 4)))
	 (d2 (tuple (a 2) (b 3) (c 4) (d 5)))
	 (d3 (tuple (x 5) (y 6) (z 7)))
	 (d4 (tuple (a 1) (b 2) (c 3)))

	 (r1 (make-relation (list d1 d4)))
	 ;; (r2 (make-relation (list d2)))
	 ;; (r3 (make-relation (list d3)))

	 (sig1 (sig (a b c) -> (d)))
	 (sig2 (sig (b c d) -> (e f)))
	 (sig3 (sig (a b c) -> (e f)))

	 (t1 (transformation ((a b c) -> (d)) == (values (* a b c))))
	 (t2 (transformation ((x y z) -> (q)) == (values (+ x y z))))
	 (t3 (transformation ((b c d) -> (e f)) == (let ((x (+ b c d)))
						     (values (* x b) (* x c)))))

	 (c1 (component (t1)))
	 (c2a (component (t1)))
	 (c2b (component (t2)))
	 (c2c (component (t3)))
	 
	 (s1 (sys (c1)))
	 (s2 (sys (c2a c2b c2c))))	
    (is (same (apply-transformation t2 d3) (tuple (x 5)(y 6)(z 7)(q 18))) "(apply-transformation t2 d3)")

    (is (same (plan s1 sig1) (list t1)) "(plan s1 sig1)")
    (is (same (plan s1 sig2) nil) "(plan s1 sig2)") 
    (is (same (plan s1 sig3) nil) "(plan s1 sig3)")

    (is (same (plan s2 sig1) (list t1)) "(plan s2 sig1)")
    (is (same (plan s2 sig2) (list t3)) "(plan s2 sig2)")

    (is (same (plan s2 sig3) (list t1 t3)) "(plan s2 sig3)")

    (is (same (solve s1 sig1 d1) (tuple (a 2)(b 3)(c 4)(d 24))) "(solve s1 sig1 d1)")
    (is (same (solve s1 sig2 d1) nil) "(solve s1 sig2 d1)")
    (is (same (solve s1 sig3 d1) nil) "(solve s1 sig3 d1)")

    (is (same (solve s2 sig1 d1) (tuple (a 2)(b 3)(c 4)(d 24))) "(solve s2 sig1 d1)")
    (is (same (solve s2 sig1 r1) (make-relation (list (tuple (a 1)(b 2)(c 3)(d 6))
						      (tuple (a 2)(b 3)(c 4)(d 24)))))
	"(solve s2 sig1 r1)")
    (is (same (solve s2 sig2 d1) nil) "(solve s2 sig2 d1)")
    (is (same (solve s2 sig2 d2) (tuple (a 2)(b 3)(c 4)(d 5)(e 36)(f 48))) " (solve s2 sig2 d2)")
    (is (same (solve s2 sig3 d1) (tuple (a 2)(b 3)(c 4)(d 24)(e 93)(f 124))) "(solve s2 sig3 d1)")))

(test join
  "Test join."
  (is (same (join (tuple (a 1) (b 2) (c 3)) (tuple (b 2) (c 3) (d 4)))
	    (tuple (a 1) (b 2) (c 3) (d 4))) "tuple-tuple join")

  (is (same (join (tuple (a 1) (b 9) (c 3)) (tuple (b 2) (c 3) (d 4)))
	    nil) "tuple-tuple join with no match")

  (is (same (join (tuple (a 1) (b 2) (c 3)) (relation (b c d)
						      (2 3 4)
						      (22 33 44)))
		  (relation (a b c d)
			    (1 2 3 4))) "tuple-relation join")

  (is (same (join (relation (a b c)
			    (1 2 3)
			    (1 22 33)
			    (3 2 3))
		  (relation (b c d)
			    (2 3 4)
			    (22 33 44)))
	    (relation (a b c d)
		      (1 2 3 4)
		      (1 22 33 44)
		      (3 2 3 4))) "tuple-relation join"))

(test rename-tuple
  "Test tuple renaming."
  (is (same (rename ((a f)(b g))
		    (tuple (a 1) (b 2) (c 3)))
	    (tuple (f 1) (g 2) (c 3)))))

(test rename-relation
  "Test relation renaming."
  (is (same (rename ((a f)(b g))
		    (relation (a b c) (1 2 3)))
	    (relation (f g c) (1 2 3)))))

(test simple-bidirectional
  "Simple test of a bidirectional constraint."
  (let* ((d1 (tuple (a 1)))
	 (d2 (tuple (b 10)))
	 (d3 (tuple (a 1) (b 5)))
	 (d4 (tuple (a 2) (b 10)))

	 (t1 (transformation ((a) -> (b)) == (* a 5)))
	 (t2 (transformation ((b) -> (a)) == (/ b 5)))

	 ;; TODO: Simplify defining components like this 'constraint'.
	 ;; TODO2: Represent it as a relation.
	 ;; TODO3: Allow for planning through relations (consider signatures).
	 (c1 (component (t1 t2)))

	 (s1 (sys (c1))))
    (is (same d3 (solve-for s1 '(b) d1)))
    (is (same d4 (solve-for s1 '(a) d2)))))

#+(or) ;; TODO: Make this work.
(test expressive-bidirectional
  "Simple test of a bidirectional constraint."
  
  (let* ((d1 (tuple (a 1)))
	 (d2 (tuple (b 10)))
	 (d3 (tuple (a 1) (b 5)))
	 (d4 (tuple (a 2) (b 10)))

	 (t1 (somesyntax (a b c) ==
			 (c (* a b))
			 (a (/ c b))
			 (b (/ c a))))
	 
	 (t1 (transformation ((a) <-> (b)) == (times a b 5)))

	 ;; TODO: Simplify defining components like this 'constraint'.
	 ;; TODO2: Represent it as a relation.
	 ;; TODO3: Allow for planning through relations (consider signatures).
	 (c1 (component (t1 t2)))

	 (s1 (sys (c1))))
    (is (same (solve-for s1 '(b) d1) d3))
    (is (same (solve-for s1 '(a) d2) d4))))
    

(test planning-terminates
  "Regression test for infinite stack bug."
  (let* ((t1 (transformation ((b c d) -> (e f)) == (let ((x (+ b c d)))
						     (values (* x b) (* x c)))))
	 (s1 (sys ((component (t1))))))

    (finishes (plan s1 (make-signature '(b c d) '(e))))))

#|
(plan s1 sig1) => (((SIG (A B C) -> (D)) . (TRANSFORMATION (SIG (A B C) -> (D)) === ASDF)))
(plan s1 sig2) => nil
(plan s1 sig3) => nil

(plan s2 sig1) => (((TRANSFORMATION (SIG (A B C) -> (D)) === ASDF)))                          ; *transformations-tried* 3

(plan s2 sig2) => (((TRANSFORMATION (SIG (B C D) -> (E F)) === FDSA))                         ; *transformations-tried* 3

(plan s2 sig3) => ((TRANSFORMATION (SIG (A B C) -> (D)) === ASDF)
		   (TRANSFORMATION (SIG (B C D) -> (E F)) === FDSA))                          ; *transformations-tried* 6
|#
