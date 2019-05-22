(defpackage :orient
  (:use :common-lisp :it.bese.FiveAm)
  (:export :apply-transformation :ask :attributes :component :constraint-system  :defconstraint-system :tuple :tuples :tuple-pairs
	   :defschema
	   :deftransformation :deftransformation= :forget
	   :tref :join :make-relation
	   :make-signature
	   :orient-tests :plan :plan-for :rel :relation :remove-attributes :remv :rename :report-data :report-solution-for :same
	   :schema-parameters :schema-description :sig :signature :signature-input :signature-output :solve :solve-for :sys :system :system-data
	   :tpl :transformation :try-with :use-construction :use-attribute
	   :where :with-construction
	   :*current-construction* :*trace-plan* :-> :=== :== &all :!>))

(in-package "ORIENT")
(def-suite orient-suite :description "Test the orient package.")
(in-suite orient-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros

;;;; Interactive Interface
(defmacro with-construction ((system-form) &rest body)
  `(let ((*current-construction* ,system-form))
     ,@body))

(defmacro forget (&rest attributes)
  `(setf (system-data *current-construction*)
	 (mapcar (lambda (d)
		   (remove-attributes ',attributes d))
		 (system-data *current-construction*))))

(defmacro try-with (attribute value-form)
  `(set-construction-parameter ',attribute ,value-form))

;;;

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

(defun format-as-infix (prefix)
  ;; TODO: actually do this
  (format nil "~A" prefix)
  )

(defmacro transformation (((&rest input-lambda-list) arrow (&rest output)) eqmark implementation)
  (assert (eql arrow '->))
  (let* ((input-lambda-list (remove-if-not #'symbolp input-lambda-list))
	 (output (remove-if-not #'symbolp output))
	 (input (process-input-list input-lambda-list)))
    (ecase eqmark
      (= `(let ((sig (make-signature ',input ',output)))
	    (make-instance 'transformation :signature sig :implementation (rlambda ,input-lambda-list ,output ,implementation))))
      (== `(let ((sig (make-signature ',input ',output)))
	     (make-instance 'transformation
			    :signature sig
			    :implementation (tlambda ,input-lambda-list ,output ,implementation)
			    :description ,(format-as-infix implementation)
			    )))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Constraints

(defmacro defconstraint-system (name constraint-definitions &key schema)  
  `(eval-when (:load-toplevel :execute)
     (let ((system (constraint-system ,constraint-definitions)))
       (when ,schema
	 (setf (system-schema system) ,schema))
       (defparameter ,name system))))

(defmacro constraint-system (constraint-definitions)
  `(make-instance 'system :components ,(expand-constraint-definitions constraint-definitions)))

(defun expand-constraint-definitions (constraint-definitions)
  `(list ,@(mapcar (lambda (constraint-form) (apply #'expand-constraint-definition constraint-form)) constraint-definitions)))

(deftype multiplication-constraint-form () `(cons (eql *)))
(deftype division-constraint-form () `(cons (eql /)))
(deftype addition-constraint-form () `(cons (eql +)))
(deftype subtraction-constraint-form () `(cons (eql -)))

;; Only handles binary constraints, for now.
(defun expand-constraint-definition (name constraint-form)
  (etypecase constraint-form
    (multiplication-constraint-form
     (expand-multiplication-constraint name (first (cdr constraint-form)) (second (cdr constraint-form))))
    (division-constraint-form
     (expand-division-constraint name (first (cdr constraint-form)) (second (cdr constraint-form))))
    (addition-constraint-form
     (expand-addition-constraint name (first (cdr constraint-form)) (second (cdr constraint-form))))
    (subtraction-constraint-form
     (expand-subtraction-constraint name (first (cdr constraint-form)) (second (cdr constraint-form))))))

(defun expand-multiplication-constraint (product a b)
  "PRODUCT = A * B"
  `(component ((transformation ((,a ,b) -> (,product)) == (* ,a ,b))
	       (transformation ((,a ,product) -> (,b)) == (/ ,product ,a))
	       (transformation ((,b ,product) -> (,a)) == (/ ,product ,b)))))

(defun expand-division-constraint (quotient dividend divisor)
  "DIVIDEND / DIVISOR = QUOTIENT => DIVIDEND = QUOTIENT * DIVISOR"
  (expand-multiplication-constraint dividend quotient divisor))

(defun expand-addition-constraint (sum  a b)
  "SUM = A + B"
  `(component ((transformation ((,a ,b) -> (,sum)) == (+ ,a ,b))
	       (transformation ((,a ,sum) -> (,b)) == (- ,sum ,a))
	       (transformation ((,b ,sum) -> (,a)) == (- ,sum ,b)))))

(defun expand-subtraction-constraint (difference minuend subtrahend)
  "MINUEND - SUBTRAHEND = DIFFERENCE => MINUEND = DIFFERENCE + SUBTRAHEND"
  (expand-addition-constraint MINUEND DIFFERENCE SUBTRAHEND))

(test two-multiplication-constraints
  "Test CONSTRAINT-SYSTEM with two multiplication contraints."
  (let* ((system (constraint-system
		  ((c (* a b))
		   (d (* a c)))))
	 (satsifying-assignment (tuple (a 3) (b 2) (c 6) (d 18))))

    (is (same satsifying-assignment
	      (solve-for system '(a b) (tuple (c 6) (d 18)))))    
    (is (same satsifying-assignment
	      (solve-for system '(a d) (tuple (b 2) (c 6)))))

    (is (same satsifying-assignment
	      (solve-for system '(b c) (tuple (a 3) (d 18)))))
    (is (same satsifying-assignment
	      (solve-for system '(b d) (tuple (a 3) (c 6)))))
    
    (is (same satsifying-assignment
	      (solve-for system '(c d) (tuple (a 3) (b 2)))))

    ;; This one can't (currently) be solved.
    (is (same nil 
	      (solve-for system '(a c) (tuple (b 2) (d 18)))))))

(test division-constraint
  "Test CONSTRAINT-SYSTEM with a division constraint."
  (let* ((system (constraint-system ((z (/ x y)))))
	 (satisfying-assignment (tuple (z 3) (x 12) (y 4))))
    (is (same satisfying-assignment
	      (solve-for system '(z) (tuple (x 12) (y 4)))))
    (is (same satisfying-assignment
	      (solve-for system '(x) (tuple (y 4) (z 3)))))
    (is (same satisfying-assignment
	      (solve-for system '(y) (tuple (x 12) (z 3)))))))

(test two-addition-constraints
  "Test CONSTRAINT-SYSTEM with two addition contraints."
  (let* ((system (constraint-system
		  ((c (+ a b))
		   (d (+ a c)))))
	 (satsifying-assignment (tuple (a 3) (b 2) (c 5) (d 8))))

    (is (same satsifying-assignment
	      (solve-for system '(a b) (tuple (c 5) (d 8)))))    
    (is (same satsifying-assignment
	      (solve-for system '(a d) (tuple (b 2) (c 5)))))

    (is (same satsifying-assignment
	      (solve-for system '(b c) (tuple (a 3) (d 8)))))
    (is (same satsifying-assignment
	      (solve-for system '(b d) (tuple (a 3) (c 5)))))
    
    (is (same satsifying-assignment
	      (solve-for system '(c d) (tuple (a 3) (b 2)))))

    ;; This one can't (currently) be solved.
    (is (same nil 
	      (solve-for system '(a c) (tuple (b 2) (d 8)))))))

(test subtraction-constraint
  "Test CONSTRAINT-SYSTEM with a division constraint."
  (let* ((system (constraint-system ((z (- x y)))))
	 (satisfying-assignment (tuple (z 8) (x 12) (y 4))))
    (is (same satisfying-assignment
	      (solve-for system '(z) (tuple (x 12) (y 4)))))
    (is (same satisfying-assignment
	      (solve-for system '(x) (tuple (y 4) (z 8)))))
    (is (same satisfying-assignment
	      (solve-for system '(y) (tuple (x 12) (z 8)))))))

(test constraint-constants
  "Test CONSTRAINT-SYSTEM with some constants."
  (let* ((system (constraint-system ((c (+ a 2)))))
	 (satisfying-assignment (tuple (a 3) (c 5))))

    (is (same satisfying-assignment
	      (solve-for system '(a) (tuple (c 5)))))
    (is (same satisfying-assignment
	      (solve-for system '(c) (tuple (a 3)))))))

#|
(defconstraint-system performance-b
    ((aws-price-TiB-year (* aws-storage-price 12))
     (annual-TiB (/ comparable-monthly-income aws-price-TiB-year))
     (monthly-TiB (/ annual-TiB miner-months-to-capacity)) ;; Rate at which we must seal.
     (daily-TiB (/ monthly-TiB (/ 365 12)))
     (hourly-TiB (/ daily-Tib 24))
     (hourly-GiB (* hourly-TiB 1024))
     (up-front-drive-cost (* TiB-drive-cost annual-TiB))
     (cycles-per-hour (* hourly-GiB GiB-replication-cycles))
     (cycles-per-minute (* cycles-per-hour 60))
     (cycles-per-second (* cycles-per-minute 60))
     (needed-ghz (/ cycles-per-second 1e9))
     (up-front-compute-cost (/ needed-ghz cpu-ghz-cost))
     (total-up-front-cost (+ up-front-compute-cost up-front-drive-cost))
     (seal-cost (/ total-up-front-cost hourly-GiB))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (implementation :initarg :implementation :initform nil :accessor transformation-implementation)
   (description :initarg :description :initform nil :accessor transformation-description)))

(defmethod print-object ((trans transformation) (stream t))
  (let ((implementation (transformation-implementation trans)))
    (format stream "(TRANSFORMATION ~S === ~S)" (transformation-signature trans) (if (functionp implementation) "FN()" implementation))))

(defun identity-transformation () (make-instance 'transformation :implementation (lambda (attributed) attributed)))

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

5(defgeneric lookup (attribute schemable)
   (:method ((attribute symbol) (schema schema))
     (find attribute (schema-parameters schema) :key #'parameter-name))
   (:method ((attribute symbol) (system system))
     (when (system-schema system)
       (lookup attribute (system-schema system)))))

(defun lookup-description (attribute schemable)
  (let ((parameter (lookup attribute schemable)))
    (and parameter (parameter-description parameter))))

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

(defgeneric satisfies-input-p (attributed b)
  (:documentation "True if all inputs to B are attributes of ATTRIBUTED.")
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

(defun pipeline-signature (pipeline)
  ;; A plan is a pipeline -- a list of transformations.
  (and pipeline
       (let ((signatures  (mapcar #'transformation-signature pipeline)))
	 (reduce #'compose-signatures signatures :initial-value (first signatures)))))


(defclass plan-profile () ((transformations-tried :initform 0 :accessor transformations-tried)))

(defmethod print-object ((p plan-profile) (stream t))
  (format stream "<PLAN-PROFILE; transformations-tried: ~d>" (transformations-tried p)))

(defvar *plan-profile*)

(defun permutations (elts)
  ;; FIXME: Convert this to a lazy stream. Otherwise we will blow up trying to manifest permutations of many elements.
  (assert (< (length elts) 11))
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

(defgeneric signature-satisfies-p (signature thing)
  (:documentation "True if signatures inputs are a subset of thing's.")
  (:method ((signature signature) (transformation transformation))
    (subsetp (signature-input (transformation-signature transformation)) (signature-input signature)))
  (:method ((signature signature) (component component))
    (some (lambda (transformation) (signature-satisfies-p signature transformation))
	  (component-transformations component))))

(defvar *trace-plan* nil)

(defun debug-plan (&rest args)
  (when *trace-plan*
    (print args)))

(defgeneric %plan-backward (system element signature plan)
  (:method ((remaining-component-list list) (transformation transformation) (signature signature) (plan list))
    (when *trace-plan* (print (list :signature signature :plan plan)))
    
    (incf (transformations-tried *plan-profile*))
    (let* ((tran-sig (transformation-signature transformation))
	   ;; Which of the still-needed output, if any, does the this transformation's signature provide?
	   (provided-output (provides (signature-output signature) tran-sig)))
      (unless provided-output
	;; If this transformation doesn't provide any needed output, continue to the next com
	(return-from %plan-backward nil))
      ;; Otherwise, add the transformation to the plan and update the signature to satisfy.
      (let* ((new-plan (cons transformation plan))
	     ;; Input of the current transformation which aren't trivially provided must now be output of the
	     ;; remaining plan (to be provided before this step's transformation is applied).
	     (additional-output (set-difference (signature-input tran-sig) (signature-input signature)))
	     ;; Output which still need to be provided.
	     (remaining-output-needed  (set-difference (union (signature-output signature) additional-output) provided-output)))
	(if remaining-output-needed
	    ;; If there are still output which need to be satisfied, continue planning the component list.
	    (%plan-backward remaining-component-list :component-list (make-signature (signature-input signature) remaining-output-needed) new-plan)
	    ;; Otherwise, return the new plan.
	    new-plan))))
  (:method ((remaining-component-list list) (component component) (signature signature) (plan list))
    (let* ((candidates (remove-if-not (lambda (tr) (transformation-provides-p signature tr)) (component-transformations component)))
	   (all-candidate-orderings (permutations candidates)))
      (loop for ordering in all-candidate-orderings
	 do (loop for transformation in ordering
	       for maybe-plan = (%plan-backward remaining-component-list transformation signature plan)
	       when maybe-plan do (return-from %plan-backward maybe-plan)))))
  #+(or) ;; Trying to return all plans eventually becomes too expensive. Leave here for now in case we want to adapt to stream plans incrementally.
  (:method ((component-list list) (selector (eql :component-list)) (signature signature) (plan list))
    (let* ((candidates (remove-if-not (lambda (c) (component-provides-p signature c)) component-list))
	   (all-candidate-orderings (permutations candidates)))
      (remove nil (loop for ordering in all-candidate-orderings
		     append (mapcan
			     ;; If we want to only return one plan, we could shortcut and return on first non-NIL result here.
			     (lambda (component)
			       (%plan-backward (remove component component-list) ;; Each component can only be used once.
					       component signature plan))
			     ordering)))))
  (:method ((component-list list) (selector (eql :component-list)) (signature signature) (plan list))
    (let* ((candidates (remove-if-not (lambda (c) (component-provides-p signature c)) component-list))
	   (all-candidate-orderings (permutations candidates)))
      (when candidates
	(loop for ordering in all-candidate-orderings
	   do (loop for component in ordering
		 for maybe-plan = (%plan-backward (remove component component-list) ;; Each component can only be used once.
						  component signature plan)
		 ;; short-circuit after first complete plan is returned.
		 when maybe-plan
		 do (return-from %plan-backward maybe-plan))))))
  
  (:method ((system system) (start (eql :system)) (signature signature) (plan list))
    (%plan-backward (system-components system) :component-list signature plan)))

(defgeneric plan-backward (system signature)
  (:method ((system system) (signature signature))
    (let* ((*plan-profile* (make-instance 'plan-profile)))
      ;; For now, ignore all but first plan.
      (values  (%plan-backward system :system (pruned-signature signature) '())
	       *plan-profile*))))


(defgeneric plan (system signature)
  (:method ((system system) (signature signature))
    (let* ((*plan-profile* (make-instance 'plan-profile))
	   (plan (%plan system :system (pruned-signature signature) '())))
      (debug-plan :found-plan plan)
      ;; For now, ignore all but first plan.
      (values  plan
	       *plan-profile*))))

(defgeneric %plan (system element signature plan)
  (:documentation "Accumulates and returns a plan in reverse (result needs to be reversed).")
  (:method ((remaining-component-list list) (transformation transformation) (signature signature) (plan list))
    (debug-plan :transformation transformation :signature signature :plan plan)
    
    (incf (transformations-tried *plan-profile*))
    (let* ((tran-sig (transformation-signature transformation)))
      ;; TODO: Under what circumstances, if any, can we skip following a branch forward?
      
      ;; Add the transformation to the plan and update the signature to satisfy.
      (let* ((new-plan (cons transformation plan))
	     (new-signature (make-signature (union (signature-input signature) (signature-output tran-sig))
					    (signature-output signature))))
	(debug-plan :new-plan new-plan :new-signature new-signature)
	(%plan remaining-component-list :component-list new-signature new-plan))))
  (:method ((remaining-component-list list) (component component) (signature signature) (plan list))
    (debug-plan :planning :component component)
    (let* ((candidates (remove-if-not (lambda (tr) (signature-satisfies-p signature tr)) (component-transformations component)))
	   (all-candidate-orderings (permutations candidates)))
      (loop for ordering in all-candidate-orderings
	 do (loop for transformation in ordering		 
	       for maybe-plan = (%plan remaining-component-list transformation signature plan)
	       when maybe-plan do (return-from %plan maybe-plan)))))
  (:method ((component-list list) (selector (eql :component-list)) (signature signature) (plan list))
    (let* ((candidates (remove-if-not (lambda (c) (signature-satisfies-p signature c)) component-list))
	   (all-candidate-orderings (permutations candidates)))
      (debug-plan :signature signature :component-list component-list :candidates candidates)
      (if candidates
	  (loop for ordering in all-candidate-orderings
	     do (loop for component in ordering
		   for maybe-plan = (%plan (remove component component-list) ;; Each component can only be used once.
					   component signature plan)
		   ;; short-circuit after first complete plan is returned.
		   when maybe-plan
		   do (return-from %plan maybe-plan)))
	  plan)))  
  (:method ((system system) (start (eql :system)) (signature signature) (plan list))
    ;; If the final pipeline doesn't satisfy SIGNATURE's output, PLAN is no good.
    (let* ((plan (reverse (%plan (system-components system) :component-list signature plan)))
	   (plan-signature (pipeline-signature plan)))

      (when (subsetp (signature-output signature) (signature-input signature))
	(return-from %plan (list (identity-transformation))))
      ;; FIXME: this will fail if there is no plan but none was needed. Fix that.
      
      ;; TODO: prune extraneous transformations from the plan.
      (and plan-signature (subsetp (signature-output signature) (signature-output plan-signature))
	   plan))))

(defvar *tsort-result*)
(defvar *tsort-remaining*)
(defvar *tsort-marked*)
(defun tsort-transformations (available attributes)
  (let ((*tsort-result* '())
	(*tsort-marked* '())
	(*tsort-remaining* available))
    (%tsort-transformations attributes)
    *tsort-result*))


(defun %tsort-transformations (attributes)
  (when *tsort-remaining*
    (loop for out in attributes
       for next-transformations = (remove-if-not (lambda (transformation)
						   (member out (signature-input (transformation-signature transformation))))
						 *tsort-remaining*)
       do (when *trace-plan* (print (list :out out :next-transformations next-transformations)))
       do (loop for next in next-transformations
	     do (progn ;(setq *tsort-remaining* (remove next *tsort-remaining*))
		  (assert (not (member next *tsort-marked*))) ;; Not a DAG
		  (push next *tsort-marked*)
		  (%tsort-transformations  (signature-output (transformation-signature next)))
		  (push next *tsort-result*)
		  (when *trace-plan* (print (list :next next)))
		  (setq *tsort-remaining* (remove next *tsort-remaining*))
		  (when *trace-plan* (print (list :result *tsort-result* :remaining *tsort-remaining*)))
		  ))
	 )))

(defun tsort-components (available attributes)
  (let ((*tsort-result* '())
	(*tsort-marked* '())
	(*tsort-remaining* available))
    (%tsort-components attributes)
    *tsort-result*))

(defun %tsort-components (attributes)
  (when *tsort-remaining*
    (loop for out in attributes
       for next-components = (remove-if-not (lambda (component)
					      (some (lambda (transformation)
						      (member out (signature-input (transformation-signature transformation))))
						    (component-transformations component)))
					    *tsort-remaining*)
       do (when *trace-plan* (print (list :out out :next-components next-components)))
       do (loop for next in next-components
	     do (progn ;(setq *tsort-remaining* (remove next *tsort-remaining*))
		  (assert (not (member next *tsort-marked*))) ;; Not a DAG
		  (let ((*tsort-marked* (cons next *tsort-marked*)))
		    (when *trace-plan* (print (list :marking next :marked *tsort-marked*)))
		    (%tsort-components (signature-output (loop for transformation in (component-transformations next)
							    for sig = (transformation-signature transformation)
							    when (member out (signature-input sig))
							    return sig)
							 )
				       ))
		  (push next *tsort-result*)
		  (when *trace-plan* (print (list :next next)))
		  (setq *tsort-remaining* (remove next *tsort-remaining*))
		  (when *trace-plan* (print (list :result *tsort-result* :remaining *tsort-remaining*))))))))

(defgeneric plan-tsort-transformations (system signature)
  (:method ((system system) (signature signature))
    (let* ((all-transformations (loop for component in (system-components system) append (component-transformations component)))
	   (tsorted-transformations (tsort-transformations all-transformations (signature-input signature))))
      tsorted-transformations)))

(defgeneric plan-tsort-components (system signature)
  (:method ((system system) (signature signature))
    (let* ((all-components (system-components system))
	   (tsorted-components (tsort-components all-components (signature-input signature))))
      tsorted-components)))

;; (test diamond-plan
;;   ;; NOTE: this never actually triggered the problem yet.
;;   "Test that a potentially pathological 'diamond' plan terminates when computing."
;;   (let ((system (constraint-system ((a (+ initial 1))
;; 				    (b (+ a 2))
;; 				    (c (+ a 3))
;; 				    (d (* b c))))))
;;     (plan system '(d))))

(defmethod defaulted-initial-data ((system system) (provided t))
  ;; TODO: allow merging of provided data.
  (or provided
      (and (system-data system)
	   (apply #'join (system-data system)))))

(defgeneric describe-transformation-calculation (transformation)
  (:method ((transformation transformation))
    (or (transformation-description transformation)
	(signature-input (transformation-signature transformation)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface

(defvar *current-construction*)

(defun use-construction (system &key data)
  (progn
    (when data
      (setf (system-data system) (if (listp data) data (list data))))
    (setq *current-construction* system)))  

(defun set-construction-parameter (attribute value)
  ;; Quick and dirty for now, just set data in first naked tuple we find.
  (let ((tuple (some (lambda (x) (and (typep x 'tuple) x)) (system-data *current-construction*))))

    (cond (tuple (setf (tref attribute tuple) value))
	  (t  ;(push (tuple (attribute value)) (system-data *current-construction*))
	   ))))

(defgeneric solve (system signature &optional initial-data &key)
  ;;(:method ((system system) (signature signature) (initial-tuple tuple))
  (:method ((system system) (signature signature)  &optional initial-data &key report) ;; TODO: create and use common supertype for tuple and relation.
    (let ((plan (plan system signature))
	  (schema (system-schema system))
	  (report-results '()))
      (and plan
	   (satisfies-input-p initial-data signature)
	   (let ((result (reduce (lambda (tuple-or-relation transformation)
				   (let ((new (apply-transformation transformation tuple-or-relation)))
				     (when report
				       (typecase new
					 (tuple 
					  (let ((rep (loop for (key value) in (tuple-pairs new)
							when (member key (signature-output (transformation-signature transformation)))
							collect (format nil "~&~A: ~A = ~A~% ~A~%"
									key
									(describe-transformation-calculation transformation)
									value
									(let ((desc (and schema (lookup-description key schema))))
									  (if desc (format nil "   ~A~%" desc) ""))
									))))
					    (setf report-results (append report-results rep))))
					 (relation (assert nil) ;; unsupported for now
						   )))
				     new
				     ))
				 plan
				 :initial-value (defaulted-initial-data system initial-data))))
	     (values result plan report-results))))))

(defgeneric report (thing context)
  (:method ((null null) (context t))
    "Nothing to report.")
  (:method ((tuple tuple) (schema schema))
    (with-output-to-string (out)
      (loop for (key value) in (tuple-pairs tuple)
	 do (format out "~&~A: ~A~% ~A~%"
		    key
		    value
		    (let ((desc (and schema (lookup-description key schema))))
		      (if desc (format nil "   ~A~%" desc) ""))))))
  (:method ((tuple tuple) (system system))
    (report tuple (system-schema system))))

(defun report-data (&optional (system *current-construction*))
  (mapcar (lambda (data) (report data system)) (system-data system)))

(defun solve-for (system output &optional initial-data &key report)
  (let* ((defaulted (defaulted-initial-data system initial-data))
	 (sig (make-signature (attributes defaulted) output)))
    (solve system sig defaulted :report report)))

(defun report-solution-for (output &key (system *current-construction*) initial-data)
  (multiple-value-bind (solution plan report) (solve-for system output initial-data :report t)
    (cond (solution
	   (assert (typep solution 'tuple))
	   (values (apply #'concatenate 'string report)
		   solution
		   ))
	  (t (values "NO SOLUTION" nil)))))

(defun ask (system output &optional initial-data)
  "Like solve-for but only returns the requested attributes in response tuple."
  (let ((solution  (solve-for system output initial-data)))
    (when solution
      (project output solution))))

(defun plan-for (system output &optional initial-data)
  (let* ((defaulted (defaulted-initial-data system initial-data))
	 (sig (make-signature (attributes defaulted) output)))
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

;; TODO: Split these up.
(test orient-tests
  "General tests planning and solving."
  (let* ((d1 (tuple (a 2) (b 3) (c 4)))
	 (d2 (tuple (a 2) (b 3) (c 4) (d 5)))
	 (d3 (tuple (x 5) (y 6) (z 7)))
	 (d4 (tuple (a 1) (b 2) (c 3)))

	 ;; (r1 (make-relation (list d1 d4)))
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

    ;; FIXME: This test fails because PLAN needs to prune superfluous transformations.
    ;;(is (same (plan s2 sig1) (list t1)) "(plan s2 sig1)")
    
    (is (same (plan s2 sig2) (list t3)) "(plan s2 sig2)")

    (is (same (plan s2 sig3) (list t1 t3)) "(plan s2 sig3)")

    (is (same (solve s1 sig1 d1) (tuple (a 2)(b 3)(c 4)(d 24))) "(solve s1 sig1 d1)")
    (is (same (solve s1 sig2 d1) nil) "(solve s1 sig2 d1)")
    (is (same (solve s1 sig3 d1) nil) "(solve s1 sig3 d1)")

    ;; FIXME: This test fails because PLAN needs to prune superfluous transformations.
    ;; Specifically, the result tuple has too many attributes because of this.
    ;; (is (same (solve s2 sig1 d1) (tuple (a 2)(b 3)(c 4)(d 24))) "(solve s2 sig1 d1)")
    ;; (is (same (solve s2 sig1 r1) (make-relation (list (tuple (a 1)(b 2)(c 3)(d 6))
    ;; 						      (tuple (a 2)(b 3)(c 4)(d 24)))))
    ;; 	"(solve s2 sig1 r1)")
    ;;
    
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
