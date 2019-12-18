(in-package orient)

(def-suite orient-suite)
(in-suite orient-suite)

(defclass parameter ()
  ((name :initarg :name :initform (error "name missing") :accessor parameter-name)
   (description :initarg :description :accessor parameter-description)
   (type :initarg :type :initform nil :accessor parameter-type)))

(defclass signature ()
  ((input :initarg :input :initform (empty-set) :accessor signature-input :type set)
   (output :initarg :output :initform (empty-set) :accessor signature-output :type set)
   ;; GROUP groups the specified attributes.
   ;; GROUP-BY groups the unspecified attributes, 'grouping by' those specified.
   (group :initarg :group :initform (empty-set) :accessor signature-group :type set)
   (group-by :initarg :group-by :initform (empty-set) :accessor signature-group-by :type set)
   (group-into :initarg :group-into :initform nil :accessor signature-group-into :type symbol)
   (reducer? :initarg :reducer :initform nil :accessor signature-reducer? :type boolean)))

(defgeneric reducer? (transformation)
  (:method ((transformation t))
    nil)
  (:method ((transformation transformation))
    (signature-reducer? (transformation-signature transformation))))

(defgeneric grouper? (thing)
  (:method ((thing t))
    nil)
  (:method ((signature signature))
    (or (not (empty? (signature-group signature)))
	(not (empty? (signature-group-by signature)))))
  (:method ((transformation transformation))
    (grouper? (transformation-signature transformation))))

(deftype reducer () '(satisfies reducer?))
(deftype grouper () '(satisfies grouper?))
(deftype aggregator () '(satisfies aggregator?))

(defun make-signature (input output &key reducer group group-by group-into)
  ;; GROUP and GROUP-BY are mutually exclusive (and complementary) ways of specifying the shape of grouping.
  (assert (not (and group group-by)))

  ;; Grouping without reducing requires specification of an attribute into which to group.
  (when (and (or group group-by) (not reducer)) (assert group-into))

  (when reducer
    ;; No new attribute will be added to group into when reducing.
    (assert (not group-into))

    ;; But current implementation does require a temporary attribute in which to store the grouped relation before reducing.
    (setq group-into (intern "into")))

  (make-instance 'signature
		 :input (convert 'set input)
		 :output (convert 'set output)
		 :reducer reducer
		 :group group
		 :group-by group-by
		 :group-into group-into))

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
  (and (subset? (signature-input s) (signature-input other))
       (subset? (signature-output s) (signature-output other))))

(defun sig-equal (a b) (and (sig-subset-p a b) (sig-subset-p b a)))

(defmethod print-object ((trans transformation) (stream t))
  (let ((implementation (transformation-implementation trans)))
    (format stream "(TRANSFORMATION ~S === ~S)" (transformation-signature trans) (if (functionp implementation) "FN()" implementation))))

(defmethod print-object ((impl implementation) (stream t))
  (format stream "<IMPLEMENTATION ~A / ~A>" (implementation-module impl) (implementation-name impl)))

(defun identity-transformation () (make-instance 'transformation :implementation (lambda (attributed) attributed)))

(defmethod print-object ((comp component) (stream t))
  (format stream "(COMPONENT ~S)" (component-transformations comp)))

(defun prune-system-for-flags (system flags)
  ;; Include system if:
  (when (subsetp (system-flags system) flags :test #'string=) ;; All of SYSTEM's flags (which may be none) are included in FLAGS.
    (let ((pruned (make-instance 'system
                                 :name (system-name system)
                                 :schema (system-schema system)
                                 :components (system-components system)
                                 :subsystems (remove nil (mapcar (lambda (x) (prune-system-for-flags x flags))
                                                                 (system-subsystems system)))
                                 :data (system-data system))))
      pruned)))

(defgeneric all-system-components (system)
  (:method ((system system))
    (reduce #'append (cons (mapcan (lambda (maybe-component)
				     (typecase maybe-component
				       (component (list maybe-component))
				       (system (all-system-components maybe-component))))
				   (system-components system))
			   (mapcar #'all-system-components (system-subsystems system))))))

(defgeneric all-system-schemas (system)
  (:method ((system system))
    (remove nil (cons (system-schema system)
		      (mapcan #'all-system-schemas (system-subsystems system))))))

(defgeneric all-system-data (system)
  (:method ((system system))
    (reduce #'append (cons (system-data system) (mapcar #'all-system-data (system-subsystems system))))))

(defmethod system-schema :around ((system system))
  (find-schema (call-next-method)))

(defgeneric expand-references (thing)
  (:method ((system system))
    (make-instance 'system
		   :name (system-name system)
		   :components (system-components system)
		   :schema (find-schema (system-schema system))
		   :subsystems (mapcar #'expand-references (system-subsystems system))
		   :data (system-data system))))

(defmethod print-object ((sys system) (stream t))
  (format stream "#<SYSTEM ~S>" `(,@(awhen (system-name sys) (list :name it))
                                    :components ,(system-components sys)
                                    :subsystems ,(system-subsystems sys)
                                    :schema ,(system-schema sys)
                                    :data ,(system-data sys))))

(defgeneric lookup- (attribute schemable)
  (:method ((attribute symbol) (schema schema))
    (find attribute (schema-parameters schema) :key #'parameter-name))
  (:method ((attribute symbol) (system system))
    (or (awhen (find-schema (system-schema system))
	  (lookup- attribute it))
	(some (lambda (system) (lookup- attribute system))
	      (system-subsystems system)))))

(defun lookup-description (attribute schemable)
  (let ((parameter (lookup-parameter attribute schemable)))
    (and parameter (parameter-description parameter))))

(defun lookup-type (attribute schemable)
  (let ((parameter (lookup-parameter attribute schemable)))
    (and parameter (parameter-type parameter))))

(defun lookup-parameter (attribute schemable)
  (lookup- attribute schemable))

(defgeneric implementation-function (implementation)
  (:documentation "Returns a function which transforms an input tuple to a tuple, relation, or nil according to IMPLEMENTATION's logic.")
  (:method ((impl internal-implementation))
    (awhen (find-symbol (implementation-name impl) (implementation-module impl))
      (symbol-function it)))
  (:method ((impl external-implementation))
    (lambda (in acc)
    ;;; From APPLY-TRANSFORMATION:
    ;;; If ACC is a tuple (not NIL), then it is the accumulator value of a reduction.
    ;;; Transformation functions which do not implement a reduction can ignore this value.
      (execute-external-implementation impl in acc))))

(defgeneric execute-external-implementation (impl in &optional acc)
  (:method ((impl external-implementation) (in wb-map) &optional acc)
    (declare (ignore acc))
    (let* ((external-input-json (interface:dump-json-to-string :tuple in))
           ;; We could get rid of these explicit strings and just pipe to/from streams, but this is easy, especially for debugging and light testing.
           (external-output-json (with-input-from-string (in external-input-json)
                                   (uiop:run-program (format nil "~A" (implementation-external-path impl))
                                                     :input in
                                                     :output 'string)))
           (output (interface:get-json-data-from-string external-output-json))
           ;; Join output to input. This will yield an empty result if inconsistent.
           (joined (join in output)))
      joined)))

(defclass engine () ())


(defun set-same-equal (a b)
  (and (subsetp a b :test #'same) (subsetp b a :test #'same)))

(defgeneric same (a b)
  (:method
      ;; Things of different type are never the same.
      ;; Things of types without specialization are the same if they are equal.
      ((a t) (b t))
    (equal? a b))
  (:method ((a signature) (b signature))
    (sig-equal a b))
  (:method ((a transformation) (b transformation))
    (and (same (transformation-signature a) (transformation-signature b))
	 (same (transformation-implementation a) (transformation-implementation b))))
  (:method ((a component) (b component))
    (set-same-equal (component-transformations a) (component-transformations b)))
  (:method ((a implementation) (b implementation))
    (and (same (implementation-module a) (implementation-module b))
	 (same (implementation-name a) (implementation-name b))))
  (:method ((a parameter) (b parameter))
    (and (same (parameter-name a) (parameter-name b))
	 (same (parameter-description a) (parameter-description b))
	 (same (parameter-type a) (parameter-type b))))
  (:method ((a schema) (b schema))
    (and (same (schema-description a) (schema-description b))
	 (set-same-equal (schema-parameters a) (schema-parameters b))))
  (:method ((a system) (b system))
    (and (same (system-schema a) (system-schema b))
	 (set-same-equal (system-components a) (system-components b))
	 (same (system-subsystems a) (system-subsystems b))
					;(same (system-data a) (system-data b))
	 )))

(test same
  ;; Regression test.
  (is (same (tuple (r (relation ())))
	    (tuple (r (relation ()))))))

(defgeneric representation (thing)
  (:method ((thing t))
    thing)
  (:method ((list list))
    (mapcar #'representation list))
  (:method ((set set))
    (convert 'list set))
  (:method ((tuple wb-map))
    `(tuple ,@(sort (loop for attr in (convert 'list (attributes tuple))
		       collect (list attr (representation (tref attr tuple))))
		    #'string< :key #'car)))
  (:method ((relation relation))
    (let ((attributes (convert 'list (fset:sort (attributes relation) #'fset:compare))))
    `(relation ,attributes
	       ,@(loop for tuple in (convert 'list (tuples relation))
		    collect (loop for attr in attributes
			       collect (representation (tref attr tuple)))))))
  (:method ((sig signature))
    `(sig  ,(representation (signature-input sig)) -> ,(representation (signature-output sig)))))

(test representation
  (flet ((roundtrip (x)
	   (is (same x (eval (representation x))))))
    (let* ((tuple (tuple (a 1) (b 2) (c 3)))
	   (r1 (relation (a b c) (1 2 3) (4 5 6)))
	   (r2 (relation (x t) (9 tuple) (8 (tuple (a 9) (b 8) (c 7))))))
      (roundtrip tuple)
      (roundtrip r1)
      (roundtrip r2)

      (is (equalp '(tuple (a 1) (b 2) (c 3)) (representation tuple)))
      (is (equalp '(relation (a b c)
		    (1 2 3)
		    (4 5 6))
		  (representation r1)))
      (is (equalp '(relation (x t)
		    (8 (tuple (a 9) (b 8) (c 7)))
		    (9 (tuple (a 1) (b 2) (c 3))))
		  (representation r2))))))

(defgeneric satisfies-input-p (attributed b)
  ;; TODO: Extend to handle wildcards with WILDCARD-MATCHES, etc.
  (:documentation "True if all inputs to B are attributes of ATTRIBUTED.")
  ;; FIXME: Make type of A ensure ATTRIBUTES.
  (:method ((a t) (b t)) nil)
  (:method ((a t) (b transformation)) (satisfies-input-p a (transformation-signature b)))
  (:method ((a t) (b signature))
    (or (empty? (signature-input b)) ;; FIXME: avoid special case here.
	(subset? (signature-input b) (attributes a)))))

(defgeneric combine-potential-relations (a b)
  (:method ((a list) (b t))
    (combine-potential-relations (convert 'set a) b))
  (:method ((a t) (b list))
    (combine-potential-relations a (convert 'set b)))
  (:method ((a relation) (b relation))
    ;; FIXME: Check that headings are compatible.
    (%make-relation (union (tuples a) (tuples b))))
  (:method ((a wb-map) (b wb-map))
    (%make-relation (set a b)))
  (:method ((a wb-map) (b set))
    (%make-relation (with b a)))
  (:method ((a list) (b wb-map))
    (combine-potential-relations b a))
  (:method ((a set) (b set))
    ;; We assume aruguments to COMBINE-POTENTIAL-RELATIONS can be destructively modified.
    (%make-relation (union a b)))
  (:method ((a wb-map) (b relation))
    ;; FIXME: Check that headings are compatible.
    (%make-relation (with (tuples b) a)))
  (:method ((a relation) (b wb-map))
    (combine-potential-relations b a))
  (:method ((a set) (b relation))
    (%make-relation (union a (tuples b))))
  (:method ((a relation) (b set))
    (combine-potential-relations b a)))

(deftype transformation-spec () '(or transformation symbol))
(deftype pipeline () '(cons transformation-spec))

;;; Parallelism
(defvar *use-parallel-solve* nil)
(defvar *use-parallel-apply-transformation* nil)
(defparameter *threadpool-size* 8)

(defun init-parallelism ()
  (setf lparallel:*kernel* (lparallel:make-kernel *threadpool-size*)))

(defun ensure-parallelism-initialized ()
  (unless lparallel:*kernel*
    (init-parallelism)))

(defgeneric apply-transformation (transformation attributed &optional acc)
  (:documentation "Applies a transformation to data, returning strict data, i.e. (RELATION, TUPLE, or NIL)")
  (:method ((f function) (tuple wb-map) &optional acc)
    ;;; All transformation applications need to go through here.
    ;;; TODO: Transformation should fail if any input is removed from output.
    ;;; This is where the real work happens.
    ;;; All transformation functions must take a tuple and an optional ACC tuple.
    ;;; If ACC is a tuple (not NIL), then it is the accumulator value of a reduction.
    ;;; Transformation functions which do not implement a reduction can ignore this value.
    (let* ((result (funcall f tuple acc)))
      (join tuple result)))
  (:method ((impl internal-implementation) (tuple t) &optional acc)
    (awhen (implementation-function impl)
      (apply-transformation it tuple acc)))
  (:method ((impl external-implementation) (tuple t) &optional acc)
    (awhen (implementation-function impl)
      (apply-transformation it tuple acc)))
  (:method ((transformation-name symbol) (tuple t) &optional acc)
    (awhen (find-transformation transformation-name) (apply-transformation it tuple acc)))
  (:method ((transformation t) (null null) &optional acc)
    (declare (ignore acc))
    ;; Everything collapses to NIL. This simplifies uniform handling of tuples/relations, but may need to be revisited.
    nil)
  (:method ((transformation transformation) (tuple wb-map) &optional acc)
    (assert (satisfies-input-p tuple transformation))
    (apply-transformation (transformation-implementation transformation) tuple acc))
  (:method ((transformation t) (relation simple-relation) &optional acc)
    (check-type acc null)
    (cond
      ((or (reducer? transformation)
	   (grouper? transformation))
       (when *use-parallel-apply-transformation*
         (error "groupind and reduction are not yet supported with parallel transformation application.")
         )
       (group-reduce-relation transformation relation))
      ((and *use-parallel-apply-transformation* (> (cardinality relation) 1))
       (let* ((schema-package orient.interface:*schema-package*)
              (results (pmapcar (lambda (tuple)
                                  (let* ((orient.interface:*schema-package* schema-package)
                                         (*package* schema-package)
                                         (json:*json-symbols-package* schema-package))
                                    (apply-transformation transformation tuple)))
                                (convert 'list (tuples relation)))))
         (reduce #'combine-potential-relations
                 results
                 :initial-value nil)))
      (t
       (reduce #'combine-potential-relations
	       (image (lambda (tuple)
			(apply-transformation transformation tuple))
		      (tuples relation))
	       :initial-value nil))))
  (:method ((pipeline list) (tuple wb-map) &optional acc)
    "Sequentially apply list of transformations, reducing from initial value of TUPLE."
    (check-type pipeline pipeline)
    ;; ACC only applies within a single transformation's processing of multiple tuples (i.e. a relation).
    (check-type acc null)
    (reduce (lambda (tuple transformation)
	      (apply-transformation transformation tuple))
	    pipeline
	    :initial-value tuple)))

(defgeneric group-reduce-relation (transformation relation)
  (:method ((transformation t) (relation simple-relation))
    (cond
      ((grouper? transformation)
       (let* ((signature (transformation-signature transformation))
	      (into-attr (signature-group-into signature))
	      (grouped (group-with-signature relation signature)))
	 (cond
	   ((reducer? transformation)
	    ;; TODO: Can we avoid the conceptually clear but somewhat wasteful process of reducing into a tuple attribute then unwrapping?
	    (map-relation (lambda (tuple)
			    (setf (tref into-attr tuple)
				  (reduce-relation transformation (tref into-attr tuple)))
			    (unwrap tuple into-attr))
			  grouped))
	   (t grouped))))
      (t (reduce-relation transformation relation)))))

(defun group-with-signature (relation signature)
  (multiple-value-bind (by-attributes invert)
      (aif (signature-group-by signature)
	   (values it nil)
	   (values (signature-group signature) t))
    (group relation by-attributes (signature-group-into signature) :invert invert)))		

(defgeneric reduce-relation (transformation relation)
  (:method ((transformation t) (relation simple-relation))
    (assert (reducer? transformation))
    (let* ((sig (transformation-signature transformation))
	   (impl (transformation-implementation transformation))
	   (reduced (reduce (lambda (acc tuple)
			      (apply-transformation impl tuple acc))
			    (tuples relation)
			    :initial-value nil)))
      (project (signature-input sig) reduced :invert t))))

(test reducer-transformation
  (let ((f (transformation ((a &acc (acc 4) (all-a '())) -> (acc all-a)) ==
			   (values (+ acc a) (cons a all-a))))
	(data (relation (a) (1) (2) (3))))
    (is (same (apply-transformation f data nil)
	      (tuple (acc 10) (all-a '(3 2 1)))))))

(test group-transformation
  ;;; FIXME: We should probably not blindly ignore the actual transformation,
  ;;; but rather either require an 'identity transformation' or some other intentional signal.
  (let ((f (transformation ((a b &group a &into x) -> (x)) == :ignored))
	(data (relation (a b) (1 2) (2 2) (3 3))))
    (is (same (relation (b x)
			(2 (relation (a) (1) (2)))
			(3 (relation (a) (3))))
	      (apply-transformation f data)))))

(test group-by-transformation
  ;;; FIXME: We should probably not blindly ignore the actual transformation,
  ;;; but rather either require an 'identity transformation' or some other intentional signal.
  (let ((f (transformation ((a b c &group-by a &into x) -> (x)) == :ignored))
	(data (relation (a b c) (1 2 3) (2 2 4) (2 3 5))))
    (is (same (relation (a x)
			(1 (relation (b c) (2 3)))
			(2 (relation (b c) (2 4) (3 5))))
	      (apply-transformation f data)))))

#+(or)
(test group-and-group-by-transformation
  ;;; FIXME: We should probably not blindly ignore the actual transformation,
  ;;; but rather either require an 'identity transformation' or some other intentional signal.
  (let ((f (transformation ((a b c d &group b &group-by a &into x) -> (x)) == :ignored))
	(data (relation (a b c d) (1 2 3 5) (2 2 4 5) (2 3 5 5))))
    (is (same (relation (a d x)
			(1 5 (relation (b c) (2 3)))
			(2 5 (relation (b c) (2 4) (3 5))))
	      (apply-transformation f data)))))

(test grouper-reducer-transformation
  (let ((f (transformation ((a b &group a &acc (sum 0) (all-a (set))) -> (sum all-a))
			   == (values (+ sum a) (with all-a a))))
	(data (relation (a b) (5 2) (6 2) (7 3))))
    (is (same (relation (b sum all-a)
			(2 11 (set 5 6))
			(3 7 (set 7)))
	      (apply-transformation f data)))))

(defgeneric compose-signatures (a b)
  ;; TODO: Make type of TUPLE ensure signature.
  (:method ((signature signature) (tuple wb-map))
    (let ((attributes (attributes tuple)))
      (make-signature (union (signature-input signature) attributes)
		      (union (signature-output signature) attributes))))
  (:method ((a signature) (b signature))
    (make-signature (union (signature-input a) (signature-input b))
		    (union (signature-output a) (signature-output b)))))

(defun pipeline-signature (pipeline)
  ;; A plan is a pipeline -- a list of transformations.
  (and pipeline
       (let ((signatures (mapcar #'transformation-signature pipeline)))
	 (reduce #'compose-signatures signatures :initial-value (first signatures)))))

(defclass plan-profile () ((transformations-tried :initform 0 :accessor transformations-tried)))

(defmethod print-object ((p plan-profile) (stream t))
  (format stream "<PLAN-PROFILE; transformations-tried: ~d>" (transformations-tried p)))

(defvar *plan-profile*)

(defgeneric signature-satisfies-p (signature thing)
  (:documentation "True if thing's inputs are a subset of signature's.")
  (:method ((signature signature) (transformation transformation))
    (subset? (signature-input (transformation-signature transformation)) (signature-input signature)))
  (:method ((signature signature) (component component))
    (some (lambda (transformation) (signature-satisfies-p signature transformation))
	  (component-transformations component)))
  (:method ((signature signature) (null null))
    nil))

(defvar *trace-plan* nil)

(defun debug-plan (&rest args)
  (when *trace-plan*
    (print args)))

(defclass problem ()
  ((input :initarg :input :initform (empty-set) :accessor problem-input :type set)
   (output :initarg :output :initform (empty-set) :accessor problem-output :type set)))

;; TODO: Use PROBLEM rather than SIGNATURE in PLAN/SOLVE.
(defgeneric plan (system signature)
  (:documentation "Find a plan for system which meets signature. That is, the plan takes signatures inputs and produces its outputs.")
  (:method ((system system) (signature signature))
    (let* ((*plan-profile* (make-instance 'plan-profile))
	   (plan (%plan :system system (pruned-signature signature) '())))
      (debug-plan :found-plan plan)
      ;; For now, ignore all but first plan.
      (values plan *plan-profile*))))

(defgeneric %plan (system element signature plan)
  (:documentation "Accumulates and returns a plan in reverse (result needs to be reversed).")
  (:method ((start (eql :system)) (system system) (signature signature) (plan list))
    ;; If the final pipeline doesn't satisfy SIGNATURE's output, PLAN is no good.
    (let* ((reversed-plan (%plan :component-list (all-system-components system) signature plan))
	   (plan (reverse reversed-plan))
	   (plan-signature (pipeline-signature plan)))

      ;; TODO: Figure out when/how we can perform this short-circuit safely?
      ;; (when (subsetp (signature-output signature) (signature-input signature))
      ;; 	;; This shortcuts constraint checks on already filled variables.
      ;; 	(return-from %plan (list (identity-transformation))))
      
      ;; FIXME: this will fail if there is no plan but none was needed. Fix that.

      (debug-plan :signature-output (signature-output signature))
      ;; TODO: prune extraneous transformations from the plan.
      (let ((needed-output (set-difference (signature-output signature) (signature-input signature))))
	;; If the plan's signature provides output which is a superset of that which is needed, return the plan.
	(and plan-signature (subset? needed-output (signature-output plan-signature))
	     plan))))
  (:method ((selector (eql :component-list)) (component-list list) (signature signature) (plan list))
    (let* ((candidates (remove-if-not (lambda (component) (signature-satisfies-p signature component))
				      component-list)))
      (debug-plan :signature signature :component-list component-list :candidates candidates :plan plan)
      (or (some (lambda (component)
		  (%plan (remove component component-list) ;; Each component can only be used once.
			 component signature plan))
		candidates)
	  ;; If no component in COMPONENT-LIST leads to a new plan, the current PLAN is exhaustive (whether successful or not).
	  plan)))
  (:method ((remaining-component-list list) (component component) (signature signature) (plan list))
    (debug-plan :planning :component component)
    (let ((candidates (remove-if-not (lambda (tr) (signature-satisfies-p signature tr)) (component-transformations component))))
      (some (lambda (transformation)
	      (%plan remaining-component-list transformation signature plan))
	    candidates)))
  (:method ((remaining-component-list list) (transformation transformation) (signature signature) (plan list))
    (debug-plan :transformation transformation :signature signature :plan plan)    
    (incf (transformations-tried *plan-profile*))
    (let* ((tran-sig (transformation-signature transformation))
	   (new-plan (cons transformation plan))
	   (new-signature (make-signature (union (signature-input signature) (signature-output tran-sig))
					  (signature-output signature))))

      ;; TODO: Under what circumstances, if any, can we skip following a branch forward?
      
      ;; Add the transformation to the plan and update the signature to satisfy.
      (debug-plan :new-plan new-plan :new-signature new-signature)
      (%plan :component-list remaining-component-list new-signature new-plan))))

(defgeneric apply-signature (signature problem)
  (:method ((signature signature) (to-signature signature))
    (etypecase signature
      (grouper
       ;; A signature can only be applied to one which satisfies its inputs.
       (assert (subset? (signature-input signature) (signature-input to-signature)))
       (let ((new-inputs (compute-group-attributes (signature-input to-signature)
						   (signature-group-by signature)
						   (signature-group-into signature))))
	 (make-signature new-inputs (signature-output to-signature))))
      (t
       (make-signature (union (signature-input signature) (signature-output to-signature))
		       (signature-output to-signature))))))

(defun flagp (symbol)
  (eql #\! (char (symbol-name symbol) 0)))

(defun make-flag (symbol)
  (symbolconc '! symbol))

(defun flags (attributed)
  (filter #'flagp (attributes attributed)))

(defun flag-symbol (tagged-flag-symbol)
  (assert (flagp tagged-flag-symbol))
  (intern (subseq (symbol-name tagged-flag-symbol) 1)))

(defgeneric separate-by-flag-combinations (relation-or-relation-list)
  (:method ((relation relation))
    (let* ((separated (make-relation (image (lambda (tpl)
                                              (let ((f (flags tpl)))
                                                (tuple (flags (project f tpl))
                                                       (non-flags (project f tpl :invert t)))))
                                            (tuples relation))))
           (grouped (group separated '(flags) 'relation)))
      (map-relation (tfn (flags relation)
                      (tuple (flags flags)
                             (relation (unwrap relation 'non-flags))))
                    grouped)))
  (:method ((list list))
    (apply #'disjoin (mapcar #'separate-by-flag-combinations list))))

(test separate-by-flag-combinations
  (let* ((j (LIST (RELATION (X !A !B) (10 NIL T) (10 T NIL))
                  (RELATION (X Y !C) (10 88 T) (10 99 T))))
         (joined (apply #'join j))
         (separated (separate-by-flag-combinations joined)))
    (is (same (RELATION (FLAGS RELATION)
                        ((TUPLE (!A NIL) (!B T) (!C T)) (RELATION (X Y) (10 88) (10 99)))
                        ((TUPLE (!A T) (!B NIL) (!C T)) (RELATION (X Y) (10 88) (10 99))))
              separated))))

(defmethod defaulted-initial-data ((system system) (provided t) &key override-data)
  ;; TODO: allow merging of provided data.
  (let ((defaulted (or (typecase provided
			 (wb-map provided)
			 (relation provided)
			 ((cons (or wb-map relation))
			  (apply #'join provided)))
		       (and (all-system-data system)
			    (apply #'join (all-system-data system)))
		       ;; Default default is an empty tuple, since APPLY-TRANSFORMATION
		       ;; treats NIL as an empty *relation*.
		       (tuple))))
    (when override-data
      (do-map (attr val override-data)
	(adjoinf defaulted attr val)))
    defaulted))

(defgeneric transformation-description (transformation)
  (:method ((transformation transformation))
    (let ((source (transformation-source transformation)))
      (typecase source
	(null "<?>")
	(t nil (format nil "~A" source))))))

(defgeneric describe-transformation-calculation (transformation)
  (:method ((transformation transformation))
    (transformation-description transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface

(defvar *current-construction*)

(defun use-construction (system &key data)
  (let ((system (find-system system)))
    (when data
      (setf (system-data system) (if (listp data) data (list data))))
    (setq *current-construction* (find-system system))))

(defun set-construction-parameter (attribute value)
  ;; Quick and dirty for now, just set data in first naked tuple we find.
  (let ((tuple (some (lambda (x) (and (typep x 'tuple) x)) (system-data *current-construction*))))

    (cond (tuple (adjoinf tuple attribute value))
	  (t  (push (tuple (attribute value)) (system-data *current-construction*))))))

(defgeneric solve (system signature initial-data &key report override-data)
  (:documentation "REPORT, if true, specifies format to use, defaulting to plain text.")
  (:method ((system-name symbol) (signature t) (initial-data t) &rest keys)
    (apply #'solve (find-system system-name) signature initial-data keys))
  (:method ((system system) (signature null) (initial-data t) &rest keys &key report override-data)
    (declare (ignore report))
    (let* ((initial-value (defaulted-initial-data system initial-data :override-data override-data))
	   (signature (or signature (make-signature (attributes initial-value) (attributes initial-value)))))
      (apply #'solve system signature initial-value keys)))
  (:method ((system system) (signature signature) (initial-data t) &key report override-data)
    (ensure-parallelism-initialized)
    (let ((initial-value (defaulted-initial-data system initial-data :override-data override-data))
	  (plan (plan system signature)))
      (cond ((and plan (satisfies-input-p initial-data signature))
             (cond
               ((and *use-parallel-solve* (not report))
                (let* ((reducer (make-pipeline-reducer system :report report))
                       (schema-package orient.interface:*schema-package*)
                       (results (pmapcar (lambda (tuple)
                                           (let* ((orient.interface:*schema-package* schema-package)
                                                  (*package* schema-package)
                                                  (json:*json-symbols-package* schema-package))

                                             (%solve plan reducer tuple nil)))
                                           (convert 'list (tuples (ensure-relation initial-value)))))
                       (final-result (reduce #'combine-potential-relations results)))
                  (values final-result plan nil initial-value)))

               (t
                (when *use-parallel-solve*
                  (warn "Trying to solve in parallel with REPORT set. Not yet supported so falling back to no parallelism."))
                (let ((reducer (make-pipeline-reducer system :report report)))
                  (%solve plan reducer initial-value report)))))
            (t (values (awhen plan
                         (awhen (pipeline-signature it)
                           (awhen (signature-output it)
                             (empty-relation it))))
                       plan nil initial-value))))))

(defun %solve (plan reducer initial-value report)
  (let ((reduce-result (reduce reducer plan :initial-value (list initial-value '()))))
    (destructuring-bind (result report-results) reduce-result
      (values result plan (synthesize-report-steps report report-results) initial-value))))

(define-condition pipeline-reduction-condition ()
  ((transformation :initarg :transformation :accessor pipeline-reduction-transformation)
   (input :initarg :input :accessor pipeline-reduction-input)))

(define-condition pipeline-reduction-error (pipeline-reduction-condition error)
  ((error :initarg :error :accessor inner-error))
  (:report (lambda (condition stream)
             ;; FIXME: implement.
             (let* ((transformation (pipeline-reduction-transformation condition))
                    (input (pipeline-reduction-input condition))
                    (projected (project (signature-input (transformation-signature transformation)) input)))
               (format stream "PIPELINE-REDUCTION-ERROR( ~A ) while applying transformation, ~A input to (partial) input: ~A."
                       (inner-error condition)
                       transformation
                       projected)))))

(defun make-pipeline-reducer (system &key report)
  (lambda (acc transformation)
    (handler-bind
        ((error (lambda (e)
                  (let ((wrapper (make-condition 'pipeline-reduction-error
                                                 :transformation transformation
                                                 :input (car acc)
                                                 :error e)))
                    (signal wrapper)))))
      (destructuring-bind (tuple-or-relation report-results) acc
        (let* ((transformed (apply-transformation transformation tuple-or-relation))
               (new-report (when report
                             (append report-results (list (report-step transformed transformation system :format report))))))
          (list transformed new-report))))))

(defun report-step (step transformation system &key format)
  (let* ((tuples (ensure-tuples step))
	 (multiple (> (size tuples) 1)))
    (reduce #'append
	    (gmap:gmap :list
		       (lambda (i tuple)
			 (create-tuple-report-step format tuple transformation system :n (and multiple i)))
		       (:index 0) (:set tuples)))))

(defgeneric synthesize-report-steps (format steps)
  (:method ((format t) (steps list))
    (reduce (lambda (&optional acc step)
	      (concatenate 'string acc step))
	    steps
	    :initial-value "")))

(defgeneric create-tuple-report-step (format tuple transformation system &key n)
  (:documentation "N is index of TUPLE if TUPLE has siblings.")
  (:method ((format t) (tuple wb-map) (transformation transformation) (system system) &key n)
    ;; FIXME: This can probably be done with GMAP.
    (reduce (lambda (acc attr val)
	      (cons (format nil "~&~@[~A. ~]~A: ~A = ~A~% ~A~%"
			    n
			    attr
			    (describe-transformation-calculation transformation)
			    val
			    (let ((desc (and system (lookup-description attr system))))
			      (if desc (format nil "   ~A~%" desc) "")))
		    acc))
	    (filter (lambda (attr val)
		      (declare (ignore val))
		      (contains? (signature-output (transformation-signature transformation)) attr))
		    tuple)
	    :initial-value '())))

(defgeneric report (thing context)
  (:method ((null null) (context t))
    "Nothing to report.")
  (:method ((tuple wb-map) (schema schema))
    (with-output-to-string (out)
      (do-map (attr val tuple) 
	(format out "~&~A: ~A~% ~A~%"
		attr
		val
		(let ((desc (and schema (lookup-description attr schema))))
		  (if desc (format nil "   ~A~%" desc) ""))))))
  (:method ((tuple wb-map) (system system))
    (report tuple (find-schema (system-schema system))))
  (:method ((tuple wb-map) (system-name symbol))
    (when system-name
      (report tuple (find-system system-name)))))

(defun report-data (&optional (system *current-construction*))
  (mapcar (lambda (data) (report data system)) (system-data system)))

;; SYSTEM-CACHE-KEY included because serializing system here is not yet implemented without circular dependencies.
(defun solve-for (system output &optional initial-data &key report override-data project-solution cache system-cache-key values-deserializer values-serializer)
  "Returns four values: solution, plan, report, and defaulted-data."
  (let* ((system (find-system system))
         ;; We need to fill defaults here (somewhat redundantly) in order to calculate SIG below.
         (defaulted (defaulted-initial-data system initial-data :override-data override-data))
         (sig (make-signature (attributes defaulted) output)))
    (multiple-value-bind (solution plan report defaulted-data)
        (if cache
            (cache:call-with-cache cache #'solve
                                   (list system-cache-key sig defaulted)
                                   (list system sig defaulted
                                         :report report
                                         :override-data override-data)
                                   :values-serializer values-serializer
                                   :values-deserializer values-deserializer)
            (solve system sig defaulted :report report :override-data override-data))
      (values (if project-solution
                  (project output solution)
                  solution)
              plan report defaulted-data))))

(defun report-solution-for (output &key system initial-data (format t) override-data project-solution return-plan return-defaulted-data)
  (let ((*use-parallel-solve* nil))
    (multiple-value-bind (solution plan report defaulted-data) (solve-for system output initial-data
                                                                          :report format
                                                                          :override-data override-data
                                                                          :project-solution project-solution)
      (if return-plan
          (values (if solution report "NO SOLUTION") solution defaulted-data plan)
          (values (if solution report "NO SOLUTION") solution defaulted-data)))))

(defun private-attr-p (attr)
  "If ATTR name ends with %, don't include in reports."
  (let* ((name (symbol-name attr))
	 (last-char (char name (1- (length name)))))
    (eql #\% last-char)))

(defun ask (system output &optional initial-data &key override-data)
  "Like solve-for but only returns the requested attributes in response tuple."
  (let ((solution (solve-for system output initial-data :override-data override-data)))
    (when solution
      (project output solution))))

(defun plan-for (system output &optional initial-data &key override-data)
  (let* ((defaulted (defaulted-initial-data system initial-data :override-data override-data))
	 (sig (make-signature (and defaulted (attributes defaulted)) output)))
    (plan system sig)))

(defun build-relation (from-tuple adding-attributes value-rows)
  (let ((tuples (loop for row in value-rows
		   collect (let ((base from-tuple))
			     (loop for attr in adding-attributes
				for val in row
				do (adjoinf base attr val))
			     base))))
    (make-relation tuples)))

(defun clean-tmps (attributed)
  (project (filter #'tmp-p (attributes attributed)) attributed :invert t))

(defstruct plan-graph (edges) (nodes))

(defun node-label (symbol &key include-tmps)
  (let ((name (symbol-name symbol)))
    (if include-tmps
	name
	(attr-base-name symbol))))

(defmethod intern-node ((graph plan-graph) label)
;  (let ((label (node-label attribute)))
    (or (gethash label (plan-graph-nodes graph))
	(setf (gethash label (plan-graph-nodes graph))
	      (make-instance 'cl-dot:node
			     :attributes (list :label label)))));)

(defmethod cl-dot:graph-object-node ((graph plan-graph) attribute)
  (intern-node graph attribute))

(defmethod cl-dot:graph-object-edges ((graph plan-graph))
  (coerce (loop for (from to) in (plan-graph-edges graph)
	     unless (or (equal from to)
			(null from)
			(null to))
	     collect (list from to))
	  'vector))

(defun generate-directed-graph (plan)
  (make-plan-graph :edges
		   (remove-duplicates (loop for transformation in plan
					 for signature = (transformation-signature transformation)
					 append (loop for dependency in (convert 'list (signature-input signature))
						   append (loop for target in (convert 'list (signature-output signature))
							     collect (list (node-label dependency) (node-label target)))))
				      :test #'equal)
		   :nodes (make-hash-table :test #'equal)))

(defun dot-graph-from-plan (plan)
  (cl-dot:generate-graph-from-roots (generate-directed-graph plan) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests / Examples

;; TODO: Split these up.
(test orient-tests
  "General tests planning and solving."
  (let* ((d1 (tuple (a 2) (b 3) (c 4)))
	 (d2 (tuple (a 2) (b 3) (c 4) (d 5)))
	 (d3 (tuple (x 5) (y 6) (z 7)))
	 ;;(d4 (tuple (a 1) (b 2) (c 3)))

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
    
    (is (same (solve s2 sig2 d1) (empty-relation)) "(solve s2 sig2 d1)")
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

(test planning-terminates
  "Regression test for infinite stack bug."
  (let* ((t1 (transformation ((b c d) -> (e f)) == (let ((x (+ b c d)))
						     (values (* x b) (* x c)))))
	 (s1 (sys ((component (t1))))))

    (finishes (plan s1 (make-signature '(b c d) '(e)))))	)
