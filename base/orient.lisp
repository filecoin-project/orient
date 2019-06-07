(in-package "ORIENT")
(def-suite orient-suite :description "Test the orient package.")
(in-suite orient-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defsetf tref set-tref))

(defmethod trem ((attribute t) (tuple tuple))
  "Remove ATTRIBUTE from TUPLE"
  (remhash attribute (tuple-hash-table tuple))
  tuple)

(defclass relation () ())

(defgeneric attributes (tuple)
  (:method ((d tuple))
    (loop for attribute being the hash-keys of (tuple-hash-table d)
       collect attribute))
  (:method ((r relation))
    (and (first (tuples r))
	 (attributes (first (tuples r))))))

(defclass simple-relation (relation)
  ((attributes :initarg :attributes :accessor attributes)
   (tuples :initarg :tuples :initform nil :accessor tuples)))

(defmethod print-object ((relation relation) (stream t))
  (format stream "<RELATION ~S ~S>" (sort (attributes relation) #'string<) (tuples relation)))

(defgeneric ensure-tuples (attributed)
  (:method ((tuple tuple))
    (list tuple))
  (:method ((relation relation))
    (tuples relation))
  (:method ((null null)) nil))

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
;; NOTE: This is destructive!
;; TODO: We should really have a non-destructive/pure-functional version.
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
  (:method ((null null)) '())
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

      (is (same (relation (d e c) (1 2 3) (4 5 6))
		(rename-attributes '((a d) (b e))
				   (relation (a b c) (1 2 3) (4 5 6))))))

;; Example, filter tuples where b is not 5:
;; (restrict (tfn (b) (= b 5)) asdf)
(defgeneric restrict (tuple-predicate relation)
  (:method ((tpred function) (relation relation))
    (make-relation (remove-if-not tpred (tuples relation)))))

(test restrict "Test RESTRICT."
      (is (same (relation (a b c) (4 5 6))
		(restrict (tfn (b) (= b 5))
			  (relation (a b c) (1 2 3) (4 5 6) (7 8 9))))))

(defgeneric project (attributes attributed)
  (:method ((attributes list) (null null))
    nil)
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

(defgeneric extract (relation)
  (:method ((relation relation))
    (and (= (cardinality relation) 1)
	 (first (tuples relation)))))

(defclass parameter ()
  ((name :initarg :name :initform (error "name missing") :accessor parameter-name)
   (description :initarg :description :accessor parameter-description)
   (type :initarg :type :initform nil :accessor parameter-type)))

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


(defmethod print-object ((trans transformation) (stream t))
  (let ((implementation (transformation-implementation trans)))
    (format stream "(TRANSFORMATION ~S === ~S)" (transformation-signature trans) (if (functionp implementation) "FN()" implementation))))

(defun identity-transformation () (make-instance 'transformation :implementation (lambda (attributed) attributed)))

(defmethod print-object ((comp component) (stream t))
  (format stream "(COMPONENT ~S)" (component-transformations comp)))

(defclass problem ()
  ((signature :initarg :signature :initform (make-signature '() '()) :accessor problem-signature)))

(defgeneric all-system-components (system)
  (:method ((system system))
    (reduce #'append (cons (system-components system)
			   (mapcar #'all-system-components (system-subsystems system))))))

(defgeneric all-system-data (system)
  (:method ((system system))
    (reduce #'append (cons (system-data system) (mapcar #'all-system-data (system-subsystems system))))))

(defmethod system-schema :around ((system system))
  (find-schema (call-next-method)))

(defmethod print-object ((sys system) (stream t))
  (format stream "(sys ~S :schema ~S)" (system-components sys) (system-schema sys)))

(defgeneric lookup (attribute schemable)
  (:method ((attribute symbol) (schema schema))
    (find attribute (schema-parameters schema) :key #'parameter-name))
  (:method ((attribute symbol) (system system))
    (or (awhen (find-schema (system-schema system))
	  (lookup attribute it))
	(some (lambda (system) (lookup attribute system))
	      (system-subsystems system)))))

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
  ;; Numbers are same if they are =.
  (:method ((a number) (b number))
    (= a b))

  ;; TODO: Implement for other lists and other compound types.
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

(deftype transformation-spec () '(or transformation symbol))

;; TODO: Transformation should fail if any output changes the value of an input.
(defgeneric apply-transformation (transformation tuple)
  (:method ((transformation-name symbol) (tuple t))
    (awhen (find-transformation transformation-name) (apply-transformation it tuple)))
  (:method ((transformation t) (null null))
    ;; Everything collapses to NIL. This simplifies uniform handling of tuples/relations, but may need to be revisited.
    nil)
  (:method ((transformation transformation) (tuple tuple))
    (assert (satisfies-input-p tuple transformation))
    (apply-transformation (transformation-implementation transformation) tuple))
  (:method ((transformation transformation) (relation simple-relation))
    (reduce #'combine-potential-relations
	    (mapcar (lambda (tuple)
		      (apply-transformation transformation tuple))
		    (tuples relation))
	    :initial-value nil))
  (:method ((list list) (tuple tuple))
    (check-type list (cons transformation-spec))
    (reduce (lambda (tuple transformation)
	      (apply-transformation transformation tuple))
	    list
	    :initial-value tuple))
  (:method ((f function) (tuple tuple))
    ;; All transformation applications need to go through here.
    (let* ((result (funcall f tuple)))
      (join tuple result))))

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
  (:documentation "True if thing's inputs are a subset of signature's.")
  (:method ((signature signature) (transformation transformation))
    (subsetp (signature-input (transformation-signature transformation)) (signature-input signature)))
  (:method ((signature signature) (component component))
    (some (lambda (transformation) (signature-satisfies-p signature transformation))
	  (component-transformations component))))

(defvar *trace-plan* nil)

(defun debug-plan (&rest args)
  (when *trace-plan*
    (print args)))

(defgeneric plan (system signature)
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

      (debug-plan :signature-output  (signature-output signature))
      ;; TODO: prune extraneous transformations from the plan.
      (let ((needed-output (set-difference (signature-output signature) (signature-input signature))))
	(and plan-signature (subsetp needed-output (signature-output plan-signature))
	     plan))))
  (:method ((selector (eql :component-list)) (component-list list) (signature signature) (plan list))
    (let* ((candidates (remove-if-not (lambda (c) (signature-satisfies-p signature c)) component-list))	   )
      (debug-plan :signature signature :component-list component-list :candidates candidates :plan plan)
      (or (some (lambda (component)
		  (%plan (remove component component-list) ;; Each component can only be used once.
			 component signature plan))
		candidates)
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
    (let* ((tran-sig (transformation-signature transformation)))
      ;; TODO: Under what circumstances, if any, can we skip following a branch forward?
      
      ;; Add the transformation to the plan and update the signature to satisfy.
      (let* ((new-plan (cons transformation plan))
	     (new-signature (make-signature (union (signature-input signature) (signature-output tran-sig))
					    (signature-output signature))))
	(debug-plan :new-plan new-plan :new-signature new-signature)
	(%plan :component-list remaining-component-list new-signature new-plan)))))

(defmethod defaulted-initial-data ((system system) (provided t) &key override-data)
  ;; TODO: allow merging of provided data.
  (let ((defaulted (duplicate (or provided
				  (and (all-system-data system)
				       (apply #'join (all-system-data system)))))))
    (loop for (key value) in (and override-data (tuple-pairs override-data))
       do (setf (tref key defaulted) value))
    defaulted))

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

    (cond (tuple (setf (tref attribute tuple) value))
	  (t  (push (tuple (attribute value)) (system-data *current-construction*))))))

(defgeneric solve (system signature initial-data &key report override-data)
  (:documentation "REPORT, if true, specifies format to use, defaulting to plain text.")
  ;;(:method ((system system) (signature signature) (initial-tuple tuple))
  (:method ((system-name symbol) (signature t) (initial-data t) &rest keys)
    (apply #'solve (find-system system-name) signature initial-data keys))
  (:method ((system system) (signature null) (initial-data t) &rest keys &key report override-data)
    (let* ((initial-value (defaulted-initial-data system initial-data :override-data override-data))
	   (signature (or signature (make-signature (attributes initial-value) (attributes initial-value)))))
      (apply #'solve system signature initial-value keys)))
  (:method ((system system) (signature signature) (initial-data t) &key report override-data)
    (let ((initial-value (defaulted-initial-data system initial-data :override-data override-data))
	  (plan (plan system signature))
	  (report-results '()))
      (and plan
	   (satisfies-input-p initial-data signature)
	   ;; TODO: Finish refactoring this to be cleaner.
	   (let ((result (reduce (lambda (tuple-or-relation transformation)
				   (let ((transformed (apply-transformation transformation tuple-or-relation)))
				     (when report
				       (setf report-results
					     (append report-results (report-step transformed transformation system :format report))))
				     transformed))
				   plan
				   :initial-value initial-value)))
		 (values result plan (synthesize-report-steps report report-results) initial-value))))))

(defun report-step (step transformation system &key format)
  (let* ((tuples (ensure-tuples step))
	 (multiple (> (length tuples) 1)))
    (loop for tuple in tuples
       for i from 0
       append (create-tuple-report-step format tuple transformation system :n (and multiple i)))))

(defgeneric synthesize-report-steps (format steps)
  (:method ((format t) (steps list))
    (reduce (lambda (&optional acc step)
	      (concatenate 'string acc step))
	    steps
	    :initial-value "")))

(defgeneric create-tuple-report-step (format tuple transformation system &key n)  
  (:documentation "N is index of TUPLE if TUPLE has siblings.")
  (:method ((format t) (tuple tuple) (transformation transformation) (system system) &key n)
    (loop for (key value) in (tuple-pairs tuple)
       when (member key (signature-output (transformation-signature transformation)))
       collect (format nil "~&~@[~A. ~]~A: ~A = ~A~% ~A~%"
		       n
		       key
		       (describe-transformation-calculation transformation)
		       value
		       (let ((desc (and system (lookup-description key system))))
			 (if desc (format nil "   ~A~%" desc) ""))))))


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
    (report tuple (find-schema (system-schema system))))
  (:method ((tuple tuple) (system-name symbol))
    (when system-name
      (report tuple (find-system system-name)))))

(defun report-data (&optional (system *current-construction*))
  (mapcar (lambda (data) (report data system)) (system-data system)))

(defun solve-for (system output &optional initial-data &key report override-data project-solution)
  (let* ((system (find-system system))
	 (defaulted (defaulted-initial-data system initial-data :override-data override-data))
	 (sig (make-signature (attributes defaulted) output)))
    (multiple-value-bind (solution plan report defaulted-data)
	(solve system sig defaulted :report report :override-data override-data)
      (values (if project-solution
		  (project output solution)
		  solution)
	      plan report defaulted-data))))

(defun report-solution-for (output &key (system *current-construction*) initial-data (format t) override-data project-solution return-plan
				     return-defaulted-data)
  (multiple-value-bind (solution plan report defaulted-data) (solve-for system output initial-data
									:report format
									:override-data override-data
									:project-solution project-solution)
    (if return-plan
	(values (if solution report "NO SOLUTION") solution defaulted-data plan)
	(values (if solution report "NO SOLUTION") solution defaulted-data))))

(defun ask (system output &optional initial-data &key override-data)
  "Like solve-for but only returns the requested attributes in response tuple."
  (let ((solution (solve-for system output initial-data :override-data override-data)))
    (when solution
      (project output solution))))

(defun plan-for (system output &optional initial-data)
  (let* ((defaulted (defaulted-initial-data system initial-data))
	 (sig (make-signature (and defaulted (attributes defaulted)) output)))
    (plan system sig)))

(defun build-relation (from-pairs adding-attributes value-rows)
  (let ((tuples (loop for row in value-rows
		   collect (let ((base (make-tuple from-pairs)))
			     (loop for attr in adding-attributes
				for val in row
				do (setf (tref attr base) val))
			     base))))
    (make-relation tuples)))

(defun generate-directed-graph (plan)
  (loop for transformation in plan
       for signature = (transformation-signature transformation)
     append (loop for dependency in (signature-input signature)
	       append (loop for target in (signature-output signature)
			 collect (list dependency target)))))

(defun write-dot-format (directed-graph stream &key base-url)
  (flet ((make-label (symbol &key url target)
	   (let* ((symbol-name (symbol-name symbol))
		  (cleaned (substitute #\_ #\- symbol-name)))
	     (if url
		 (format nil "~A~%~A [LABEL = \"asdf\"; URL = \"~A#~A\"; TARGET = \"~A\" ] " cleaned cleaned base-url symbol-name target)
		 cleaned
		 ))))
    (format stream "digraph {~%")
    (loop for (dependency node) in directed-graph
       do (format stream "~A -> ~A ~%" (make-label dependency) (make-label node :url "xxx" :target "yyy")))
    (format stream "}~%")))

(defun dot-format (directed-graph stream &key base-url)
  (with-output-to-string (stream)
    (write-dot-format directed-graph stream :base-url base-url)))

(defun dot (dot-format &key format output-file (layout "dot"))
  (with-input-from-string (in dot-format)
    (uiop:run-program (format nil "dot -K~A -T ~A" layout format) :output output-file :input in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraints

(defun expand-constraint-definitions (constraint-definitions)
  `(list ,@(mapcar (lambda (constraint-form) (apply #'expand-constraint-definition constraint-form)) constraint-definitions)))

;;; TODO: Definer macro to simplify defining new constraints.

(deftype multiplication-constraint-form () '(cons (eql *)))
(deftype division-constraint-form () '(cons (eql /)))
(deftype addition-constraint-form () '(cons (eql +)))
(deftype subtraction-constraint-form () '(cons (eql -)))
(deftype log-constraint-form () '(cons (eql log)))
(deftype expt-constraint-form () '(cons (eql expt)))
(deftype integer-constraint-form () '(cons (eql integer)))
(deftype equality-constraint-form () '(cons (eql ==)))
(deftype disequality-constraint-form () '(cons (eql =/=)))
(deftype less-than-constraint-form () '(cons (eql <)))
(deftype greater-than-constraint-form () '(cons (eql >)))
(deftype less-than-or-equal-constraint-form () '(cons (eql <=)))
(deftype greater-than-or-equal-constraint-form () '(cons (eql >=)))

(deftype and-constraint-form () '(cons (eql and)))

(deftype constraint-form () '(or multiplication-constraint-form division-constraint-form addition-constraint-form subtraction-constraint-form
			      log-constraint-form expt-constraint-form integer equality-constraint-form disequality-constraint-form and-constraint-form
			      less-than-constraint-form greater-than-constraint-form
			      less-than-or-equal-constraint-form
			      greater-than-or-equal-constraint-form))

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
     (expand-subtraction-constraint name (first (cdr constraint-form)) (second (cdr constraint-form))))
    (log-constraint-form
     (expand-log-constraint name (first (cdr constraint-form)) (second (cdr constraint-form))))
    (expt-constraint-form
     (expand-expt-constraint name (first (cdr constraint-form)) (second (cdr constraint-form))))
    (integer-constraint-form
     (expand-integer-constraint name (first (cdr constraint-form))))
    (equality-constraint-form
     (expand-equality-constraint name (first (cdr constraint-form))))
    #+(or)
    (disequality-constraint-form
     (expand-disequality-constraint name (first (cdr constraint-form))))
    (and-constraint-form
     (expand-and-constraint name (first (cdr constraint-form)) (second (cdr constraint-form))))
    (less-than-constraint-form
     (expand-less-than-constraint-form name (first (cdr constraint-form)) (second (cdr constraint-form))))
    (less-than-or-equal-constraint-form
     (expand-less-than-or-equal-constraint-form name (first (cdr constraint-form)) (second (cdr constraint-form))))
    (greater-than-constraint-form
     (expand-greater-than-constraint-form name (first (cdr constraint-form)) (second (cdr constraint-form))))
    (greater-than-or-equal-constraint-form
     (expand-greater-than-or-equal-constraint-form name (first (cdr constraint-form)) (second (cdr constraint-form))))))

(defun expand-multiplication-constraint (product a b)
  "PRODUCT = A * B"
  `(component ((transformation ((,a ,b) -> (,product)) == (* ,a ,b))
	       (transformation ((,a ,product) -> (,b)) == (/ ,product ,a))
	       (transformation ((,b ,product) -> (,a)) == (/ ,product ,b)))))


(test multiplication-constraint
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

    ;; Inconsistent data are not produced.
    (is (null (solve-for system '() (tuple (a 3) (b 2) (c 5) (d 10)))))
    
    ;; This one can't (currently) be solved.
    (is (same nil 
	      (solve-for system '(a c) (tuple (b 2) (d 18)))))))

(defun expand-division-constraint (quotient dividend divisor)
  "DIVIDEND / DIVISOR = QUOTIENT => DIVIDEND = QUOTIENT * DIVISOR"
  (expand-multiplication-constraint dividend quotient divisor))

(test division-constraint
  "Test CONSTRAINT-SYSTEM with a division constraint."
  (let* ((system (constraint-system ((z (/ x y)))))
	 (satisfying-assignment (tuple (z 3) (x 12) (y 4))))
    (is (same satisfying-assignment
	      (solve-for system '(z) (tuple (x 12) (y 4)))))
    (is (same satisfying-assignment
	      (solve-for system '(x) (tuple (y 4) (z 3)))))
    (is (same satisfying-assignment
	      (solve-for system '(y) (tuple (x 12) (z 3)))))

    ;; Inconsistent data are not produced.
    (is (null (solve-for system '() (tuple (z 3) (x 12) (y 5)))))
    ))

(defun expand-addition-constraint (sum  a b)
  "SUM = A + B"
  `(component ((transformation ((,a ,b) -> (,sum)) == (+ ,a ,b))
	       (transformation ((,a ,sum) -> (,b)) == (- ,sum ,a))
	       (transformation ((,b ,sum) -> (,a)) == (- ,sum ,b)))))

(test addition-constraint
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

    ;; Inconsistent data are not produced.
    (is (null (solve-for system '() (tuple (a 3) (b 2) (c 5) (d 9)))))

    ;; This one can't (currently) be solved. TODO: Return relation of factorization tuples.
    (is (same nil 
	      (solve-for system '(a c) (tuple (b 2) (d 8)))))))

(defun expand-subtraction-constraint (difference minuend subtrahend)
  "MINUEND - SUBTRAHEND = DIFFERENCE => MINUEND = DIFFERENCE + SUBTRAHEND"
  (expand-addition-constraint MINUEND DIFFERENCE SUBTRAHEND))

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

(defun expand-log-constraint (log n base)
  "LOG = (LOG N BASE)"
  `(component ((transformation ((,n ,base) -> (,log)) == (log ,n ,base))
	       ;; TODO:
	       ;(transformation ((,n ,log) -> (,base)) == ;; need log-th root of n) 
	       (transformation ((,base ,log) -> (,n)) == (expt ,base ,log)))))

(test log-constraint
  "Test CONSTRAINT-SYSTEM with a log constraint."
  (let* ((system (constraint-system ((l (log n base)))))
	 (satisfying-assignment (tuple (base 2) (n 8.0) (l 3.0))))

    (is (same satisfying-assignment
	      (solve-for system '(l) (tuple (base 2) (n 8.0)))))
    (is (same satisfying-assignment
	      (solve-for system '(n) (tuple (base 2) (l 3.0)))))

    (is (not (same satisfying-assignment
		   (solve-for system '(l) (tuple (base 3) (n 8.0))))))

    ;; Inconsistent data are not produced.
    (is (null (solve-for system '() (tuple (base 3) (n 8.0) (l 5.0)))))
    ))

(defun expand-expt-constraint (pow base ex)
  "POW = (EXPT BASE EX)"
  `(component ((transformation ((,base ,ex) -> (,pow)) == (expt ,base ,ex))
	       ;(transformation ((,ex ,pow) -> (,base)) == ;; need ex-th root of pow)
	       (transformation ((,base ,pow) -> (,ex)) == (log ,pow ,base)))))


(defun must-integer (n)
  "Returns N if it is equivalent to an integer, otherwise NIL."
  (multiple-value-bind (div rem)
      (floor n)
    (and (zerop rem) div)))

(defun expand-integer-constraint (integer maybe-integer)
  "Returns a component which returns a relation binding INTEGER to MAYBE-INTEGER if it is equal to an integer, otherwise an empty relation."
  `(component ((transformation ((,maybe-integer) => (,integer)) == (awhen (must-integer ,maybe-integer)
								    `((,it))))
	       (transformation ((,integer) => (,maybe-integer)) == (progn (check-type ,integer integer)
									  `((,,integer)))))))

(defgeneric transformation-description (transformation)
  (:method ((transformation transformation))
    (let ((source (transformation-source transformation)))
      (typecase source
	((or constraint-form (and symbol (not null))) (format nil "~A" source))
	(null "<?>")
	(t nil (format nil "~A" source))))))

(test integer-constraint
  "Test CONSTRAINT-SYSTEM with an integer constraint."
  (let* ((system (constraint-system ((k (integer n))))))
    
    (is (same (relation (k n) (4 4.0))
     	      (solve-for system '(k) (tuple (n 4.0)))))
    (is (same (relation (k n) (4 4))
    	      (solve-for system '(k) (tuple (n 4)))))

    (is (same (relation (k n) (4 4))
    	      (solve-for system '(n) (tuple (k 4)))))
    (is (same (relation (k n) (4 4))
    	      (solve-for system '(n) (tuple (k 4)))))

    ;; It is a program error for the INTEGER input to the INTEGER constraint not to be an integer.
    (signals (type-error) (solve-for system '(n) (tuple (k 4.0))))

    ;; Inconsistent data are not produced. NOTE: Most such constraints yield NIL, but this explicitly produces an empty relation. Normalize?
    (is (same (relation (k n))
	      (solve-for system '() (tuple (k 3) (n 4.0)))))
    
    (is (same (relation (k n)) ;; Empty relation with expected heading.
    	      (solve-for system '(k) (tuple (n 4.1)))))
    ))


(defun expand-equality-constraint (a b)
  ;; TODO: in general, constraints must be able to function as restrictions -- but they are at least sometimes now short-circuited
  ;; by the planning process. (may be fixed -- verify)
  "Sets A to B or vice versa." 
  `(component ((transformation ((,a) -> (,b)) == ,a)
	       (transformation ((,b) -> (,a)) == ,b)
	       ;; This is no longer needed because apply-transformation eliminates inconsistent transformations *and* transformations
	       ;; are eagerly matched. 
	       #+(or)
	       (transformation ((,a ,b) => (,a ,b)) == (awhen (same ,a ,b)
							 `((,,a ,,b))))
	       )))

(test equality-constraint
  "Test CONSTRAINT-SYSTEM with an equality constraint."
  (let ((system (constraint-system ((a (== b)))))
	(satsifying-assignment (tuple (a 1) (b 1))))

    (is (same satsifying-assignment
	      (solve-for system '(a) (tuple (b 1)))))

    (is (same satsifying-assignment
	      (solve-for system '(b) (tuple (a 1)))))

    ;; Inconsistent data are not produced.
    (is (null (solve-for system '(a b) (tuple (a 1) (b 2)))))))


(defun  expand-less-than-constraint-form (result a b)
  "RESULT == A < B." 
  `(component ((transformation ((,a ,b) -> (,result)) == (< ,a ,b)))))

(test less-than-constraint
  "Test CONSTRAINT-SYSTEM with a LESS-THAN-CONSTRAINT."
  (let ((system (constraint-system ((x (< a b))))))
    (is (same (tuple (x t)) (ask system '(x) (tuple (a 1) (b 2)))))
    (is (same (tuple (x nil)) (ask system '(x) (tuple (a 2) (b 2)))))))

(defun expand-less-than-or-equal-constraint-form (result a b)
  "RESULT == A <= B" 
  `(component ((transformation ((,a ,b) -> (,result)) == (<= ,a ,b)))))

(test less-than-or-equal-constraint
  "Test CONSTRAINT-SYSTEM with a LESS-THAN-OR-EQUAL-CONSTRAINT."
  (let ((system (constraint-system ((x (<= a b))))))
    (is (same (tuple (x t)) (ask system '(x) (tuple (a 1) (b 2)))))
    (is (same (tuple (x t)) (ask system '(x) (tuple (a 2) (b 2)))))
    (is (same (tuple (x nil)) (ask system '(x) (tuple (a 3) (b 2)))))))

(defun expand-greater-than-constraint-form (result a b)
  "RESULT == A > B." 
  `(component ((transformation ((,a ,b) -> (,result)) == (> ,a ,b)))))

(test greater-than-constraint
  "Test CONSTRAINT-SYSTEM with a GREATER-THAN-CONSTRAINT."
  (let ((system (constraint-system ((x (> a b))))))
    (is (same (tuple (x t)) (ask system '(x) (tuple (a 3) (b 2)))))
    (is (same (tuple (x nil)) (ask system '(x) (tuple (a 2) (b 2)))))))

(defun expand-greater-than-or-equal-constraint-form (result a b)
  "RESULT == A >= B" 
  `(component ((transformation ((,a ,b) -> (,result)) == (>= ,a ,b)))))

(test greater-than-or-equal-constraint
  "Test CONSTRAINT-SYSTEM with a LESS-THAN-OR-EQUAL-CONSTRAINT."
  (let ((system (constraint-system ((x (>= a b))))))
    (is (same (tuple (x t)) (ask system '(x) (tuple (a 3) (b 2)))))
    (is (same (tuple (x t)) (ask system '(x) (tuple (a 2) (b 2)))))
    (is (same (tuple (x nil)) (ask system '(x) (tuple (a 1) (b 2)))))))


(defun expand-and-constraint (conjunction a b)
  "CONJUCTION = A && B"
  `(component ((transformation ((,a ,b) -> (,conjunction)) == (and ,a ,b))
	       (transformation ((,a ,conjunction) => (,b)) == (if ,a
								  `((,(and ,conjunction ,a)))
								  '((t) (nil))))
	       (transformation ((,b ,conjunction) => (,a)) == (if ,b
								  `((,(and ,conjunction ,b)))
								  '((t) (nil)))))))

(test and-constraint
  "Test CONSTRAINT-SYSTEM with two multiplication contraints."
  (let* ((system (constraint-system
		  ((c (and a b)))))
	 (sa-1 (tuple (a t) (b t) (c t)))
	 (sa-2 (tuple (a t) (b nil) (c nil)))
	 (sa-3 (tuple (a nil) (b nil) (c nil)))
	 (sa-4 (tuple (a nil) (b t) (c nil))))

    (is (same (rel sa-1) (solve-for system '(a) (tuple (b t) (c t)))))
    (is (same (rel sa-1) (solve-for system '(b) (tuple (a t) (c t)))))
    (is (same sa-1 (solve-for system '(c) (tuple (a t) (b t)))))

    (is (same (rel sa-2 sa-3) (solve-for system '(a) (tuple (b nil) (c nil)))))
    (is (same (rel sa-2) (solve-for system '(b) (tuple (a t) (c nil)))))
    (is (same sa-2 (solve-for system '(c) (tuple (a t) (b nil)))))

    (is (same (rel sa-2 sa-3) (solve-for system '(a) (tuple (b nil) (c nil)))))
    (is (same (rel sa-3 sa-4) (solve-for system '(b) (tuple (a nil) (c nil)))))
    (is (same sa-3 (solve-for system '(c) (tuple (a nil) (b nil)))))

    (is (same (rel sa-2 sa-3) (solve-for system '(a) (tuple (b nil) (c nil)))))
    (is (same (rel  sa-3 sa-4) (solve-for system '(b) (tuple (a nil) (c nil)))))
    (is (same sa-4 (solve-for system '(c) (tuple (a nil) (b t)))))
    
    ;; Inconsistent data are not produced.
    (is (null (solve-for system '() (tuple (a t) (b nil) (c t)))))))

(test constraint-constants
  "Test CONSTRAINT-SYSTEM with some constants."
  (let* ((system (constraint-system ((c (+ a 2)))))
	 (satisfying-assignment (tuple (a 3) (c 5))))

    (is (same satisfying-assignment
	      (solve-for system '(a) (tuple (c 5)))))
    (is (same satisfying-assignment
	      (solve-for system '(c) (tuple (a 3)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests / Examples

;; TODO: Split these up.
(test orient-tests
  "General tests planning and solving."
  (let* ((d1 (tuple (a 2) (b 3) (c 4)))
	 (d2 (tuple (a 2) (b 3) (c 4) (d 5)))
	 (d3 (tuple (x 5) (y 6) (z 7)))
	 ;(d4 (tuple (a 1) (b 2) (c 3)))

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

(test planning-terminates
  "Regression test for infinite stack bug."
  (let* ((t1 (transformation ((b c d) -> (e f)) == (let ((x (+ b c d)))
						     (values (* x b) (* x c)))))
	 (s1 (sys ((component (t1))))))

    (finishes (plan s1 (make-signature '(b c d) '(e))))))
