(in-package "ORIENT")
(def-suite orient-suite :description "Test the orient package.")
(in-suite orient-suite)

(deftype tuple () 'fset:wb-map)

(defun make-tuple (&optional pairs dotted)
  (convert 'wb-map pairs :value-fn (if dotted #'cdr #'cadr)))

(defmacro tref (attribute tuple)
  "Get value of ATTRIBUTE in TUPLE."
  `(@ ,tuple ,attribute))

(defclass relation () ())

(defgeneric attributes (tuple)
  (:method ((null null)) nil)
  (:method ((d wb-map)) (domain d))
  (:method ((r relation))
    (awhen (arb (tuples r))
	 (attributes it))))

;; TODO: Make TUPLES accept an optional conversion type to simplifying getting list for iteration.
(defclass simple-relation (relation)
  ((tuples :initarg :tuples :initform nil :accessor tuples :type set)))

(defgeneric ensure-tuples (thing)
  (:method ((tuple wb-map))
    (list tuple))
  (:method ((relation relation))
    (tuples relation)))

(defmethod print-object ((relation relation) (stream t))
  (format stream "<RELATION ~S ~S>" (sort (attributes relation) #'string<) (tuples relation)))

(defgeneric ensure-tuples (attributed)
  (:method ((tuple wb-map))
    (set tuple))
  (:method ((relation relation))
    (tuples relation))
  (:method ((null null)) nil))

(defgeneric make-relation (tuples)
  (:documentation
   "Create relation from tuples, removing duplicates. Returns NIL if tuples don't have all have same attributes.")
  ;; Rather than return NIL, should mismatch be an error?
  (:method ((tuples list)) (make-relation (convert 'set tuples)))
  (:method ((tuples set))
    (let ((attributes (awhen (arb tuples) (attributes it))))
      (and (every (lambda (tuple) (equal? (attributes tuple) attributes)) tuples)
	   (make-instance 'simple-relation :tuples tuples)))))

(defgeneric cardinality (relation)
  (:method ((r relation))
    (size (tuples r))))

(defgeneric degree (attributed)
  (:method ((tuple wb-map))
    (size tuple))
  (:method ((r relation))
    (size (attributes r))))

(defgeneric %join (relation-a relation-b)
  (:documentation "Helper function so JOIN can avoid expensive creation of relations which will be immediately stripped for their contained tuples.")
  (:method ((a wb-map) (b wb-map))
    (let* ((a-attributes (attributes a))
	   (b-attributes (attributes b))
	   (shared (intersection a-attributes b-attributes))
	   (all-matchp (every (lambda (attr)
				(same (tref attr a) (tref attr b)))
			      shared)))
      (when all-matchp (map-union a b))))
  (:method ((a wb-map) (b relation))
    (less (gmap:gmap :set
		     (lambda (x) (%join x a))
		     (:set (tuples b)))
	  nil))
  (:method ((a relation) (b wb-map))
    (%join b a))
  (:method ((a relation) (b relation))
    (reduce (lambda (acc tuple)
	      (union acc (%join tuple b)))
	    (tuples a)
	    :initial-value (empty-set))))

(defgeneric join- (a b)
  (:documentation "Binary JOIN. Returns a relation, a single tuple, or NIL.")
  (:method ((a wb-map) (b wb-map))
    (%join a b))
  (:method ((a wb-map) (b relation))
    (make-relation (%join a b)))
  (:method ((a relation) (b wb-map))
    (join- b a))
  (:method ((a relation) (b relation))
    (make-relation (%join a b))))

(defun join (&rest things) (reduce #'join- things))

(defgeneric rename-attributes (old-new-pairs attributed)
  (:method ((pairs list) (r relation))
    (make-relation (image (lambda (tuple) (rename-attributes pairs tuple))
			  (tuples r))))
  (:method ((pairs list) (tuple wb-map))
    (reduce (lambda (acc pair)
	      (destructuring-bind (old new) pair
		(with (less acc old) new (@ acc old))))
	    pairs
	    :initial-value tuple)))

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
    (make-relation (filter tpred (tuples relation)))))

(test restrict "Test RESTRICT."
      (is (same (relation (a b c) (4 5 6))
		(restrict (tfn (b) (= b 5))
			  (relation (a b c) (1 2 3) (4 5 6) (7 8 9))))))

(defgeneric project (attributes attributed)
  (:method ((attributes list) (attributed t))
    (project (convert 'set attributes) attributed))
  (:method ((attributes set) (null null))
    nil)
  (:method ((attributes set) (tuple wb-map))
    (fset:restrict tuple attributes))
  (:method ((attributes set) (relation relation))
    (make-relation
     (image (lambda (tuple) (project attributes tuple))
	    (tuples relation)))))

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
	 (arb (tuples relation)))))

(defclass parameter ()
  ((name :initarg :name :initform (error "name missing") :accessor parameter-name)
   (description :initarg :description :accessor parameter-description)
   (type :initarg :type :initform nil :accessor parameter-type)))

(defclass signature ()
  ((input :initarg :input :initform (empty-set) :accessor signature-input :type set)
   (output :initarg :output :initform (empty-set) :accessor signature-output :type set)))

(defun make-signature (input output)
  (make-instance 'signature :input (convert 'set input) :output (convert 'set output)))

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

(defclass problem ()
  ((signature :initarg :signature :initform (make-signature '() '()) :accessor problem-signature)))

(defgeneric all-system-components (system)
  (:method ((system system))
    (reduce #'append (cons (system-components system)
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
  (format stream "<SYSTEM ~S)" (list :components (system-components sys)
				     :subsystems (system-subsystems sys)
				     :schema (system-schema sys)
				     :data (system-data sys))))

(defgeneric lookup- (attribute schemable)
  (:method ((attribute symbol) (schema schema))
    (find attribute (schema-parameters schema) :key #'parameter-name))
  (:method ((attribute symbol) (system system))
    (or (awhen (find-schema (system-schema system))
	  (lookup- attribute it))
	(some (lambda (system) (lookup- attribute system))
	      (system-subsystems system)))))

(defun lookup-description (attribute schemable)
  (let ((parameter (lookup- attribute schemable)))
    (and parameter (parameter-description parameter))))

(defgeneric implementation-function (implementation)
  (:method ((impl implementation))
    (awhen (find-symbol (implementation-name impl) (implementation-module impl))
      (symbol-function it))))

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
  (:method ((a relation) (b relation))
    (equal? (tuples a) (tuples b)))
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

(defgeneric satisfies-input-p (attributed b)
  (:documentation "True if all inputs to B are attributes of ATTRIBUTED.")
  ;; FIXME: Make type of A ensure ATTRIBUTES.
  (:method ((a t) (b t)) nil)
  (:method ((a t) (b transformation)) (satisfies-input-p a (transformation-signature b)))
  (:method ((a t) (b signature)) (subset? (signature-input b) (attributes a))) ;; FIXME: new superclass of types with attributes.
  )

(defgeneric ensure-relation (potential-relation)
  (:method ((r relation)) r)
  (:method ((tuple wb-map))
    (make-relation (set tuple)))
  (:method ((list list))
    (check-type list (cons tuple)) ;; Not exhaustive, but a good sanity check.
    (make-relation list))
  (:method ((set set))
    (let ((tuple (arb set)))
      (check-type tuple tuple) ;; Not exhaustive, but a good sanity check.
      (make-relation set)))
  )

(defgeneric combine-potential-relations (a b)
  (:method ((a list) (b t))
    (combine-potential-relations (convert 'set a) b))
  (:method ((a t) (b list))
    (combine-potential-relations a (convert 'set b)))
  (:method ((a relation) (b relation))
    ;; FIXME: Check that headings are compatible.
    (make-relation (union (tuples a) (tuples b))))
  (:method ((a wb-map) (b wb-map))
    (make-relation (set a b)))
  (:method ((a wb-map) (b set))
    (make-relation (with b a)))
  (:method ((a list) (b wb-map))
    (combine-potential-relations b a))
  (:method ((a set) (b set))
    ;; We assume aruguments to COMBINE-POTENTIAL-RELATIONS can be destructively modified.
    (make-relation (union a b)))
  (:method ((a wb-map) (b relation))
    ;; FIXME: Check that headings are compatible.
    (make-relation (cons a (tuples b))))
  (:method ((a relation) (b wb-map))
    (combine-potential-relations b a))
  (:method ((a set) (b relation))
    (make-relation (union a (tuples b))))
  (:method ((a relation) (b set))
    (combine-potential-relations b a)))

(deftype transformation-spec () '(or transformation symbol))

;; TODO: Transformation should fail if any input is removed from output.
(defgeneric apply-transformation (transformation tuple)
  (:method ((f function) (tuple wb-map))
    ;;; This is where the real work happens.
    ;;; All transformation applications need to go through here.
    
    (let* ((result (funcall f tuple)))
      (join tuple result)))
  (:method ((impl implementation) (tuple t))
    (awhen (implementation-function impl)
      (apply-transformation it tuple)))
  (:method ((transformation-name symbol) (tuple t))
    (awhen (find-transformation transformation-name) (apply-transformation it tuple)))
  (:method ((transformation t) (null null))
    ;; Everything collapses to NIL. This simplifies uniform handling of tuples/relations, but may need to be revisited.
    nil)
  (:method ((transformation transformation) (tuple wb-map))
    (assert (satisfies-input-p tuple transformation))
    (apply-transformation (transformation-implementation transformation) tuple))
  (:method ((transformation t) (relation simple-relation))
    ;; TODO: Can we use GMAP here?
    (reduce #'combine-potential-relations
	    (image (lambda (tuple)
		      (apply-transformation transformation tuple))
		   (tuples relation))
	    :initial-value nil))
  (:method ((list list) (tuple wb-map))
    (check-type list (cons transformation-spec))
    (reduce (lambda (tuple transformation)
	      (apply-transformation transformation tuple))
	    list
	    :initial-value tuple)))

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

      (debug-plan :signature-output (signature-output signature))
      ;; TODO: prune extraneous transformations from the plan.
      (let ((needed-output (set-difference (signature-output signature) (signature-input signature))))
	(and plan-signature (subset? needed-output (signature-output plan-signature))
	     plan))))
  (:method ((selector (eql :component-list)) (component-list list) (signature signature) (plan list))
    (let* ((candidates (remove-if-not (lambda (c) (signature-satisfies-p signature c)) component-list)

	     ))
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
  (let ((defaulted (or provided
		       (and (all-system-data system)
			    (apply #'join (all-system-data system))))))
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
    (let ((initial-value (defaulted-initial-data system initial-data :override-data override-data))
	  (plan (plan system signature)))
      (and plan
	   (satisfies-input-p initial-data signature)
	   (let* ((reducer (make-pipeline-reducer system :report report))
		  (reduce-result (reduce reducer plan :initial-value (list initial-value '()))))
	     (destructuring-bind (result report-results) reduce-result
	       (values result plan (synthesize-report-steps report report-results) initial-value)))))))

(defun make-pipeline-reducer (system &key report)
  (lambda (acc transformation)
    (destructuring-bind (tuple-or-relation report-results) acc
      (let* ((transformed (apply-transformation transformation tuple-or-relation))
	     (new-report (when report
			   (append report-results (report-step transformed transformation system :format report)))))
	(list transformed new-report)))))

(defun report-step (step transformation system &key format)
  (let* ((tuples (ensure-tuples step))
	 (multiple (> (size tuples) 1)))
    (reduce #'append (gmap:gmap :list
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

(defun solve-for (system output &optional initial-data &key report override-data project-solution)
  "Returns four values: solution, plan, report, and defaulted-data."
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

(defun build-relation (from-tuple adding-attributes value-rows)
  (let ((tuples (loop for row in value-rows
		   collect (let ((base from-tuple))
			     (loop for attr in adding-attributes
				for val in row
				do (adjoinf base attr val))
			     base))))
    (make-relation tuples)))

(defun generate-directed-graph (plan)
  (loop for transformation in plan
       for signature = (transformation-signature transformation)
     append (loop for dependency in (convert 'list (signature-input signature))
	       append (loop for target in (convert 'list (signature-output signature))
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

(defun dot-format (directed-graph &key base-url)
  (with-output-to-string (out)
    (write-dot-format directed-graph out :base-url base-url)))

(defun dot (dot-format &key format output-file (layout "dot"))
  (with-input-from-string (in dot-format)
    (uiop:run-program (format nil "dot -K~A -T ~A" layout format) :output output-file :input in)))

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

    (finishes (plan s1 (make-signature '(b c d) '(e)))))	)
