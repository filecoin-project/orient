(defpackage :orient
  (:use "COMMON-LISP")
  (:export :apply-transformation :component :data-map :data-map-pairs :deftransformation :make-signature :plan :same :sig :signature
	   :signature-input :signature-output :solve :sys :system :transformation :-> :=== :==))

(in-package "ORIENT")

(defclass data-map ()
  ((hash-table :initform (make-hash-table) :accessor data-map-hash-table)))

(defmethod print-object ((d data-map) (stream t))
  (format stream "<DATA-MAP ~S>" (data-map-pairs d)))

(defun data-map (&optional pairs)
  (let* ((data-map (make-instance 'data-map))
	 (h (data-map-hash-table data-map)))
    (loop for (k v) in pairs do (setf (gethash k h) v))
    data-map))

(defmethod data-map-pairs ((data-map data-map) &key dotted)
  (loop
     for key being the hash-keys of (data-map-hash-table data-map)
     for val being the hash-values of (data-map-hash-table data-map)
     collect (if dotted
		 (cons key val)
		 (list key val))))

(defmethod getd ((key t) (data-map data-map))
  "Get value of KEY in DATA-MAP."
  (gethash key (data-map-hash-table data-map)))

(defmethod setd ((key t) (data-map data-map) (value t))
  "Set value of KEY to VALUE in DATA-MAP."
  (setf (gethash key (data-map-hash-table data-map)) value))

(defsetf getd setd)

(defclass relation () ())
(defclass simple-relation (relation)
  ((data-maps :initarg :data-maps :initform nil :accessor data-maps)))

(defgeneric domain (data-map)
  (:method ((d data-map))
    (loop for key being the hash-keys of (data-map-hash-table d)
       collect key))
  (:method ((r relation))
    (and (first (data-maps r))
	 (domain (first (data-maps r))))))

(defun set-equal (a b &key (test #'eql)) (and (subsetp  a b :test test) (subsetp b a :test test)))

(defgeneric make-relation (data-maps)
  (:documentation
  "Create relation from data-maps, removing duplicates. Returns NIL if data-maps don't have all have same domain. ")
  (:method ((data-maps list))
    (and(let ((first (first data-maps)))
	  (every (lambda (x) (set-equal (domain x) (domain first)))
		 (cdr data-maps)))
	(make-instance 'simple-relation :data-maps (remove-duplicates data-maps :test #'same)))))

(defclass parameter ()
  ((name :initarg :name :initform (error "name missing") :accessor parameter-name)
   (description :initarg :description :accessor parameter-description)
   (type :initarg :type :initform nil :accessor parameter-type)))

(defclass schema ()
  ((parameters :initarg parameters :initform '() :accessor schema-parameters)))

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
   (components :initarg :components :initform '() :accessor system-components)))

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
  (:method ((a data-map) (b data-map))
    (equalp (data-map-hash-table a) (data-map-hash-table b)))
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
    (set-equal (data-maps a) (data-maps b) :test #'same))
  (:method ((a list) (b list))
    (and (eql (length a) (length b))
	 (every #'same a b))))

(defgeneric satisfies-input-p (a b)
  ;; FIXME: Make type of A ensure DOMAIN.
  (:method ((a t) (b t)) nil)
  (:method ((a t) (b transformation)) (satisfies-input-p a (transformation-signature b)))
  (:method ((a t) (b signature)) (subsetp (signature-input b) (domain a))) ;; FIXME: new superclass of types with domain.
  )

(defgeneric apply-transformation (transformation data-map)
  (:method ((transformation transformation) (data-map data-map))
    (assert (satisfies-input-p data-map transformation))
    (apply-transformation (transformation-implementation transformation) data-map))
  (:method ((transformation transformation) (relation simple-relation))
      (make-relation (mapcar (lambda (x) (apply-transformation transformation x))
			     (data-maps relation))))
  (:method ((list list) (data-map data-map))
    (reduce (lambda (data-map transformation)
	      (apply-transformation transformation data-map))
	    list
	    :initial-value data-map))
  (:method ((f function) (data-map data-map))
    (funcall f data-map))
  (:method ((s symbol) (data-map data-map))
    (funcall s data-map)))

(defgeneric compose-signatures (a b)
  ;; FIXME: Make type of DATA-MAP ensure signature.
  (:method ((signature signature) (data-map data-map))
    (let ((domain (domain data-map)))
      (make-signature (union (signature-input signature) domain)
		      (union (signature-output signature) domain))))
  (:method ((a signature) (b signature))
    (make-signature (union (signature-input a) (signature-input b))
		    (union (signature-output a) (signature-output b)))))

(defclass plan-profile () ((transformations-tried :initform 0 :accessor transformations-tried)))

(defmethod print-object ((p plan-profile) (stream t))
  (format stream "<PLAN-PROFILE; transformations-tried: ~d>" (transformations-tried p)))

(defvar *plan-profile*)

(defgeneric %plan (system element signature plan)
  (:method ((system system) (transformation transformation) (signature signature) (plan list))
    (incf (transformations-tried *plan-profile*))
    (let* ((sig (transformation-signature transformation))
	   ;; Which of the still-needed output, if any, does the this transformation's signature provide?
	   (provided-output (provides (signature-output signature) sig)))     
      (unless provided-output
	;; If this transformation doesn't provide any needed output, fail early.
	(return-from %plan nil))

      ;; Otherwise, add the transformation to the plan and update the signature to satisfy.
      (let* ((new-plan (cons transformation plan))
	     ;; Input of the current transformation which aren't trivially provided must now be output of the
	     ;; remaining plan (to be provided before this step's transformation is applied).
	     (additional-output (set-difference (signature-input sig) (signature-input signature)))
	     ;; Output which still need to be provided.
	     (remaining-output (union (set-difference (signature-output sig) provided-output) additional-output)))
	(if remaining-output
	    ;; If there are still output which need to be satisfied, continue planning the system.
	    (%plan  system :system (make-signature (signature-input signature) remaining-output) new-plan)
	    ;; Otherwise, return the new plan.
	    new-plan))))
  (:method ((system system) (component component) (signature signature) (plan list))
    (mapcan (lambda (transformation)
	      (%plan system transformation signature plan))
	    (component-transformations component)))
  (:method ((system system) (start (eql :system)) (signature signature) (plan list))
    (mapcan (lambda (component)
	      (%plan system component signature plan))
	    (system-components system))))

(defgeneric plan (system signature)
  (:method ((system system) (signature signature))
    (let ((*plan-profile* (make-instance 'plan-profile)))
      (values (%plan system :system (pruned-signature signature) '())
	      *plan-profile*))))

(defgeneric solve (system signature initial-data)
  ;;(:method ((system system) (signature signature) (initial-data-map data-map))
  (:method ((system system) (signature signature) (initial-data t)) ;; FIXME: create and use common supertype for data-map and relation.
    (let ((plan (plan system signature)))
      (and plan
	   (satisfies-input-p initial-data signature)
	   (reduce (lambda (data-map transformation)
		     (apply-transformation transformation data-map))
		   plan
		   :initial-value initial-data)))))

;;; Syntax
(defmacro sig ((&rest input) arrow (&rest output))
  (assert (eql arrow '->))
  `(make-signature ',input ',output))

(defmacro transformation ((&rest input) arrow (&rest output) eqmark implementation)
  (assert (eql arrow '->))
  (ecase eqmark
    (=== `(let ((sig (make-signature ',input ',output)))
	     (make-instance 'transformation :signature sig :implementation ,implementation)))
    (== `(let ((sig (make-signature ',input ',output)))
	    (make-instance 'transformation :signature sig :implementation (tlambda ,input ,output
								       ,implementation))))))

(defmacro deftransformation (name ((&rest input) arrow (&rest output)) &body implementation)  
  (assert (eql arrow '->))
  `(eval-when (:load-toplevel :execute)
     (progn (defparameter ,name (transformation (,@input) -> (,@output) == (progn ,@implementation))))))

(defmacro component (transformations)
  `(make-instance 'component :transformations (list ,@transformations)))

(defmacro defcomponent (name (&rest transformations))
  `(defparameter ,name (make-instance 'component :transformations (list ,@transformations))))


;; Ignore schema for now.
(defmacro sys ((&rest components))
  `(make-instance 'system :components (list ,@components)))

(defmacro tlambda ((&rest input) (&rest output) &body body)
  (let ((data-map (gensym "DATA-MAP"))
	(new-data-map (gensym "NEW-DATA-MAP"))
	(out (gensym "OUTPUT")))
    `(lambda (,data-map)
       (symbol-macrolet
	   (,@(loop for in in input
		collect `(,in (getd ',in ,data-map))))
	 (let ((,new-data-map (data-map (data-map-pairs ,data-map)))
	       (,out (multiple-value-list (progn ,@body))))	   
	   ,@(loop for key in output
		collect `(setf (getd ',key ,new-data-map) (pop ,out)))
	   ,new-data-map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests / Examples
;; (defpackage orient-test (:use "COMMON-LISP" "ORIENT"))
;; (in-package orient-test)

(defparameter d1 (data-map '((a 2) (b 3) (c 4))))
(defparameter d2 (data-map '((a 2) (b 3) (c 4) (d 5))))
(defparameter d3 (data-map '((x 5) (y 6) (z 7))))

(defparameter r1 (make-relation (list d1)))
(defparameter r2 (make-relation (list d2)))
(defparameter r3 (make-relation (list d3)))

(defparameter sig1 (sig (a b c) -> (d)))
(defparameter sig2 (sig (b c d) -> (e f)))
(defparameter sig3 (sig (a b c) -> (e f)))

(progn ;; Example usages
  (component ((transformation (a b c) -> (d) === (tlambda (a b c) (d)
					      (values (* a b c))))
	      (transformation (x y z) -> (q) == (values (+ x y z)))	    
	      (transformation (b c d) -> (e f) == (let ((x (+ b c d)))
					       (values (* x b) (* x c)))))))

(deftransformation t1 ((a b c) -> (d)) (values (* a b c)))
(deftransformation t2 ((x y z) -> (q)) (values (+ x y z)))
(deftransformation t3 ((b c d) -> (e f))
  (let ((x (+ b c d)))
    (values (* x b) (* x c))))

(defcomponent c1 (t1))
(defcomponent c2 (t1 t2 t3))

(defparameter s1 (sys (c1)))
(defparameter s2 (sys (c2)))

(assert (same (apply-transformation t2 d3) (data-map '((x 5)(y 6)(z 7)(q 18)))))

(assert (same (plan s1 sig1) (list t1)))
(assert (same (plan s1 sig2) nil)) 
(assert (same (plan s1 sig3) nil))

(assert (same (plan s2 sig1) (list t1)))
(assert (same (plan s2 sig2) (list t3)))
(assert (same (plan s2 sig3) (list t1 t3)))

(assert (same (solve s1 sig1 d1) (data-map '((a 2)(b 3)(c 4)(d 24)))))
(assert (same (solve s1 sig2 d1) nil))
(assert (same (solve s1 sig3 d1) nil))

(assert (same (solve s2 sig1 d1) (data-map '((a 2)(b 3)(c 4)(d 24)))))
(assert (same (solve s2 sig1 r1) (make-relation (list (data-map '((a 2)(b 3)(c 4)(d 24)))))))
(assert (same (solve s2 sig2 d1) nil))
(assert (same (solve s2 sig2 d2) (data-map '((a 2)(b 3)(c 4)(d 5)(e 36)(f 48)))))
(assert (same (solve s2 sig3 d1) (data-map '((a 2)(b 3)(c 4)(d 24)(e 93)(f 124)))))


#|
(plan s1 sig1) => (((SIG (A B C) -> (D)) . (TRANSFORMATION (SIG (A B C) -> (D)) === ASDF)))
(plan s1 sig2) => nil
(plan s1 sig3) => nil

(plan s2 sig1) => (((TRANSFORMATION (SIG (A B C) -> (D)) === ASDF)))                          ; *transformations-tried* 3

(plan s2 sig2) => (((TRANSFORMATION (SIG (B C D) -> (E F)) === FDSA))                         ; *transformations-tried* 3

(plan s2 sig3) => ((TRANSFORMATION (SIG (A B C) -> (D)) === ASDF)
		   (TRANSFORMATION (SIG (B C D) -> (E F)) === FDSA))                          ; *transformations-tried* 6
|#

