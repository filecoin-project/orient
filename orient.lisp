(defpackage :orient
  (:use "COMMON-LISP")
  (:export :apply-transform :component :data-map :deftransform :plan :same :sig :solve :sys :transform
	   :-> :=== :==))
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

(defmethod data-map-pairs ((data-map data-map))
  (loop
     for key being the hash-keys of (data-map-hash-table data-map)
     for val being the hash-values of (data-map-hash-table data-map)
     collect (list key val)))

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
  ((inputs :initarg :inputs :initform '() :accessor signature-inputs)
   (outputs :initarg :outputs :initform '() :accessor signature-outputs)))

(defun make-signature (inputs outputs)
  (make-instance 'signature :inputs inputs :outputs outputs))

(defun pruned-signature (sig)
  "Return a new signature, with outputs which are also inputs pruned, since these will be trivially provided."
  (let* ((inputs (signature-inputs sig))
	 (pruned-outputs (set-difference (signature-outputs sig) inputs)))
    (if pruned-outputs
	(make-signature inputs pruned-outputs)
	sig)))

(defmethod print-object ((sig signature) (stream t))
  (format stream "(SIG ~S -> ~S)" (signature-inputs sig) (signature-outputs sig)))

(defmethod sig-subset-p ((s signature) (other signature))
  "Returns true if s is a subset of other."
  (and (subsetp (signature-inputs s) (signature-inputs other))
       (subsetp (signature-outputs s) (signature-outputs other))))

(defun sig-equal (a b) (and (sig-subset-p a b) (sig-subset-p b a)))

(defmethod provides-p ((s signature) (name symbol))
  "Returns true if name is an output of signature."
  (member name (signature-outputs s)))

(defmethod provides ((outputs list) (s signature))
  (intersection (signature-outputs s) outputs))

(defclass transform ()
  ((signature :initarg :signature :initform (make-signature '() '()) :accessor transform-signature)
   (implementation :initarg :implementation :initform nil :accessor transform-implementation)))

(defmethod print-object ((trans transform) (stream t))
  (format stream "(TRANSFORM ~S === ~S)" (transform-signature trans) (transform-implementation trans)))

(defun identity-transform () (make-instance 'transform))

(defclass component ()
  ((transforms :initarg :transforms :initform '() :accessor component-transforms)))

(defmethod print-object ((comp component) (stream t))
  (format stream "(COMPONENT ~S)" (component-transforms comp)))


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
  (:method ((a transform) (b transform))
    (and (same (transform-signature a) (transform-signature b))
	 (equal (transform-implementation a) (transform-implementation b))))
  (:method ((a component) (b component))
    ;; FIXME: use set-equal
    (and (subsetp (component-transforms a) (component-transforms b) :test #'same)
	 (subsetp (component-transforms b) (component-transforms a) :test #'same)))
  (:method ((a relation) (b relation))
    (set-equal (data-maps a) (data-maps b) :test #'same))
  (:method ((a list) (b list))
    (and (eql (length a) (length b))
	 (every #'same a b))))

(defgeneric satisfies-inputs-p (a b)
  (:method ((a t) (b t)) nil)
  (:method ((a t) (b transform)) (satisfies-inputs-p a (transform-signature b)))
  (:method ((a t) (b signature)) (subsetp (signature-inputs b) (domain a))) ;; FIXME: new superclass of types with domain.
  )

(defgeneric apply-transform (transform data-map)
  (:method ((transform transform) (data-map data-map))
    (assert (satisfies-inputs-p data-map transform))
    (apply-transform (transform-implementation transform) data-map))
  (:method ((transform transform) (relation simple-relation))
      (make-relation (mapcar (lambda (x) (apply-transform transform x))
			     (data-maps relation))))
  (:method ((f function) (data-map data-map))
    (funcall f data-map))
  (:method ((s symbol) (data-map data-map))
    (funcall s data-map)))

(defclass plan-profile () ((transforms-tried :initform 0 :accessor transforms-tried)))

(defmethod print-object ((p plan-profile) (stream t))
  (format stream "<PLAN-PROFILE; transforms-tried: ~d>" (transforms-tried p)))

(defvar *plan-profile*)

(defgeneric %plan (system element signature plan)
  (:method ((system system) (transform transform) (signature signature) (plan list))
    (incf (transforms-tried *plan-profile*))
    (let* ((sig (transform-signature transform))
	   ;; Which of the still-needed outputs, if any, does the this transform's signature provide?
	   (provided-outputs (provides (signature-outputs signature) sig)))     
      (unless provided-outputs
	;; If this transform doesn't provide any needed outputs, fail early.
	(return-from %plan nil))

      ;; Otherwise, add the transform to the plan and update the signature to satisfy.
      (let* ((new-plan (cons transform plan))
	     ;; Inputs of the current transform which aren't trivially provided must now be outputs of the
	     ;; remaining plan (to be provided before this step's transform is applied).
	     (additional-outputs (set-difference (signature-inputs sig) (signature-inputs signature)))
	     ;; Outputs which still need to be provided.
	     (remaining-outputs (union (set-difference (signature-outputs sig) provided-outputs) additional-outputs)))
	(if remaining-outputs
	    ;; If there are still outputs which need to be satisfied, continue planning the system.
	    (%plan  system :system (make-signature (signature-inputs signature) remaining-outputs) new-plan)
	    ;; Otherwise, return the new plan.
	    new-plan))))
  (:method ((system system) (component component) (signature signature) (plan list))
    (mapcan (lambda (transform)
	      (%plan system transform signature plan))
	    (component-transforms component)))
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
	   (satisfies-inputs-p initial-data signature)
	   (reduce (lambda (data-map transform)
		     (apply-transform transform data-map))
		   plan
		   :initial-value initial-data)))))

;;; Syntax
(defmacro sig ((&rest inputs) arrow (&rest outputs))
  (assert (eql arrow '->))
  `(make-signature ',inputs ',outputs))

(defmacro transform ((&rest inputs) arrow (&rest outputs) eqmark implementation)
  (assert (eql arrow '->))
  (ecase eqmark
    (=== `(let ((sig (make-signature ',inputs ',outputs)))
	     (make-instance 'transform :signature sig :implementation ,implementation)))
    (== `(let ((sig (make-signature ',inputs ',outputs)))
	    (make-instance 'transform :signature sig :implementation (tlambda ,inputs ,outputs
								       ,implementation))))))

(defmacro deftransform (name ((&rest inputs) arrow (&rest outputs)) &body implementation)  
  (assert (eql arrow '->))
  `(eval-when (:load-toplevel :execute)
     (progn (defparameter ,name (transform (,@inputs) -> (,@outputs) == (progn ,@implementation))))))

(defmacro component (transforms)
  `(make-instance 'component :transforms (list ,@transforms)))

(defmacro defcomponent (name (&rest transforms))
  `(defparameter ,name (make-instance 'component :transforms (list ,@transforms))))


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
  (component ((transform (a b c) -> (d) === (tlambda (a b c) (d)
					      (values (* a b c))))
	      (transform (x y z) -> (q) == (values (+ x y z)))	    
	      (transform (b c d) -> (e f) == (let ((x (+ b c d)))
					       (values (* x b) (* x c)))))))

(deftransform t1 ((a b c) -> (d)) (values (* a b c)))
(deftransform t2 ((x y z) -> (q)) (values (+ x y z)))
(deftransform t3 ((b c d) -> (e f))
  (let ((x (+ b c d)))
    (values (* x b) (* x c))))

(defcomponent c1 (t1))
(defcomponent c2 (t1 t2 t3))

(defparameter s1 (sys (c1)))
(defparameter s2 (sys (c2)))

(assert (same (apply-transform t2 d3) (data-map '((x 5)(y 6)(z 7)(q 18)))))

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
(Plan s1 sig1) => (((SIG (A B C) -> (D)) . (TRANSFORM (SIG (A B C) -> (D)) === ASDF)))
(plan s1 sig2) => nil
(plan s1 sig3) => nil

(plan s2 sig1) => (((TRANSFORM (SIG (A B C) -> (D)) === ASDF)))                          ; *transforms-tried* 3

(plan s2 sig2) => (((TRANSFORM (SIG (B C D) -> (E F)) === FDSA))                         ; *transforms-tried* 3

(plan s2 sig3) => ((TRANSFORM (SIG (A B C) -> (D)) === ASDF)
		   (TRANSFORM (SIG (B C D) -> (E F)) === FDSA))                          ; *transforms-tried* 6
|#
