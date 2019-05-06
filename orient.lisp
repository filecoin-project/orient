(defpackage :orient (:use "COMMON-LISP"))
(in-package "ORIENT")

(defclass data-map ()
  ((hash-table :initform (make-hash-table) :accessor data-map-hash-table)))

(defun make-data-map (&optional pairs)
  (let* ((data-map (make-instance 'data-map))
	 (h (data-map-hash-table data-map)))
    (loop for (k . v) in pairs do (setf (gethash k h) v))
    data-map))

(defmethod data-map-pairs ((data-map data-map))
  (loop
     for key being the hash-keys of (data-map-hash-table data-map)
     for val being the hash-values of (data-map-hash-table data-map)
     collect (cons key val)))  

(defmethod getd ((key t) (data-map data-map))
  "Get value of KEY in DATA-MAP."
  (gethash key (data-map-hash-table data-map)))

(defmethod setd ((key t) (data-map data-map) (value t))
  "Set value of KEY to VALUE in DATA-MAP."
  (setf (gethash key (data-map-hash-table data-map)) value))

(defsetf getd setd)

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
  (format stream "(SIG ~A -> ~A)" (signature-inputs sig) (signature-outputs sig)))

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
  (format stream "(TRANSFORM ~A === ~A)" (transform-signature trans) (transform-implementation trans)))

(defun identity-transform () (make-instance 'transform))

(defclass component ()
  ((transforms :initarg :transforms :initform '() :accessor component-transforms)))

(defmethod print-object ((comp component) (stream t))
  (format stream "(COMPONENT ~A)" (component-transforms comp)))


(defclass problem ()
  ((signature :initarg :signature :initform (make-signature '() '()) :accessor problem-signature)))

(defclass system ()
  ((schema :initarg :schema :initform nil :accessor system-schema)
   (components :initarg :components :initform '() :accessor system-components)))

(defmethod print-object ((sys system) (stream t))
  (format stream "(sys ~A :schema ~S)" (system-components sys) (system-schema sys)))

(defclass engine () ())

(defgeneric same (a b)
  (:method ;; By default, most things are not the same.
      ((a t) (b t)) nil)
  (:method ((a signature) (b signature))
    (sig-equal a b))
  (:method ((a transform) (b transform))
    (and (same (transform-signature a) (transform-signature b))
	 (equal (transform-implementation a) (transform-implementation b))))
  (:method ((a component) (b component))
    (and (subsetp (component-transforms a) (component-transforms b) :test #'same)
	 (subsetp (component-transforms b) (component-transforms a) :test #'same)))
  (:method ((a list) (b list))
    (and (eql (length a) (length b))
	 (every #'same a b))))

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

(defgeneric solve (signature system data-map)
  (:method ((system system) (signature signature) (initial-data-map data-map))
    (reduce (lambda (data-map transform)
	      (apply transform data-map))
	    (plan signature system)
	    :initial-value initial-data-map)))
  

;;; Syntax
(defmacro sig ((&rest inputs) arrow (&rest outputs))
  (assert (eql arrow '->))
  `(make-signature ',inputs ',outputs))

(defmacro transform ((&rest inputs) arrow (&rest outputs) eqmark implementation)
  (assert (eql arrow '->))
  (assert (eql eqmark '===))
  `(let ((sig (make-signature ',inputs ',outputs)))
     (make-instance 'transform :signature sig :implementation ',implementation)))

(defmacro component (transforms)
  `(make-instance 'component :transforms (list ,@transforms)))

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
	 (let ((,new-data-map (make-data-map (data-map-pairs ,data-map)))
	       (,out (multiple-value-list (progn ,@body))))
	   
	   ,@(loop for key in output
		collect `(setf (getd ',key ,new-data-map) (pop ,out)))
	   ,new-data-map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests


(defparameter sig1 (sig (a b c) -> (d)))
(defparameter sig2 (sig (b c d) -> (e f)))
(defparameter sig3 (sig (a b c) -> (e f)))

(defparameter t1 (transform (a b c) -> (d) === :asdf))
(defparameter t2 (transform (x y z) -> (q) === :uiop))
(defparameter t3 (transform (b c d) -> (e f) === :fdsa))

(defparameter s1 (sys ((component (t1)))))
(defparameter s2 (sys ((component (t1 t2 t3)))))

(assert (same (plan s1 sig1) (list t1)))
(assert (same (plan s1 sig2) nil)) 
(assert (same (plan s1 sig3) nil))

(assert (same (plan s2 sig1) (list t1)))
(assert (same (plan s2 sig2) (list t3)))
(assert (same (plan s2 sig3) (list t1 t3)))

#|
(plan sig1 s1) => (((SIG (A B C) -> (D)) . (TRANSFORM (SIG (A B C) -> (D)) === ASDF)))
(plan sig2 s1) => nil
(plan sig3 s1) => nil

(plan sig1 s2) => (((TRANSFORM (SIG (A B C) -> (D)) === ASDF)))                          ; *transforms-tried* 3

(plan sig2 s2) => (((TRANSFORM (SIG (B C D) -> (E F)) === FDSA))                         ; *transforms-tried* 3

(plan sig3 s2) => ((TRANSFORM (SIG (A B C) -> (D)) === ASDF)
		   (TRANSFORM (SIG (B C D) -> (E F)) === FDSA))                          ; *transforms-tried* 6
|#
