(defpackage :orient)

(deftype data-map () 'hash-table)

(defclass parameter ()
  ((name :initarg :name :initform (error "name missing") :accessor parameter-name)
   (description :initarg :description :accessor parameter-description)
   (type :initarg :type :initform nil :accessor parameter-type)))

(defclass schema ()
  ((parameters :initarg parameters :initform '() :accessor schema-parameters)))

(defun make-data-map (pairs)
  (let ((h (make-hash-table)))
    (loop for (k . v) in pairs do (setf (gethash k h) v))
    h))

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

(defmethod contains-p ((s signature) (other signature))
  "Returns true if s is a subset of other."
  (and (subsetp (signature-inputs s) (signature-inputs other))
       (subsetp (signature-outputs s) (signature-outputs other))))

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

(defvar *transforms-tried* 0)

(defgeneric %plan (element signature system plan)
  (:method ((transform transform) (signature signature) (system system) (plan list))
    (incf *transforms-tried*)
    (let* ((sig (transform-signature transform))
	   ;; Which of the still-needed outputs, if any, does the this transform's signature provide?
	   (provided-outputs (provides (signature-outputs signature) sig)))     
      (unless provided-outputs
	;; If this transform doesn't provide any needed outputs, fail early.
	(return-from %plan nil))

      ;; Otherwise, add the transform to the plan and update the signature to satisfy.
      (let* ((new-plan (cons transform plan))
	     ;; Inputs of the current transform which aren't trivially provided must now be outputs of the remaining plan (to be provided before
	     ;; this step's transform is applied).
	     (additional-outputs (set-difference (signature-inputs sig) (signature-inputs signature)))
	     ;; Outputs which still need to be provided.
	     (remaining-outputs (union (set-difference (signature-outputs sig) provided-outputs) additional-outputs)))
	(if remaining-outputs
	    ;; If there are still outputs which need to be satisfied, continue planning the system.
	    (%plan :system (make-signature (signature-inputs signature) remaining-outputs) system new-plan)
	    ;; Otherwise, return the new plan.
	    new-plan))))
  (:method ((component component) (signature signature) (system system) (plan list))
    (mapcan (lambda (transform)
	      (%plan transform signature system plan))
	    (component-transforms component)))
  (:method ((start (eql :system)) (signature signature) (system system) (plan list))
    (mapcan (lambda (component)
	      (%plan component signature system plan))
	    (system-components system))))

(defun plan (signature system)
  (let ((*transforms-tried* 0))
    (values (%plan :system (pruned-signature signature) system '())
	    *transforms-tried*)))

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

(defparameter sig1 (sig (a b c) -> (d)))
(defparameter sig2 (sig (b c d) -> (e f)))
(defparameter sig3 (sig (a b c) -> (e f)))

(defparameter s1 (sys ((component ((transform (a b c) -> (d) === :asdf))))))

(defparameter s2 (sys ((component ((transform (a b c) -> (d) === :asdf)
				   (transform (x y z) -> (q) === :uiop)
				   (transform (b c d) -> (e f) === :fdsa))))))

#|
(plan sig1 s1) => (((SIG (A B C) -> (D)) . (TRANSFORM (SIG (A B C) -> (D)) === ASDF)))
(plan sig2 s1) => nil
(plan sig3 s1) => nil

(plan sig1 s2) => (((TRANSFORM (SIG (A B C) -> (D)) === ASDF)))                          ; *transforms-tried* 3

(plan sig2 s2) => (((TRANSFORM (SIG (B C D) -> (E F)) === FDSA))                         ; *transforms-tried* 3

(plan sig3 s2) => ((TRANSFORM (SIG (A B C) -> (D)) === ASDF)
		   (TRANSFORM (SIG (B C D) -> (E F)) === FDSA))                          ; *transforms-tried* 6
|#
