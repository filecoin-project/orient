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

(defun plan-step (signature-to-apply transform)
  (cons signature-to-apply transform))

(defvar *transforms-tried* 0)

(defgeneric %plan (element signature system plan)
  (:method ((transform transform) (signature signature) (system system) (plan list))
    (incf *transforms-tried*)
    (let* ((sig (transform-signature transform))
	   (provided-outputs (provides (signature-outputs signature) sig)))

      (unless provided-outputs
	(return-from %plan '()))
     
      (let ((new-plan (cons transform plan))
	    (remaining-outputs (set-difference (signature-outputs sig) provided-outputs)))       
	(when remaining-outputs
	  (let ((diminished-signature (make-signature (signature-inputs signature) remaining-outputs)))
	    (return-from %plan (%plan :system diminished-signature system new-plan))))
	(let ((remaining-inputs (set-difference (signature-inputs sig) (signature-inputs signature))))
	  (if remaining-inputs
	    (let ((new-sig (make-signature (signature-inputs signature) remaining-inputs)))
	      (%plan :system new-sig system new-plan))
	    new-plan)))))
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
    (values (%plan :system signature system '())
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
(defparameter s2 (sys ((component ((transform (a b c) -> (d) === :asdf)))
		       (component ((transform (x y z) -> (q) === :uiop)))
		       (component ((transform (b c d) -> (e f) === :fdsa))))))

#|
(plan sig1 s1) => (((SIG (A B C) -> (D)) . (TRANSFORM (SIG (A B C) -> (D)) === ASDF)))
(plan sig2 s1) => nil
(plan sig3 s1) => nil

(plan sig1 s2) => (((TRANSFORM (SIG (A B C) -> (D)) === ASDF)))                          ; *transforms-tried* 3

(plan sig2 s2) => (((TRANSFORM (SIG (B C D) -> (E F)) === FDSA))                         ; *transforms-tried* 3

(plan sig2 s3) => ((TRANSFORM (SIG (A B C) -> (D)) === ASDF)
		   (TRANSFORM (SIG (B C D) -> (E F)) === FDSA))                          ; *transforms-tried* 6
|#
