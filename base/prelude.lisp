(in-package :orient)

;;;; Functions and declarations which should preced macro definitions.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *schemas* (make-hash-table))
  (defvar *systems* (make-hash-table))
  (defvar *transformations* (make-hash-table))
  (defvar *components* (make-hash-table)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolconc (&rest symbols)
    (intern (apply #'concatenate 'string (mapcar #'string symbols)))))

(defclass schema ()
  ((description :initarg :description :initform nil :accessor schema-description)
   (parameters :initarg :parameters :initform '() :accessor schema-parameters)))

(defclass system ()
  ((name :initarg :name :initform nil :accessor system-name)
   (schema :initarg :schema :initform nil :accessor system-schema)
   (components :initarg :components :initform '() :accessor system-components)
   (subsystems :initarg :subsystems :initform '() :accessor system-subsystems)
   (data :initarg :data :initform '() :accessor system-data)
   (flags :initarg :flags :initform '() :accessor system-flags)))

(defclass transformation ()
  ((signature :initarg :signature :initform (make-signature '() '()) :accessor transformation-signature)
   (implementation :initarg :implementation :initform nil :accessor transformation-implementation)
   (source :initarg :source :initform nil :accessor transformation-source)
   (name :initarg :name :initform nil :accessor transformation-name)))

(defclass component ()
  ((transformations :initarg :transformations :initform '() :accessor component-transformations)
   (operation :initarg :operation :initform nil :accessor component-operation)
   (target :initarg :target :accessor component-target)
   (args :initarg :args :accessor component-args)))

(defclass implementation ()
  ((module :initarg :module :initform *package* :accessor implementation-module)
   (name :initarg :name :accessor implementation-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: remove boilerplate with macros

(deftype toplevel-things () '(member :schema :system :transformation :component))    

(defun toplevel-hash-table (type)
  (ecase type
    (:schema *schemas*)
    (:system *systems*)
    (:transformation *transformations*)
    (:component *components*)))

(defun toplevel-type (type)
  (ecase type
    (:schema 'schema)
    (:system 'system)
    (:transformation 'transformation)
    (:component 'component)))

(defun define-toplevel-thing (name type thing)
  (assert (typep thing (toplevel-type type)))
  (let ((hash-table (toplevel-hash-table type)))
    (setf (gethash name hash-table) thing)))

(defun find-schema (schema-spec)
  (if (typep schema-spec 'schema)
      schema-spec
      (gethash schema-spec *schemas*)))

(defun find-system (system-spec)
  (if (typep system-spec 'system)
      system-spec
      (gethash system-spec *systems*)))

(defun find-transformation (transformation-spec)
  (if (typep transformation-spec 'transformation)
      transformation-spec
      (gethash transformation-spec *transformations*)))

(defun find-component (component-spec)
  (if (typep component-spec 'component)
      component-spec
      (gethash component-spec *components*)))
