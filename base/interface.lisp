(defpackage orient.interface
  (:use :common-lisp :orient :cl-json :it.bese.FiveAm)
  (:import-from :fset :wb-map :convert)
  (:shadowing-import-from :fset :set)
  (:export :*schema-package* :dump-json :load-pipeline :load-transformation :load-tuple :load-json :<-json)
  (:nicknames :interface))

(in-package "INTERFACE")
(def-suite interface-suite)
(in-suite interface-suite)

(defvar *schema-package*)

(defun effective-schema-package ()
  (or (and (boundp '*schema-package*)
	   *schema-package*)
      *package*))
  
(defun effective-schema-package-name ()
  (package-name (effective-schema-package)))

(defun schema-intern (name &key (upcase nil))
  (let ((name (if upcase (string-upcase name) name)))
    (when (> (length name) 0)
      (intern (simplified-camel-case-to-lisp name) (effective-schema-package-name)))))

(defgeneric load-json (type-spec json-pathspec)
  (:method ((type-spec t) (stream stream))
    (%load-json type-spec stream))
  (:method ((type-spec t) (json-pathspec t))
    (load-json type-spec (pathname json-pathspec)))
  (:method ((type-spec t) (json-pathname pathname))
    (%load-json type-spec (pathname json-pathname))))

(deftype tuple-pair () '(cons symbol (not cons)))
(deftype tuple () '(cons tuple-pair t))

(defun %load-json (type-spec source)
  (let* ((*json-symbols-package* *schema-package*)
	 (json (decode-json-from-source source)))
    (<-json type-spec json)))

;; TODO: Better destructuring to make this all clearer.
(defgeneric <-json (type-spec json)
  (:method ((type-spec (eql :pipeline)) (json list))
    (mapcar #'transformation<-json json))
  (:method ((type-spec (eql :transformation)) (json t))
    (transformation<-json json))
  (:method ((type-spec (eql :data)) (json t))
    (typecase json
      (atom json)
      (tuple (<-json :tuple json))
      ;; TODO: relations
    ))
  (:method ((type-spec (eql :tuple)) (json list))
    (make-tuple json t))
  (:method ((type-spec (eql :signature)) (json list))
    (let* ((string-inputs  (cdr (assoc :input json)))
	   (string-outputs (cdr (assoc :output json)))
	   (input (mapcar #'schema-intern string-inputs))
	   (output (mapcar #'schema-intern string-outputs)))
      (make-signature input output)))
  (:method ((type-spec (eql :schema)) (json list))
    (let ((description (cdr (assoc :description json)))
	  (parameters (extract-from-json-list :parameters :parameter json)))
      (make-instance 'schema :description description :parameters parameters)))
  (:method ((type-spec (eql :parameter)) (json list))
    (let ((name (schema-intern (cdr (assoc :name json))))
	  (description (cdr (assoc :description json)))
	  (type-spec (schema-intern (cdr (assoc :type json)))))
      (make-instance 'parameter :name name :description description :type type-spec)))
  (:method ((type-spec (eql :component)) (json list))
    (make-instance 'component :transformations (extract-from-json-list :transformations :transformation json)))
  (:method ((type-spec (eql :system-spec)) (json t))
    (typecase json
      (string
       ;; Assume named systems will be in schema package.
       (schema-intern json)) 
      (list (<-json :system json))))
  (:method ((type-spec (eql :system)) (json list))
    (make-instance 'system
		   :components (extract-from-json-list :components :component json)
		   :subsystems (extract-from-json-list :subsystems :system-spec json)
		   :schema (extract-from-json :schema :schema json)
		   :data (extract-from-json-list :data :data json)))
  ;;; TODO: :constraint-system
  )

(defun extract-from-json (key type-spec json)
  (<-json type-spec (cdr (assoc key json))))

(defun extract-from-json-list (key type-spec json)
  (mapcar (lambda (json)
	    (<-json type-spec json))
	  (cdr (assoc key json))))
	  

(defun load-pipeline (json-pathspec)
  (load-json :pipeline json-pathspec))

(defun load-transformation (json-pathspec)
  (load-json :transformation json-pathspec))

(defun load-tuple (json-source)
  (load-json :tuple json-source))

(defmethod encode-json ((tuple wb-map) &optional stream)
 (encode-json-alist (convert 'list tuple) stream))

(defmethod encode-json ((set set) &optional stream)
  (encode-json (convert 'list set) stream))

(defun signature-plist (signature)
  `(:input ,(convert 'list (signature-input signature)) :output ,(convert 'list (signature-output signature))))

(defmethod encode-json ((signature signature) &optional stream)
  (encode-json-plist
   (signature-plist signature)
   stream))

(defmethod encode-embedded-signature (signature stream)
  (as-object-member (:input stream)
    (encode-json (convert 'list (signature-input signature)) stream))
  (as-object-member (:output stream)
    (encode-json (convert 'list (signature-output signature)) stream)))

(defmethod encode-json ((transformation transformation) &optional stream)
  (with-object (stream)
    (as-object-member (:transformation stream)
      (with-object (stream)
	(encode-embedded-signature (transformation-signature transformation) stream)
	(as-object-member (:implementation stream)
	  (encode-implementation-json (transformation-implementation transformation) stream))))))

(defmethod encode-json :around ((component component) &optional stream)
  (cond
    ((slot-boundp component 'operation)
     (with-object (stream)
       (as-object-member (:operation stream)
	 (encode-json (symbol-name (component-operation component)) stream))
       (as-object-member (:target stream)
	 (encode-json (component-target component) stream))
       (as-object-member (:args stream)
	 (encode-json (component-args component) stream))))
    (t (call-next-method))))

(defun encode-implementation-json (impl stream)
  (typecase impl
    (implementation (encode-json impl stream))
    (symbol (encode-json-plist `(:module ,(package-name (symbol-package impl)) :name ,(symbol-name impl)) stream))
    (function (encode-json-plist `(:source ,(with-output-to-string (out)
					      (write (function-lambda-expression impl) :stream out)))
				 stream))))

;; TODO: roundtrip tests
;; TODO: there must be a cleaner/clearer way to do this.
(defun transformation<-json (transformation-spec)
  (let* ((transformation-json (cdr (assoc :transformation transformation-spec))) ;; TODO: Should the spec just use :transformation?
	 (implementation-spec (cdr (assoc :implementation transformation-json)))
	 (module-name (cdr (assoc :module implementation-spec)))
	 (package-name (cond (module-name (string-upcase module-name))
			     (*schema-package* (package-name *schema-package*))
			     (t (package-name *package*))))
	 (implementation-symbol-name (cdr (assoc :name implementation-spec)))
	 (implementation (make-instance 'implementation :name implementation-symbol-name :module package-name))
	 (signature (<-json :signature transformation-json)))
    (make-instance 'transformation :signature signature :implementation implementation)))

(defun dump-json (spec thing stream)
  (declare (ignore spec))
  (encode-json thing stream)
  (terpri))

(defun test-roundtrip (type-spec thing)
  (let* ((json-string (encode-json-to-string thing))
	 (*schema-package* *package*)
	 (json (decode-json-from-string json-string))
	 (returned (<-json type-spec json)))
    (is (same thing returned))))

(test roundtrip-transformation
  (test-roundtrip :transformation (transformation ((a b c) ~> (d e f)) == xxx)))

(test roundtrip-signature
  (test-roundtrip :signature (sig (q r s) -> (u v w))))

(test roundtrip-tuple
  (let ((*json-symbols-package* *package*))
    (test-roundtrip :tuple (tuple (a 1) (b 2) (c 3)))))

(test roundtrip-schema
  (test-roundtrip :schema (schema "Test Schema"
				  (a-param "A parameter.")
				  (another "Something else you should know."))))

(test roundtrip-parameter
  (test-roundtrip :parameter (make-instance 'parameter
					    :name 'parmesan
					    :description "The parmesan parameter."
					    :type 'cheese)))

(test roundtrip-component
  (test-roundtrip :component (component ((transformation ((a b c) ~> (d e f)) == xxx)
					 (transformation ((x y) ~> (z)) == yyy)))))

(test roundtrip-system
  (test-roundtrip :system (make-instance 'system
					   :components (list (component ((transformation ((a b c) ~> (d e f)) == xxx)
									 (transformation ((x y) ~> (z)) == yyy)))
							     (component ((transformation ((a b) ~> (c d e)) == xxx)
									 (transformation ((x y z) ~> (q)) == yyy))))
					   :subsystems (list 'system-a 'system-b)
					   :schema (schema "Test Schema"
							   (a-param "A parameter.")
							   (another "Something else you should know."))
					   ;; TODO: handle data.
					   :data (list (tuple (a 1) (b 2) (c 3)))
					   )))
