(defpackage orient.interface
  (:use :common-lisp :orient :cl-json :it.bese.FiveAm :orient.base.util)
  (:import-from :fset :wb-map :convert)
  (:shadowing-import-from :fset :set)
  (:export :*schema-package* :dump-json :load-pipeline :load-transformation :load-tuple :load-json :<-json
	   :test-roundtrip)
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

(defun schema-intern (name &key (upcase t))
  (let ((name (if upcase (string-upcase name) name)))
    (when (> (length name) 0)
      (intern name (effective-schema-package-name)))))

(defgeneric load-json (type-spec json-pathspec)
  (:method ((type-spec t) (stream stream))
    (%load-json type-spec stream))
  (:method ((type-spec t) (json-pathspec t))
    (load-json type-spec (pathname json-pathspec)))
  (:method ((type-spec t) (json-pathname pathname))
    (%load-json type-spec (pathname json-pathname))))

(deftype tuple-pair () '(cons symbol (not cons)))

;; FIXME: Should probably change this name.
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
      (tuple (<-json :tuple (intern-schema-symbols json)))
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
      (string (schema-intern json)) ; Assume named systems will be in schema package.
      (list (<-json :system json))))
  (:method ((type-spec (eql :system)) (json list))
    (make-instance 'system
		   :components (extract-from-json-list :components :component json)
		   :subsystems (extract-from-json-list :subsystems :system-spec json)
		   :schema (extract-from-json :schema :schema json)
		   :data (extract-from-json-list :data :data json))))

(defun intern-schema-symbols (list)
  (transform-tree-if #'keywordp (lambda (x) (schema-intern (symbol-name x))) list))

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

;; FIXME: Need to add support for &acc, &group, &group-by, and &into --
;; here and in canonical printed representations.
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
	;; Can we just use the non-embedded signature?
	(encode-embedded-signature (transformation-signature transformation) stream)	
	(encode-implementation-json transformation stream)))))

(defun encode-implementation-json (transformation stream)
  (let ((impl (transformation-implementation transformation)))
    (typecase impl
      (implementation (as-object-member (:implementation stream)
			(encode-json impl stream)))
      (symbol (as-object-member (:implementation stream)
		(encode-json-plist `(:module
				     ,(package-name (symbol-package impl))
				     :name
				     ,(symbol-name impl))
				   stream)))
      (t (awhen (transformation-name transformation)
	   (as-object-member (:implementation stream)
	     (with-object (stream)
	       (as-object-member (:module stream)
		 (encode-json (package-name (symbol-package it)) stream))
	       (as-object-member (:name stream)
		 (encode-json (symbol-name it) stream)))))))))

(defvar *encode-constraint-component-formulas* t)
(defvar *encode-constraint-component-structure* nil)

(defmethod encode-json ((component component) &optional stream)
  (cond
    ((component-operation component)
     (with-object (stream)
       (when *encode-constraint-component-structure*
	 (as-object-member (:operation stream)
	   (encode-json (symbol-name (component-operation component)) stream))
	 (as-object-member (:target stream)
	   (encode-json (component-target component) stream))
	 (as-object-member (:args stream)
	   (encode-json (component-args component) stream)))
       (when *encode-constraint-component-formulas*
	 (encode-constraint component stream))))
    (t
     (with-object (stream)
       (as-object-member (:transformations stream)
	 (encode-json (component-transformations component) stream)
	 )))))

(defun encode-constraint (component stream)
  (as-object-member ((component-target component) stream)
    (encode-json (let ((*package* (effective-schema-package)))
		   (write-to-string `(,(component-operation component) ,@(component-args component))
				    :case :downcase
				    :pretty nil))
		 stream)))

(defmethod encode-json ((system system) &optional stream)
  (with-object (stream)
    (awhen (system-name system)
      (as-object-member (:name stream)
	(encode-json it stream)))
    (awhen (system-schema system)
      (as-object-member (:schema stream)
	(encode-json it stream)))
    (awhen (system-components system)
      (let ((constraint-components (remove-if-not #'component-operation it))
	    (non-constraint-components (remove-if #'component-operation it)))
	(when constraint-components
	  (as-object-member (:constraints stream)
	    (with-object (stream)
	      (dolist (component constraint-components)
		(encode-constraint component stream)))))
	(when non-constraint-components
	  (as-object-member (:components stream)
	    (encode-json non-constraint-components stream)))))
    (awhen (system-subsystems system)
      (as-object-member (:subsystems stream)
	(encode-json it stream)))
    (awhen (system-data system)
      (as-object-member (:data stream)
	(encode-json it stream)))))

(defmethod encode-json ((parameter parameter) &optional stream)
  (with-object (stream)
    (awhen (parameter-name parameter)
      (as-object-member (:name stream)
	(encode-json it stream)))
    (awhen (parameter-description parameter)
      (as-object-member (:description stream)
	(encode-json it stream)))
    (awhen (parameter-type parameter)
      (as-object-member (:type stream)
	(encode-json it stream)))))

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

(defun dump-json (spec thing stream &key expand-references)
  (declare (ignore spec))
  (let ((json:*lisp-identifier-name-to-json* #'string-downcase)
	(to-use (if expand-references
		    (expand-references thing)
		    thing)))
    (encode-json to-use stream)
    (terpri)))

(defun test-roundtrip (type-spec thing &key parsing-only)
  "Test that thing can be converted to JSON and back, preserving sameness. If PARSING-ONLY is true, only verify that encoding/decoding completes without error. This at least ensure the JSON is well-formed."
  (let ((returned (finishes
		    (let* ((*schema-package* *package*)
			   (json:*lisp-identifier-name-to-json* #'string-downcase)
			   (json-string (encode-json-to-string thing))
			   (json (decode-json-from-string json-string)))
		          (setq *dval* json-string)

		      (<-json type-spec json)))))
    (when (not parsing-only)
      (is (same thing returned)))))

(defun test-encoding (thing expected-json)
  (let ((json (finishes
		(let* ((*schema-package* *package*)
		       (json-string (encode-json-to-string thing)))
		  (decode-json-from-string json-string)))))
    (is (same expected-json json))))

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
					   :subsystems  (list 'system-a 'system-b)
					   :schema (schema "Test Schema"
							   (a-param "A parameter.")
							   (another "Something else you should know."))
					   ;; TODO: handle data.
					   :data (list (tuple (a 1) (b 2) (c 3)))
					   )))

(test constraint-encoding
  (test-encoding (constraint-system ((dog (* cat pig))
				     ;; Include an alias constraint to ensure the operation used in definition is encoded.
				     (slug (- snail shell))
				     ))
		 ;; TODO: We want :CONSTRAINTS to be a keyword, but DOG and SNAIL should be interned in *SCHEMA-PACKAGE*.
		 ;; Making this happen correctly may require a custom binding of *BEGINNING-OF-OBJECT-HANDLER*.
		 '((:CONSTRAINTS (:DOG . "(* cat pig)") (:slug . "(- snail shell)")))))

