(in-package :orient.lang)

(def-suite orient-lang-suite)
(in-suite orient-lang-suite)

;;; Reader

(defvar *line-comment* "//")

(defvar *parse-line* 0)
(defvar *lang-load-pathname* nil)

(define-condition lang-error (error)
  ((line :initarg :line :initform *parse-line* :accessor lang-error-line)
   (description :initarg :description :initform nil :accessor lang-error-description))
  (:report (lambda (condition stream)
	     (format stream "Error parsing Orient lang at line ~d ~@[: ~a~]"
		     (lang-error-line condition)
		     (lang-error-description condition)))))

(defun comma-reader (stream char)
  (declare (ignore stream char))
  (values))

(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))

(defparameter *lang-readtable-case* :upcase)
(defparameter *lang-definition-readtable-case* :preserve)

(defun make-lang-readtable ()
  (let ((readtable (copy-readtable nil)))
    (setf (readtable-case readtable) *lang-readtable-case*)
    readtable))

(defparameter *lang-readtable* (make-lang-readtable))

(defun make-definition-readtable ()
  (let ((*readtable* (copy-readtable *readtable*)))
    (set-macro-character #\, #'(lambda (stream char)
				 (declare (ignore stream char))
				 (values)))
    (set-macro-character #\] #'read-delimiter)

    ;; This is failing with end of file, even when it seemingly shouldn't.
    (set-macro-character #\[
			 #'(lambda (stream char)
			     (declare (ignore char))
			     (awhen (read-delimited-list #\] stream t)
			       (coerce it 'vector))))

    (setf (readtable-case *readtable*) *lang-definition-readtable-case*)
    *readtable*))

(defparameter *definition-readtable* (make-definition-readtable))

(defparameter *lang-read-infix* (editor-hints.named-readtables:ensure-readtable 'cmu-infix:syntax))

(progn (setf (readtable-case cmu-infix::*infix-readtable*) *lang-readtable-case*))

(defun read-with-case (stream &rest args)
  (case-transform-tree (apply #'read stream args)))

(defun read-infix (stream)
  (let* ((*readtable* *lang-read-infix*)
	 (saved-infix-readtable-case (readtable-case cmu-infix::*infix-readtable*)))
    ;; This is slightly evil; we temporarily change the READTABLE-CASE of the internal infix readtable.
    (unwind-protect
	 (progn
	   (setf (readtable-case cmu-infix::*infix-readtable*) *lang-readtable-case*)
	   (read-with-case stream))
      (setf (readtable-case cmu-infix::*infix-readtable*) saved-infix-readtable-case))))

;;; End Reader

(defun clean-line (string)
  (let* ((comment-start (search *line-comment* string))
	 (without-comment (if comment-start
			      (subseq string 0 comment-start)
			      string))
	 (comment (when comment-start
		    (subseq string comment-start)))
	 (trimmed (string-right-trim '(#\Space #\Tab #\Newline) without-comment))
	 (indent-level (position-if (lambda (x) (not (eql x #\space))) trimmed)))
    (values trimmed indent-level comment)))

(defun case-transform-tree (tree)
  (transform-tree #'(lambda (symbol)
		      (intern (interface:camel-case-to-lisp* (symbol-name symbol)); (string-downcase (symbol-name symbol)))
			      (symbol-package symbol)))
		  tree :test #'symbolp))

(defun read-infix-from-string (string)
  (with-input-from-string (in (format nil "#I(~a)" string))
    (read-infix in)))

(defun parse-line (string)
  (multiple-value-bind (cleaned indent-level comment)
      (clean-line string)
    (declare (ignore comment))
    (let ((last-pos (1- (length cleaned))))
      (cond ((zerop (length cleaned)) (list nil))
	    ((eql (aref (string-left-trim '(#\space #\tab) cleaned) 0) #\:) ;; First char is colon means this is an include. TODO: document or change.
	     (let* ((sub-system-pathname (pathname (string-left-trim '(#\space #\tab #\:) cleaned)))
		    (parsed-sub-system (get-system sub-system-pathname
						   :base-location *lang-load-pathname*
						   :as :parsed
						   :group nil)))
	       (loop for (parsed parsed-indent-level) in parsed-sub-system
		    collect (list parsed (+ indent-level parsed-indent-level)))))
	    ((eql (aref cleaned last-pos) #\:) ;; Last char is colon means this is a simple definition.
	     (list (list (parse-definition-line (subseq string 0 last-pos)) indent-level)))
            ((search ":" cleaned) ;; Otherwise, a colon means this is an external definition. TODO: Refactor.
             (destructuring-bind  (definition-part library-file)
                 (string-split ":" cleaned)
               (let* ((lib (string-trim '(#\space #\tab) library-file))
                      (def (parse-definition-line definition-part)))
                 (when lib
                   (let ((resolved (truename (merge-pathnames lib *lang-load-pathname*))))
                     (setf (definition-external-path def) resolved)))
                 (list (list def indent-level)))))
	    (t (let* ((infix (read-infix-from-string cleaned))
		      (parsed (typecase infix
				(symbol infix)
				(t infix))))
		 (list (list parsed indent-level))))))))

(test parse-line-with-lib
  (let* ((line "  SomeLib: some_lib.exe"))
    (destructuring-bind ((definition indent-level)) (parse-line line)
      (is (equal '*some-lib (definition-name definition)))
      (is (equal "some_lib.exe" (definition-external-path definition)))
      (is (= 2 indent-level)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct definition name flags dependencies declarations descriptions constraints sub-definitions external-path)
  (defmethod make-load-form ((d definition) &optional environment)
    (make-load-form-saving-slots d :environment environment)))

(defun parse-definition-line (line)
  (let ((*readtable* *definition-readtable*))
    (with-input-from-string (in line)
      (let* ((name (read-with-case in nil nil))
	     (first (read-with-case in nil nil))
	     (second (read-with-case in nil nil))
	     (flags (when (listp first) first))
	     (dependencies (cond
			     ;; Any (Flags, F2) must precede [Dependencies, D2] — though both are optional.
			     ((vectorp first)
			      (assert (not (vectorp second)))
			      first)
			     (t (awhen (vectorp second)
				  (coerce second 'list))))))
	(make-definition :name name :flags flags :dependencies dependencies)))))

(defun parse-into-lines (stream)
  (let ((*readtable* *lang-readtable*))
    (remove nil (loop for line = (read-line stream nil)
		   while line
		   when (not (equal line ""))
		   append (parse-line line)) :key #'car)))

(defun group-by-indentation (input-lines &optional (indent-level 0) (acc '()))
  (loop
     (let ((input-line	(pop input-lines)))
       (destructuring-bind (&optional parsed line-indent-level comment)
	   input-line
	 (declare (ignore comment))
	 (cond
	   ((not (and parsed line-indent-level))
	    (return (values (reverse acc) input-lines)))
	   ((= line-indent-level indent-level)
	    (push parsed acc)
	    )
	   ((> line-indent-level indent-level)
	    (multiple-value-bind (result remaining-input-lines)
		(group-by-indentation input-lines line-indent-level (list parsed (pop acc)))
	      (push result acc)
	      (setq input-lines remaining-input-lines)
	      ))
	   (t
	    ;; Put INPUT-LINE back on INPUT-LINES to be handled by caller.
	    (push input-line input-lines)
	    (return (values (reverse acc) input-lines))))))))

(defun parse-file (filespec &key (group t))
  (with-open-file (in filespec :direction :input)
    (parse in :group group)))

(defun parse (stream &key (group t))
  (let* ((*readtable* *lang-readtable*)
	 (parsed-lines (parse-into-lines stream)))
    (if group
	(group-by-indentation parsed-lines)
	parsed-lines)))

(defun parse-string (input &key (group t))
  (with-input-from-string (in input)
    (parse in :group group)))

(deftype sexp (name) `(cons (eql ,name) list))

(defmacro push-end (value place)
  `(setf ,place (reverse (cons ,value (reverse ,place)))))

(defmacro append-end (list place)
  `(setf ,place (append ,place ,list)))

(defvar *constraint-counter* nil)

(defmacro with-constraint-counter (&body body)
  `(cond (*constraint-counter* ,@body)
	 (t (let ((*constraint-counter* 0))
	      ,@body))))

(defun new-constraint (symbol)
  (intern (format nil "~A.CONSTRAINT~D%" symbol (incf *constraint-counter*))))

(defun constraints<-lang-form (form)
  ;; This is to catch constraints which are not equality. Those will be parsed as (SETQ …).
  (with-constraint-counter
    (handler-case
	(etypecase form
	  ((cons symbol)
	   (handler-case
	       (etypecase (cdr form)
		 ((cons symbol)
		  (let ((new-constraint (new-constraint (cadr form))))
		    `((,new-constraint ,form)
		      (,new-constraint (== true))))))
	     (type-error ()
	       (error 'lang-error
		      :description (format nil "Could not parse as constraint. Target attribute must be a symbol: ~s" (cadr form)))))))
      (type-error ()
	(error 'lang-error
	       :description (format nil "Constraint name must be a symbol: ~s" (car form)))))))

(defun %nested<-parsed (input)
  (with-constraint-counter
    (typecase input
      ((cons definition)
       (let ((definition (car input)))
	 (dolist (sub (cdr input))
	   (typecase sub
	     ((sexp declare)
	      (push-end sub (definition-declarations definition)))
	     ((sexp describe)
	      (push sub (definition-descriptions definition)))
	     ((sexp assume)
	      ;; TODO: Extract constraint from assert and add as normal constraint with metadata.
	      )
	     ((sexp setq)
	      (push-end (cdr sub) (definition-constraints definition)))
	     (definition (push-end sub (definition-sub-definitions definition)))
	     ((cons definition) (push-end (%nested<-parsed sub) (definition-sub-definitions definition)))
	     (t
	      (let ((constraint-forms (constraints<-lang-form sub)))
		(append-end constraint-forms (definition-constraints definition))))))
	 definition))
      (t (nested<-parsed input)))))

(defun nested<-parsed (parsed)
  "Compile from parsed syntax to correctly populated and nested definitions. Mutates definitions to accomplish this."
  (mapcar #'%nested<-parsed parsed))

(defun source<-nested (nested)
  (typecase nested
    (definition
     (cond ((not (definition-external-path nested))
            `(defconstraint-system ,(definition-name nested)
                 (,@(mapcan #'expand-declaration (definition-declarations nested))
                    ,@(mapcar #'expand-constraint (definition-constraints nested)))
               ,@(awhen (definition-sub-definitions nested) `(:subsystems ,(source<-nested it)))
               :flags ',(definition-flags nested)
               :dependencies ',(definition-dependencies nested)
               ,@(awhen (definition-descriptions nested)
                   (list :schema `(maybe-create-schema ',it)))))
           (t
            (let ((external-path (definition-external-path nested)))
              (assert (not (definition-sub-definitions nested)))
              `(defexternal-system ,(definition-name nested)
                 (,@(mapcan #'expand-declaration (definition-declarations nested))
                    ,@(expand-external-terms external-path (definition-constraints nested)))
                 :flags ',(definition-flags nested)
                 :dependencies ',(definition-dependencies nested)
                 :external-path ,external-path
                 ,@(awhen (definition-descriptions nested)
                     (list :schema `(maybe-create-schema ',it))))))))
    (list `(list ,@(mapcar #'source<-nested nested)))))

(defun maybe-create-schema (descriptions)
  (when descriptions
    (let ((schema (make-instance 'schema)))
      (loop for description-form in (reverse descriptions)
	 for parameter = (destructuring-bind (d name description &optional type)
			     description-form
			     (declare (ignore d))
			   (make-instance 'parameter
					  :name name
					  :description description
					  :type type))
	 do (push parameter (schema-parameters schema)))
      schema)))

(defun combine-systems (systems &key name)
  (make-instance 'system
		 :name name
		 :subsystems systems))

(defun expand-declaration (declaration)
  (etypecase declaration
    ((cons (eql declare) (cons symbol (cons (eql integer))))
     (list `(,(symbolconc (second declaration) '-integer%) (integer ,(second declaration)))))
    ((cons (eql declare) (cons symbol))
     ;; An empty declaration doesn't actually do anything at the moment.
     nil)))

(defun expand-constraint (constraint)
  (typecase constraint
    ((cons symbol (cons atom)) `(,(car constraint) (== ,(cadr constraint))))
    (t constraint)))

;; Example: (a (extern b c d))
(deftype extern-term () '(cons symbol (cons (cons (eql extern) list))))

(defun expand-external-terms (external-path terms) 
  (let* ((terms (mapcar
                 (partial #'expand-external-term external-path)
                 terms))
         ;; TODO: introduce syntax for grouping transformation terms into components.
         ;; For now, all transformations are included in a single component.
         (grouped-terms (list terms)))
    (loop for group in grouped-terms
         collect `(component (,@group)))))

(defun expand-external-term (external-path term)
  (etypecase term
    (extern-term
     (let ((target (car term))
           (dependencies (cdadr term)))
       `(transformation (,dependencies ~=> (,target)) == ,external-path)))))

(defun build-system (string &key (as :system) name)
  "Get system definition from LOCATION-SPEC and create a system from it."
  ;; TODO: make some syntax so this can be expressed more cleanly.
  (let ((raw string))
    (when (eql as :raw)
      (return-from build-system raw))
    (let ((parsed (parse-string raw)))
      (when (eql as :parsed)
	(return-from build-system parsed))
      (let ((nested (nested<-parsed parsed)))
	(when (eql as :nested)
	  (return-from build-system nested))
	(let ((source (source<-nested nested)))
	  (when (eql as :source)
	    (return-from build-system source))
	  (assert (or (null as) (eql as :system)))
	  (combine-systems (eval source) :name name))))))

(defun get-system (location-spec &key (as :system) name base-location (group t))
  "Get system definition from LOCATION-SPEC and create a system from it."
  ;; TODO: make some syntax so this can be expressed more cleanly.
  (multiple-value-bind (raw *lang-load-pathname*)
      (get-string location-spec :base-location base-location)
    (when (eql as :raw)
      (return-from get-system raw))
    (let ((parsed (parse-string raw :group group)))
      (when (eql as :parsed)
	(return-from get-system parsed))
      (let ((nested (nested<-parsed parsed)))
	(when (eql as :nested)
	  (return-from get-system nested))
	(let ((source (source<-nested nested)))
	  (when (eql as :source)
	    (return-from get-system source))
	  (assert (or (null as) (eql as :system)))
	  (combine-systems (eval source) :name name))))))

(test full-parse
  (let* ((input "ZigZag:
  Graph:
    DRG (Flag, Other) [Dependency, Dep2]:
      declare(degree_base, integer)
      drg_e = 0.80
      describe(drg_e, \"Epsilon\", number)
      drg_d = 1/4
    Chung:
      declare(degree_chung, integer)
    declare(block_size, integer)
    declare(size, integer)
    declare(nodes, integer)
    nodes = size / block_size
    degree = degree_base + degree_chung
    apple <= orange
    assume(dog < cat)
")
	 (expected-parse  '((#S(DEFINITION
				:NAME *ZIG-ZAG
				:FLAGS NIL
				:DEPENDENCIES NIL
				:DECLARATIONS NIL
				:DESCRIPTIONS NIL
				:CONSTRAINTS NIL
				:SUB-DEFINITIONS NIL)
			     (#S(DEFINITION
				    :NAME *GRAPH
				  :FLAGS NIL
				  :DEPENDENCIES NIL
				  :DECLARATIONS NIL
				  :DESCRIPTIONS NIL
				  :CONSTRAINTS NIL
				  :SUB-DEFINITIONS NIL)
			      (#S(DEFINITION
				     :NAME DRG
				   :FLAGS (*FLAG *OTHER)
				   :DEPENDENCIES (|Dependency| |Dep2|)
				   :DECLARATIONS NIL
				   :DESCRIPTIONS NIL
				   :CONSTRAINTS NIL
				   :SUB-DEFINITIONS NIL)
				 (DECLARE DEGREE-BASE
					  INTEGER)
				 (SETQ DRG-E 0.8) (DESCRIBE DRG-E "Epsilon" number) (SETQ DRG-D (/ 1 4)))
			      (#S(DEFINITION
				     :NAME *CHUNG
				   :FLAGS NIL
				   :DEPENDENCIES NIL
				   :DECLARATIONS NIL
				   :DESCRIPTIONS NIL
				   :CONSTRAINTS NIL
				   :SUB-DEFINITIONS NIL)
				 (DECLARE DEGREE-CHUNG
					  INTEGER))
			      (DECLARE BLOCK-SIZE
				       INTEGER)
			      (DECLARE SIZE
				       INTEGER)
			      (DECLARE NODES
				       INTEGER)
			      (SETQ NODES (/ SIZE BLOCK-SIZE))
			      (SETQ DEGREE (+ DEGREE-BASE DEGREE-CHUNG))
			      (<= APPLE ORANGE)
			      (ASSUME (< DOG CAT))))))
	 (expected-nested
	  `(#S(DEFINITION
		  :NAME *ZIG-ZAG
		:FLAGS NIL
		:DEPENDENCIES NIL
		:DECLARATIONS NIL
		:DESCRIPTIONS NIL
		:CONSTRAINTS NIL
		:SUB-DEFINITIONS (#S(DEFINITION
					:NAME *GRAPH
				      :FLAGS NIL
				      :DEPENDENCIES NIL
				      :DECLARATIONS ((DECLARE BLOCK-SIZE
							      INTEGER)
						     (DECLARE SIZE
							      INTEGER)
						     (DECLARE NODES
							      INTEGER))
				      :DESCRIPTIONS NIL
				      :CONSTRAINTS ((NODES (/ SIZE BLOCK-SIZE))
						    (DEGREE (+ DEGREE-BASE DEGREE-CHUNG))
						    (APPLE.CONSTRAINT1% (<= APPLE ORANGE))
						    (APPLE.CONSTRAINT1% (== TRUE)))
				      :SUB-DEFINITIONS (#S(DEFINITION
							      :NAME DRG
							    :FLAGS (*FLAG *OTHER)
							    :DEPENDENCIES (|Dependency|
									   |Dep2|)
							    :DECLARATIONS ((DECLARE
									    DEGREE-BASE
									    INTEGER))
							    :DESCRIPTIONS ((DESCRIBE DRG-E "Epsilon" number))
							    :CONSTRAINTS ((DRG-E 0.8)
									  (DRG-D (/ 1 4)))
							    :SUB-DEFINITIONS NIL)
							  #S(DEFINITION
								:NAME *CHUNG
							      :FLAGS NIL
							      :DEPENDENCIES NIL
							      :DECLARATIONS ((DECLARE
									      DEGREE-CHUNG
									      INTEGER))
							      :DESCRIPTIONS NIL
							      :CONSTRAINTS NIL
							      :SUB-DEFINITIONS NIL)))))))
	 (expected-source '(LIST
			    (DEFCONSTRAINT-SYSTEM *ZIG-ZAG NIL :SUBSYSTEMS
			     (LIST
			      (DEFCONSTRAINT-SYSTEM *GRAPH
				  ((BLOCK-SIZE-INTEGER%
				    (INTEGER BLOCK-SIZE))
				   (SIZE-INTEGER% (INTEGER SIZE))
				   (NODES-INTEGER% (INTEGER NODES))
				   (NODES (/ SIZE BLOCK-SIZE))
				   (DEGREE
				    (+ DEGREE-BASE DEGREE-CHUNG))
				   (APPLE.CONSTRAINT1% (<= APPLE ORANGE))
				   (APPLE.CONSTRAINT1% (== TRUE)))
				:SUBSYSTEMS
				(LIST
				 (DEFCONSTRAINT-SYSTEM DRG
				     ((DEGREE-BASE-INTEGER%
				       (INTEGER
					DEGREE-BASE))
				      (DRG-E
				       (==
					0.8))
				      (DRG-D
				       (/ 1
					  4)))
				   :FLAGS
				   '(*FLAG
				     *OTHER)
				   :DEPENDENCIES
				   '(|Dependency|
				     |Dep2|)
				   :SCHEMA
				   (MAYBE-CREATE-SCHEMA
				    '((DESCRIBE
				       DRG-E
				       "Epsilon" number))))
				 (DEFCONSTRAINT-SYSTEM *CHUNG
				     ((DEGREE-CHUNG-INTEGER%
				       (INTEGER
					DEGREE-CHUNG)))
				   :FLAGS
				   'NIL
				   :DEPENDENCIES
				   'NIL))
				:FLAGS 'NIL :DEPENDENCIES 'NIL))
			     :FLAGS 'NIL :DEPENDENCIES 'NIL))))
    
    (let ((parsed (parse-string input)))
      (is (equalp expected-parse parsed)))

    (let* ((nested (nested<-parsed (parse-string input))))
      (is (equalp expected-nested nested)))

    (let* ((source (source<-nested (nested<-parsed (parse-string input)))))
      (is (equalp expected-source source)))))

(test parse-file
  (let* ((input (project-merge "base/systems/zigzag.orient"))
	 (parsed (parse-file input)))
    (is (equalp '((#S(DEFINITION
		      :NAME *ZIG-ZAG
		      :FLAGS NIL
		      :DEPENDENCIES NIL
		      :DECLARATIONS NIL
		      :DESCRIPTIONS NIL
		      :CONSTRAINTS NIL
		      :SUB-DEFINITIONS NIL)
		   (#S(DEFINITION
			  :NAME *GRAPH
			:FLAGS NIL
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (#S(DEFINITION
			   :NAME DRG
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (DECLARE DEGREE-BASE
				INTEGER)
		       (SETQ DRG-E 0.8) (SETQ DRG-D (/ 1 4)))
		    (#S(DEFINITION
			   :NAME *CHUNG
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (DECLARE DEGREE-CHUNG
				INTEGER))
		    (DECLARE BLOCK-SIZE
			     INTEGER)
		    (DECLARE SIZE
			     INTEGER)
		    (DECLARE NODES
			     INTEGER)
		    (SETQ NODES (/ SIZE BLOCK-SIZE)) (SETQ DEGREE (+ DEGREE-BASE DEGREE-CHUNG)))
		   (#S(DEFINITION
			  :NAME *PARAMETERS
			:FLAGS NIL
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (#S(DEFINITION
			   :NAME *SOUNDNESS
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (DECLARE LAMBDA
				INTEGER)
		       (ASSUME (> SOUNDNESS 0)) (ASSUME (< SOUNDNESS 0.5))
		       (SETQ SOUNDNESS (EXPT (/ 1 2) (- LAMBDA))))
		    (#S(DEFINITION
			   :NAME *LAYERS
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (DECLARE LAYERS
				INTEGER)
		       (ASSUME (> LAYERS 0))
		       (> LAYERS
			  (+ (* 2 (LOG2 (/ 1 (* 3 (- EPSILON (* 2 DELTA))))))
			     (/ (* 2 (+ (- 0.8 EPSILON) DELTA)) (- 0.12 (* 2 DELTA))) 2)))
		    (#S(DEFINITION
			   :NAME *SPACE-GAP
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (ASSUME (> SPACEGAP 0)) (ASSUME (< SPACEGAP 0.5))
		       (SETQ SPACEGAP (+ EPSILON (* 2 DELTA)))
		       (SETF (+ EPSILON (* 3 DELTA)) 0.24))
		    (#S(DEFINITION
			   :NAME *CHALLENGES
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (#S(DEFINITION
			      :NAME *OFFLINE-CHALLENGES
			    :FLAGS NIL
			    :DEPENDENCIES NIL
			    :DECLARATIONS NIL
			    :DESCRIPTIONS NIL
			    :CONSTRAINTS NIL
			    :SUB-DEFINITIONS NIL)
			  (DECLARE OFFLINE-CHALLENGES
				   INTEGER)
			  (DECLARE OFFLINE-CHALLENGES-ALL
				   INTEGER)
			  (ASSUME (> OFFLINE-CHALLENGES 0))
			  (> OFFLINE-CHALLENGES
			     (- (/ LAMBDA (- (LOG2 (- 2 EPSILON (* 3 DELTA))) 1))))
			  (> OFFLINE-CHALLENGES-ALL (* LAYERS OFFLINE-CHALLENGES)))
		       (#S(DEFINITION
			      :NAME *ONLINE-CHALLENGES
			    :FLAGS NIL
			    :DEPENDENCIES NIL
			    :DECLARATIONS NIL
			    :DESCRIPTIONS NIL
			    :CONSTRAINTS NIL
			    :SUB-DEFINITIONS NIL)
			  (DECLARE ONLINE-CHALLENGES
				   INTEGER)
			  (ASSUME (> ONLINE-CHALLENGES 0))
			  (> ONLINE-CHALLENGES (LOG2 (/ 1 (* 3 (- EPSILON (* 2 DELTA)))))))))
		   (#S(DEFINITION
			  :NAME *ENCODING
			:FLAGS NIL
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (ASSUME (> KDF-CONTENT 0)) (ASSUME (> ENCODING-TIME 0))
		    (ASSUME (> POLLING-TIME 0)) (SETQ KDF-CONTENT (+ DEGREE 1))
		    (SETQ ENCODING-TIME
			  (* LAYERS NODES (* KDF-CONTENT BLOCK-SIZE) KDF-HASH-TIME))
		    (SETQ MALICIOUS-ENCODING (/ ENCODING-TIME ENCODING-AMAX))
		    (SETQ POLLING-TIME (* MALICIOUS-ENCODING DRG-D)))
		   (#S(DEFINITION
			  :NAME *COMMITMENT
			:FLAGS (*ZIG-ZAG-COMMITMENT)
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (ASSUME (> REPLICA-COMMIT-TIME 0))
		    (SETQ REPLICA-COMMIT-TIME (* COMMIT-TIME 3)))
		   (#S(DEFINITION
			  :NAME SNARK
			:FLAGS (*ZIG-ZAG-COMMITMENT)
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (ASSUME (> OPENING-TIME 0))
		    (SETQ OPENING-PER-CHALLENGE (+ DEGREE-BASE (* 2 DEGREE-CHUNG) 1))
		    (SETQ OPENINGS (* OFFLINE-CHALLENGES-ALL OPENING-PER-CHALLENGE))
		    (#S(DEFINITION
			   :NAME *LEAF
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (SETQ LEAF-ELEMENTS LAYERS) (SETQ LEAF-SIZE (* LEAF-ELEMENTS BLOCK-SIZE))
		       (SETQ LEAF-CONSTRAINTS (* LEAF-SIZE LEAF-HASH-CONSTRAINTS))
		       (SETQ LEAF-TIME (* LEAF-ELEMENTS LEAF-HASH-CIRCUIT-TIME)))
		    (#S(DEFINITION
			   :NAME *INCLUSION
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (SETQ INCLUSION-CIRCUIT-TIME (* TREE-DEPTH MERKLE-HASH-TIME-CIRCUIT)))
		    (#S(DEFINITION
			   :NAME SNARK
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (SETQ SNARK-TIME-PER-TREE (* (+ LEAF-TIME INCLUSION-CIRCUIT-TIME) OPENING))
		       (SETQ SNARK-CIRCUIT-PER-TREE
			     (* (+ LEAF-CONSTRAINTS INCLUSION-CONSTRAINTS) OPENINGS))
		       (SETQ SNARK-TIME (* SNARK-CIRCUIT-PER-TREE 3))
		       (SETQ SNARK-CIRCUIT (* SNARK-CIRCUIT-PER-TREE 3))))
		   (#S(DEFINITION
			  :NAME *INCLUSION-PROOF
			:FLAGS NIL
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (#S(DEFINITION
			   :NAME *MERKLE-INCLUSION-PROOF
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (DECLARE TREE-DEPTH
				INTEGER)
		       (SETQ TREE-DEPTH (LOG2 NODES))
		       (SETQ INCLUSION-CONSTRAINTS (* TREE-DEPTH MERKLE-HASH-CONSTRAINTS))
		       (SETQ COMMIT-TIME (* NODES VC-HASH-CONSTRAINTS))))
		   (#S(DEFINITION
			  :NAME *SEAL
			:FLAGS NIL
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (SETQ SEAL-TIME (+ COMMIT-TIME SNARK-TIME ENCODING-TIME))
		    (SETQ PARALLEL-SEAL-TIME
			  (+ (/ (+ SNARK-TIME COMMIT-TIME) CORES) ENCODING-TIME)))
		   (#S(DEFINITION
			  :NAME *COST
			:FLAGS NIL
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (SETQ SEAL-COST
			  (* SEAL-TIME (+ CPU-COST-PER-SECOND MEMORY-COST-PER-SECOND))))))
		parsed))))

(test custom-components
  (let* ((input "Example:
  SomeLib: some_lib.exe
    a = extern(b, c)
    b = extern(a, c)
    c = extern(a, b)
")
         (src (build-system input :as :source))
         (sys (build-system input)))

    (is (equal '(LIST
                 (DEFCONSTRAINT-SYSTEM *EXAMPLE NIL :SUBSYSTEMS
                  (LIST
                   (DEFEXTERNAL-SYSTEM *SOME-LIB
                       ((COMPONENT
                         ((TRANSFORMATION ((B C) ~=> (A) )
                                          ==
                                          #1="some_lib.exe")
                          (TRANSFORMATION ((A C) ~=> (B))
                                          == #1#)
                          (TRANSFORMATION ((A B) ~=> (C))
                                          == #1#))))
                     :FLAGS 'NIL :DEPENDENCIES 'NIL
                     :EXTERNAL-PATH #1#))
                  :FLAGS 'NIL :DEPENDENCIES 'NIL))
               src))
    (solve-for sys nil)))
