(defpackage orient.lang
  (:use :common-lisp :orient :it.bese.FiveAm :orient.base.util :cl-json)
  (:import-from :fset :wb-map :convert)
  (:shadowing-import-from :fset :set)
  (:export :combine-systems :get-system :nested<-parsed :parse-string :source<-nested)
  (:nicknames :lang))

(in-package :orient.lang)

(def-suite orient-lang-suite)
(in-suite orient-lang-suite)

;;; Reader

(defvar *line-comment* "//")

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
			     (read-delimited-list #\] stream t)))
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
      (cond ((zerop (length cleaned)) nil)
	    ((eql (aref cleaned last-pos) #\:)
	     (values (parse-definition-line (subseq string 0 last-pos)) indent-level))
	    (t (let* ((infix (read-infix-from-string cleaned))
		      (parsed (typecase infix
				(symbol infix)
				(t infix))))
		 (values parsed indent-level)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct definition name flags dependencies declarations descriptions assumptions constraints sub-definitions)
  (defmethod make-load-form ((d definition) &optional environment)
    (make-load-form-saving-slots d :environment environment)))

(defun parse-definition-line (line)
  (let ((*readtable* *definition-readtable*))
    (with-input-from-string (in line)
      (let ((name (read-with-case in nil nil))
	    (flags (read-with-case in nil nil))
	    (dependencies (read-with-case in nil nil)))
	(make-definition :name name :flags flags :dependencies dependencies)))))

(defun parse-into-lines (stream)
  (let ((*readtable* *lang-readtable*))
    (loop for line = (read-line stream nil)
       while line
       when (not (equal line ""))
       collect (multiple-value-list (parse-line line)))))

(defun group-by-indentation (input-lines &optional (indent-level 0) (acc '()))
  (loop
     (let ((input-line (pop input-lines)))
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

(defun parse-string (input)
  (with-input-from-string (in input)
    (parse in)))

(deftype sexp (name) `(cons (eql ,name) list))

(defmacro push-end (value place)
  `(setf ,place (reverse (cons ,value (reverse ,place)))))

(defun %nested<-parsed (input)
  (typecase input
    ((cons definition)
     (let ((definition (car input)))
       (dolist (sub (cdr input))
	 (typecase sub
	   ((sexp declare)
	    (push-end sub (definition-declarations definition)))
	   ((sexp assert)
	    ;; TODO: Extract constraint from assert and add as normal constraint with metadata.
	    )
	   ((sexp assume) (push-end sub (definition-assumptions definition)))
	   ((sexp setq) (push-end (cdr sub) (definition-constraints definition)))
	   (definition (push-end sub (definition-sub-definitions definition)))
	   ((cons definition) (push-end (%nested<-parsed sub) (definition-sub-definitions definition)))
	   (t (push-end sub (definition-constraints definition)))))
       definition))
    (t (nested<-parsed input))))

(defun nested<-parsed (parsed)
  "Compile from parsed syntax to correctly populated and nested definitions. Mutates definitions to accomplish this."
  (mapcar #'%nested<-parsed parsed))

(defun source<-nested (nested)
  (typecase nested
    (definition
     `(defconstraint-system ,(definition-name nested)
	  (,@(mapcan #'expand-declaration (definition-declarations nested))
	     ,@(mapcar #'expand-constraint (definition-constraints nested)))
	,@(awhen (definition-sub-definitions nested) `(:subsystems ,(source<-nested it)))
	:flags ',(definition-flags nested)))
    (list `(list ,@(mapcar #'source<-nested nested)))))

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

(defun get-system (location-spec &key (as :system) name)
  "Get system definition from LOCATION-SPEC and create a system from it."
  ;; TODO: make some syntax so this can be expressed more cleanly.
  (let ((raw (get-string location-spec)))
    (when (eql as :raw)
      (return-from get-system raw))
    (let ((parsed (parse-string raw)))
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
      drg_d = 1/4
    Chung:
      declare(degree_chung, integer)
    declare(block_size, integer)
    declare(size, integer)
    declare(nodes, integer)
    nodes = size / block_size
    degree = degree_base + degree_chung
")
	 (expected-parse  '((#S(DEFINITION
				:NAME *ZIG-ZAG
				:FLAGS NIL
				:DEPENDENCIES NIL
				:DECLARATIONS NIL
				:DESCRIPTIONS NIL
				:ASSUMPTIONS NIL
				:CONSTRAINTS NIL
				:SUB-DEFINITIONS NIL)
			     (#S(DEFINITION
				    :NAME *GRAPH
				  :FLAGS NIL
				  :DEPENDENCIES NIL
				  :DECLARATIONS NIL
				  :DESCRIPTIONS NIL
				  :ASSUMPTIONS NIL
				  :CONSTRAINTS NIL
				  :SUB-DEFINITIONS NIL)
			      (#S(DEFINITION
				     :NAME DRG
				   :FLAGS (*FLAG *OTHER)
				   :DEPENDENCIES (*DEPENDENCY *DEP-2)
				   :DECLARATIONS NIL
				   :DESCRIPTIONS NIL
				   :ASSUMPTIONS NIL
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
				   :ASSUMPTIONS NIL
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
			      (SETQ DEGREE (+ DEGREE-BASE DEGREE-CHUNG))))))
	 (expected-nested
	  `(#S(DEFINITION
		  :NAME *ZIG-ZAG
		:FLAGS NIL
		:DEPENDENCIES NIL
		:DECLARATIONS NIL
		:DESCRIPTIONS NIL
		:ASSUMPTIONS NIL
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
				      :ASSUMPTIONS NIL
				      :CONSTRAINTS ((NODES (/ SIZE BLOCK-SIZE))
						    (DEGREE (+ DEGREE-BASE DEGREE-CHUNG)))
				      :SUB-DEFINITIONS (#S(DEFINITION
							      :NAME DRG
							    :FLAGS (*FLAG *OTHER)
							    :DEPENDENCIES (*DEPENDENCY
									   *DEP-2)
							    :DECLARATIONS ((DECLARE
									    DEGREE-BASE
									    INTEGER))
							    :DESCRIPTIONS NIL
							    :ASSUMPTIONS NIL
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
							      :ASSUMPTIONS NIL
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
				   (DEGREE (+ DEGREE-BASE DEGREE-CHUNG)))
				:SUBSYSTEMS
				(LIST
				 (DEFCONSTRAINT-SYSTEM DRG
				     ((DEGREE-BASE-INTEGER%
				       (INTEGER DEGREE-BASE))
				      (DRG-E (== 0.8))
				      (DRG-D (/ 1 4)))
				   :FLAGS '(*FLAG *OTHER))
				 (DEFCONSTRAINT-SYSTEM *CHUNG
				     ((DEGREE-CHUNG-INTEGER%
				       (INTEGER DEGREE-CHUNG)))
				   :FLAGS
				   'NIL))
				:FLAGS 'NIL))
			     :FLAGS 'NIL))))
    
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
		      :ASSUMPTIONS NIL
		      :CONSTRAINTS NIL
		      :SUB-DEFINITIONS NIL)
		   (#S(DEFINITION
			  :NAME *GRAPH
			:FLAGS NIL
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:ASSUMPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (#S(DEFINITION
			   :NAME DRG
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :ASSUMPTIONS NIL
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
			 :ASSUMPTIONS NIL
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
			:ASSUMPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (#S(DEFINITION
			   :NAME *SOUNDNESS
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :ASSUMPTIONS NIL
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
			 :ASSUMPTIONS NIL
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
			 :ASSUMPTIONS NIL
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
			 :ASSUMPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (#S(DEFINITION
			      :NAME *OFFLINE-CHALLENGES
			    :FLAGS NIL
			    :DEPENDENCIES NIL
			    :DECLARATIONS NIL
			    :DESCRIPTIONS NIL
			    :ASSUMPTIONS NIL
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
			    :ASSUMPTIONS NIL
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
			:ASSUMPTIONS NIL
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
			:ASSUMPTIONS NIL
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
			:ASSUMPTIONS NIL
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
			 :ASSUMPTIONS NIL
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
			 :ASSUMPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (SETQ INCLUSION-CIRCUIT-TIME (* TREE-DEPTH MERKLE-HASH-TIME-CIRCUIT)))
		    (#S(DEFINITION
			   :NAME SNARK
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :ASSUMPTIONS NIL
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
			:ASSUMPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (#S(DEFINITION
			   :NAME *MERKLE-INCLUSION-PROOF
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :ASSUMPTIONS NIL
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
			:ASSUMPTIONS NIL
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
			:ASSUMPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (SETQ SEAL-COST
			  (* SEAL-TIME (+ CPU-COST-PER-SECOND MEMORY-COST-PER-SECOND))))))
		parsed)))) 
