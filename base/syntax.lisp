(in-package :orient)
;; (get-system "https://gist.githubusercontent.com/nicola/90f8e37648043118721dc8c4e9fb8542/raw/649bd8d4b4ad23914dcb365bdeeac650890c15fb/ubercalc-zigzag-model.orient" :as :system)

(def-suite orient-suite)
(in-suite orient-suite)

(defvar *line-comment* "//")

(defun comma-reader (stream char)
  (declare (ignore stream char))
  (values))

(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))

(defun make-definition-readtable ()
  (let ((*readtable* (copy-readtable nil)))
    (set-macro-character #\, #'(lambda (stream char)
				 (declare (ignore stream char))
				 (values)))
    (set-macro-character #\] #'read-delimiter)

    ;; This is failing with end of file, even when it seemingly shouldn't.
    (set-macro-character #\[
			 #'(lambda (stream char)
			     (declare (ignore char))
			     (read-delimited-list #\] stream t)))   
    *readtable*))

(defparameter *definition-readtable* (make-definition-readtable))

(defun read-infix (stream)
  (let ((*readtable* (editor-hints.named-readtables:ensure-readtable 'cmu-infix:syntax)))
    (read stream)))

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
      (let ((name (read in nil nil))
	    (flags (read in nil nil))
	    (dependencies (read in nil nil)))
	(make-definition :name name :flags flags :dependencies dependencies)))))

(defun parse-into-lines (stream)
  (loop for line = (read-line stream nil)
     while line
     when (not (equal line ""))
     collect (multiple-value-list (parse-line line))))

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
  (let ((parsed-lines (parse-into-lines stream)))
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
	   ((sexp declare) (push-end sub (definition-declarations definition)))
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
	,@(awhen (definition-sub-definitions nested) `(:subsystems ,(source<-nested it)))))
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

(defun get-system (uri &key (as :system) name)
  ;; TODO: make some syntax so this can be expressed more cleanly.
  (let ((raw (dex:get uri)))
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
				:NAME ZIGZAG
				:FLAGS NIL
				:DEPENDENCIES NIL
				:DECLARATIONS NIL
				:DESCRIPTIONS NIL
				:ASSUMPTIONS NIL
				:CONSTRAINTS NIL
				:SUB-DEFINITIONS NIL)
			     (#S(DEFINITION
				    :NAME GRAPH
				  :FLAGS NIL
				  :DEPENDENCIES NIL
				  :DECLARATIONS NIL
				  :DESCRIPTIONS NIL
				  :ASSUMPTIONS NIL
				  :CONSTRAINTS NIL
				  :SUB-DEFINITIONS NIL)
			      (#S(DEFINITION
				     :NAME DRG
				   :FLAGS (FLAG OTHER)
				   :DEPENDENCIES (DEPENDENCY DEP2)
				   :DECLARATIONS NIL
				   :DESCRIPTIONS NIL
				   :ASSUMPTIONS NIL
				   :CONSTRAINTS NIL
				   :SUB-DEFINITIONS NIL)
				 (DECLARE DEGREE_BASE
					  INTEGER)
				 (SETQ DRG_E 0.8) (SETQ DRG_D (/ 1 4)))
			      (#S(DEFINITION
				     :NAME CHUNG
				   :FLAGS NIL
				   :DEPENDENCIES NIL
				   :DECLARATIONS NIL
				   :DESCRIPTIONS NIL
				   :ASSUMPTIONS NIL
				   :CONSTRAINTS NIL
				   :SUB-DEFINITIONS NIL)
				 (DECLARE DEGREE_CHUNG
					  INTEGER))
			      (DECLARE BLOCK_SIZE
				       INTEGER)
			      (DECLARE SIZE
				       INTEGER)
			      (DECLARE NODES
				       INTEGER)
			      (SETQ NODES (/ SIZE BLOCK_SIZE))
			      (SETQ DEGREE (+ DEGREE_BASE DEGREE_CHUNG))))))
	 (expected-nested
	  `(#S(DEFINITION
		  :NAME ZIGZAG
		:FLAGS NIL
		:DEPENDENCIES NIL
		:DECLARATIONS NIL
		:DESCRIPTIONS NIL
		:ASSUMPTIONS NIL
		:CONSTRAINTS NIL
		:SUB-DEFINITIONS (#S(DEFINITION
					:NAME GRAPH
				      :FLAGS NIL
				      :DEPENDENCIES NIL
				      :DECLARATIONS ((DECLARE BLOCK_SIZE
							      INTEGER)
						     (DECLARE SIZE
							      INTEGER)
						     (DECLARE NODES
							      INTEGER))
				      :DESCRIPTIONS NIL
				      :ASSUMPTIONS NIL
				      :CONSTRAINTS ((NODES (/ SIZE BLOCK_SIZE))
						    (DEGREE (+ DEGREE_BASE DEGREE_CHUNG)))
				      :SUB-DEFINITIONS (#S(DEFINITION
							      :NAME DRG
							    :FLAGS (FLAG OTHER)
							    :DEPENDENCIES (DEPENDENCY DEP2)
							    :DECLARATIONS ((DECLARE
									    DEGREE_BASE
									    INTEGER))
							    :DESCRIPTIONS NIL
							    :ASSUMPTIONS NIL
							    :CONSTRAINTS ((DRG_E 0.8)
									  (DRG_D (/ 1 4)))
							    :SUB-DEFINITIONS NIL)
							  #S(DEFINITION
								:NAME CHUNG
							      :FLAGS NIL
							      :DEPENDENCIES NIL
							      :DECLARATIONS ((DECLARE
									      DEGREE_CHUNG
									      INTEGER))
							      :DESCRIPTIONS NIL
							      :ASSUMPTIONS NIL
							      :CONSTRAINTS NIL
							      :SUB-DEFINITIONS NIL)))))))
	 (expected-source '(list
			    (defconstraint-system zigzag ()
			     :subsystems (list
					  (defconstraint-system graph
					      ((block_size-integer% (integer block_size))
					       (size-integer% (integer size))
					       (nodes-integer% (integer nodes))
					       (nodes (/ size block_size))
					       (degree (+ degree_base degree_chung)))
					    :subsystems (list
							 (defconstraint-system drg ((degree_base-integer% (integer degree_base))
										    (drg_e (== 0.8))
										    (drg_d (/ 1 4))))
							 (defconstraint-system chung ((degree_chung-integer% (integer degree_chung)))))))))))
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
		      :NAME ZIGZAG
		      :FLAGS NIL
		      :DEPENDENCIES NIL
		      :DECLARATIONS NIL
		      :DESCRIPTIONS NIL
		      :ASSUMPTIONS NIL
		      :CONSTRAINTS NIL
		      :SUB-DEFINITIONS NIL)
		   (#S(DEFINITION
			  :NAME GRAPH
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
		       (DECLARE DEGREE_BASE
				INTEGER)
		       (SETQ DRG_E 0.8) (SETQ DRG_D (/ 1 4)))
		    (#S(DEFINITION
			   :NAME CHUNG
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :ASSUMPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (DECLARE DEGREE_CHUNG
				INTEGER))
		    (DECLARE BLOCK_SIZE
			     INTEGER)
		    (DECLARE SIZE
			     INTEGER)
		    (DECLARE NODES
			     INTEGER)
		    (SETQ NODES (/ SIZE BLOCK_SIZE)) (SETQ DEGREE (+ DEGREE_BASE DEGREE_CHUNG)))
		   (#S(DEFINITION
			  :NAME PARAMETERS
			:FLAGS NIL
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:ASSUMPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (#S(DEFINITION
			   :NAME SOUNDNESS
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
			   :NAME LAYERS
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
			   :NAME SPACEGAP
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
			   :NAME CHALLENGES
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :ASSUMPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (#S(DEFINITION
			      :NAME OFFLINECHALLENGES
			    :FLAGS NIL
			    :DEPENDENCIES NIL
			    :DECLARATIONS NIL
			    :DESCRIPTIONS NIL
			    :ASSUMPTIONS NIL
			    :CONSTRAINTS NIL
			    :SUB-DEFINITIONS NIL)
			  (DECLARE OFFLINE_CHALLENGES
				   INTEGER)
			  (DECLARE OFFLINE_CHALLENGES_ALL
				   INTEGER)
			  (ASSUME (> OFFLINE_CHALLENGES 0))
			  (> OFFLINE_CHALLENGES
			     (- (/ LAMBDA (- (LOG2 (- 2 EPSILON (* 3 DELTA))) 1))))
			  (> OFFLINE_CHALLENGES_ALL (* LAYERS OFFLINE_CHALLENGES)))
		       (#S(DEFINITION
			      :NAME ONLINECHALLENGES
			    :FLAGS NIL
			    :DEPENDENCIES NIL
			    :DECLARATIONS NIL
			    :DESCRIPTIONS NIL
			    :ASSUMPTIONS NIL
			    :CONSTRAINTS NIL
			    :SUB-DEFINITIONS NIL)
			  (DECLARE ONLINE_CHALLENGES
				   INTEGER)
			  (ASSUME (> ONLINE_CHALLENGES 0))
			  (> ONLINE_CHALLENGES (LOG2 (/ 1 (* 3 (- EPSILON (* 2 DELTA)))))))))
		   (#S(DEFINITION
			  :NAME ENCODING
			:FLAGS NIL
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:ASSUMPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (ASSUME (> KDF_CONTENT 0)) (ASSUME (> ENCODING_TIME 0))
		    (ASSUME (> POLLING_TIME 0)) (SETQ KDF_CONTENT (+ DEGREE 1))
		    (SETQ ENCODING_TIME
			  (* LAYERS NODES (* KDF_CONTENT BLOCK_SIZE) KDF_HASH_TIME))
		    (SETQ MALICIOUS_ENCODING (/ ENCODING_TIME ENCODING_AMAX))
		    (SETQ POLLING_TIME (* MALICIOUS_ENCODING DRG_D)))
		   (#S(DEFINITION
			  :NAME COMMITMENT
			:FLAGS (ZIGZAGCOMMITMENT)
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:ASSUMPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (ASSUME (> REPLICA_COMMIT_TIME 0))
		    (SETQ REPLICA_COMMIT_TIME (* COMMIT_TIME 3)))
		   (#S(DEFINITION
			  :NAME SNARK
			:FLAGS (ZIGZAGCOMMITMENT)
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:ASSUMPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (ASSUME (> OPENING_TIME 0))
		    (SETQ OPENING_PER_CHALLENGE (+ DEGREE_BASE (* 2 DEGREE_CHUNG) 1))
		    (SETQ OPENINGS (* OFFLINE_CHALLENGES_ALL OPENING_PER_CHALLENGE))
		    (#S(DEFINITION
			   :NAME LEAF
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :ASSUMPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (SETQ LEAF_ELEMENTS LAYERS) (SETQ LEAF_SIZE (* LEAF_ELEMENTS BLOCK_SIZE))
		       (SETQ LEAF_CONSTRAINTS (* LEAF_SIZE LEAF_HASH_CONSTRAINTS))
		       (SETQ LEAF_TIME (* LEAF_ELEMENTS LEAF_HASH_CIRCUIT_TIME)))
		    (#S(DEFINITION
			   :NAME INCLUSION
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :ASSUMPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (SETQ INCLUSION_CIRCUIT_TIME (* TREE_DEPTH MERKLE_HASH_TIME_CIRCUIT)))
		    (#S(DEFINITION
			   :NAME SNARK
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :ASSUMPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (SETQ SNARK_TIME_PER_TREE (* (+ LEAF_TIME INCLUSION_CIRCUIT_TIME) OPENING))
		       (SETQ SNARK_CIRCUIT_PER_TREE
			     (* (+ LEAF_CONSTRAINTS INCLUSION_CONSTRAINTS) OPENINGS))
		       (SETQ SNARK_TIME (* SNARK_CIRCUIT_PER_TREE 3))
		       (SETQ SNARK_CIRCUIT (* SNARK_CIRCUIT_PER_TREE 3))))
		   (#S(DEFINITION
			  :NAME INCLUSIONPROOF
			:FLAGS NIL
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:ASSUMPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (#S(DEFINITION
			   :NAME MERKLEINCLUSIONPROOF
			 :FLAGS NIL
			 :DEPENDENCIES NIL
			 :DECLARATIONS NIL
			 :DESCRIPTIONS NIL
			 :ASSUMPTIONS NIL
			 :CONSTRAINTS NIL
			 :SUB-DEFINITIONS NIL)
		       (DECLARE TREE_DEPTH
				INTEGER)
		       (SETQ TREE_DEPTH (LOG2 NODES))
		       (SETQ INCLUSION_CONSTRAINTS (* TREE_DEPTH MERKLE_HASH_CONSTRAINTS))
		       (SETQ COMMIT_TIME (* NODES VC_HASH_CONSTRAINTS))))
		   (#S(DEFINITION
			  :NAME SEAL
			:FLAGS NIL
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:ASSUMPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (SETQ SEAL_TIME (+ COMMIT_TIME SNARK_TIME ENCODING_TIME))
		    (SETQ PARALLEL_SEAL_TIME
			  (+ (/ (+ SNARK_TIME COMMIT_TIME) CORES) ENCODING_TIME)))
		   (#S(DEFINITION
			  :NAME COST
			:FLAGS NIL
			:DEPENDENCIES NIL
			:DECLARATIONS NIL
			:DESCRIPTIONS NIL
			:ASSUMPTIONS NIL
			:CONSTRAINTS NIL
			:SUB-DEFINITIONS NIL)
		    (SETQ SEAL_COST
			  (* SEAL_TIME (+ CPU_COST_PER_SECOND MEMORY_COST_PER_SECOND))))))
		parsed)))) 

