(in-package orient)
(def-suite orient-constraint-suite)
(in-suite orient-constraint-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraints

(defvar *constraint-factories* (tuple))

(defmacro constraint-system (constraint-definitions)
  `(make-constraint-system ',constraint-definitions))

(defun make-constraint-system (constraint-definitions)
  "Takes a list of constraint definitions (which may include 'system constraint' definitions) and returns a system containing the corresponding constraint components or constraint subsystems."
  (let* ((constraints (make-constraints constraint-definitions))
	 (components)
	 (systems))
    (dolist (constraint constraints)
      (typecase constraint
	(component (push constraint components))
	(system (push constraint systems))))
    (make-instance 'system
		   :components (nreverse components)
		   :subsystems (nreverse systems))))

(defun make-constraints (constraint-definitions)
  (mapcar #'make-constraint constraint-definitions))

(defun make-constraint (constraint-definition)		
  "Takes a constraint definition and returns a component."
  (destructuring-bind (name (operator &rest args))
      constraint-definition
    (make-operation-constraint operator name args)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-constraint-forms (constraint-forms)
    (mapcar (lambda (constraint-form) (expand-constraint-form constraint-form))
	    constraint-forms))

  (defun expand-constraint-form (constraint-form)
    (destructuring-bind (target (op &rest inputs) &key (constraint-factories '*constraint-factories*))
	constraint-form
      (declare (ignore constraint-factories))
      ;;; WITH-NAMESPACE must be lexically bound in containing expression.
      `(make-operation-constraint ',op (with-namespace ',target) (list ,@(mapcar (lambda (input)
										  `(with-namespace ',input))
										 inputs))))))

(defmacro define-constraint (operator (target (op &rest inputs) &key (constraint-factories '*constraint-factories*)) &rest body)
  ;; TODO: make a macro to reduce this documentation-supporting boilerplate.
  (multiple-value-bind (documentation transformations)
      (typecase (first body)
	(string (destructuring-bind (doc transformations)
		    body
		  (values doc transformations)))
	(t (destructuring-bind (transformations)
	       body
	     (values nil transformations))))
    (declare (ignore documentation))
    (assert (eq operator op))
    `(setf (tref ',operator ,constraint-factories)
	   (constraint (,target (,op ,@inputs)) ,transformations))))



(defmacro constraint ((target (operator &rest inputs)) transformations)
  `(lambda (,target args &key source-operator source-name source-args)
     (destructuring-bind (,@inputs) args
       (make-instance 'component
		      :transformations (list ,@transformations)
		      :operation (or source-operator ',operator)
		      :target (or source-name ,target)
		      :args (or source-args args)))))

(defmacro define-alias-constraint (operator (target (op &rest inputs) &key (constraint-factories '*constraint-factories*))
				   &rest body)
  ;; Define OPERATOR as an alias for another operator but with rearranged target and inputs.
  ;; This is most commonly used to implement the inverse of a binary operator in terms of that operator.
  (destructuring-bind (documentation (other-target (other-op &rest other-inputs)))
      (typecase (first body)
	(string (destructuring-bind (doc alias-spec)
		    body
		  (list doc alias-spec)))
	(t (destructuring-bind (alias-spec)
	       body
	     (list nil alias-spec))))
    (declare (ignore documentation))
    (assert (eq operator op))
    `(setf (tref ',operator ,constraint-factories)	   
	   (lambda (,target args)
	     (destructuring-bind (,@inputs) args
	       (make-operation-constraint ',other-op ,other-target (list ,@other-inputs)
					  :source-operator ',operator
					  :source-name ,target
					  :source-args args))))))

#+(or)
(defmacro define-alias-constraint (operator (target (op &rest inputs) &key (constraint-factories '*constraint-factories*))
				   &rest body)
  ;; Define OPERATOR as an alias for another operator but with rearranged target and inputs.
  ;; This is most commonly used to implement the inverse of a binary operator in terms of that operator.
  (destructuring-bind (documentation (other-target (other-op &rest other-inputs)))
      (typecase (first body)
	(string (destructuring-bind (doc alias-spec)
		    body
		  (list doc alias-spec)))
	(t (destructuring-bind (alias-spec)
	       body
	     (list nil alias-spec))))
    (declare (ignore documentation))
    (assert (eq operator op))
    `(setf (tref ',operator ,constraint-factories)	   
	   (alias-constraint (,target (,op ,@inputs)
				      :other-op ,other-op
				      :other-target ,other-target
				      :other-inputs ,other-inputs)))))

(defmacro alias-constraint ((target (op &rest inputs) &key other-op other-target other-inputs))
    `(lambda (,target args)
	     (destructuring-bind (,@inputs) args
	       (make-operation-constraint ',other-op ,other-target (list ,@other-inputs)
					  :source-operator ',op
					  :source-name ,target
					  :source-args args))))

(defmacro define-system-constraint (name (target (op &rest inputs)
						&key (constraint-factories '*constraint-factories*))
				    &body body)
  (assert (eq name op))
  (destructuring-bind (definitions) body
    `(setf (tref ',name ,constraint-factories)
	   (lambda (,target args)
	     (system-constraint (,target (,op ,@inputs)) ,definitions)))))

(defmacro system-constraint ((target (op &rest inputs)) constraint-definitions)
  (declare (ignore op)) ;; Should we record this somewhere? May need.
  `(flet ((with-namespace (x) (symbolconc ,target '\. x)))
	       (destructuring-bind (,@inputs) args
		 (make-instance 'system
				:components
				(list
				 ;; Assign internally-namespaced result to supplied target.
				 (make-operation-constraint '== ,target (list (with-namespace ',target)))
				 ;; Assign supplied inputs to internally-namespaced inputs.
				 ,@(mapcar (lambda (input)
					     `(make-operation-constraint
					       '== (with-namespace ',input)
					       (list ,input)))
					   inputs)
				 ,@(expand-constraint-forms constraint-definitions))))))

(define-system-constraint some-complex-constraint (result (some-complex-constraint a b c))
  ((q (+ a b))
   (f (* b c))
   (g (- f q))
   (result (* g g))))

(test system-constraint
  (let ((system (constraint-system
		 ((x (some-complex-constraint aa bb cc)))))
	(satisfying-assignment (tuple (aa 1) (bb 2) (cc 3) (x 9)
				      (x.a 1) (x.b 2) (x.c 3) (x.q 3) (x.f 6) (x.g 3) (x.result 9))))
    (is (same satisfying-assignment
	      (solve-for system '() (tuple (aa 1) (bb 2) (cc 3)))))))
#|
Want to do this:

(defxxx some-complex-constraint (a b c)
  ((q (+ a b))
   (r (* b c))
   (internal (- q r))
   (some-complex-constraint (+ a internal))))

(constraint-system
 ((c (* a b))
  (d (+ e f))
  (g (- f a))
  (some-complex-constraint ...)
  (h (some-complex-constraint c d g))
  (i (+ h.internal 9))))
|#

(defun make-operation-constraint (operator name args &key (constraint-factories *constraint-factories*)
						       source-operator source-name source-args)
  (awhen (tref operator constraint-factories)
    (if source-operator
	;; Only pass alias-related keywords if this is an alias, which means it has a SOURCE-OPERATOR.
	(funcall it name args :source-operator source-operator :source-name source-name :source-args source-args)
	(funcall it name args))))

(define-constraint * (product (* a b))
    "PRODUCT = A * B"
  ((transformation* ((a b) -> (product)) == (* a b))
   (transformation* ((a product) -> (b)) == (/ product a))
   (transformation* ((b product) -> (a)) == (/ product b))))

#|
;;; Question: would this syntax or something like it be better?
(define-constraint * (a b)
    "* = A * B"
  ((transformation* ((a b) -> (*)) == (* a b))
   (transformation* ((a *) -> (b)) == (/ * a))
   (transformation* ((b *) -> (a)) == (/ * b))))
|#

(test multiplication-constraint
  "Test CONSTRAINT-SYSTEM with two multiplication contraints."
  (let* ((system (constraint-system
		  ((c (* a b))
		   (d (* a c)))))
	 (satsifying-assignment (tuple (a 3) (b 2) (c 6) (d 18))))

    (is (same satsifying-assignment
	      (solve-for system '(a b) (tuple (c 6) (d 18)))))    
    (is (same satsifying-assignment
	      (solve-for system '(a d) (tuple (b 2) (c 6)))))

    (is (same satsifying-assignment
	      (solve-for system '(b c) (tuple (a 3) (d 18)))))
    (is (same satsifying-assignment
	      (solve-for system '(b d) (tuple (a 3) (c 6)))))
    
    (is (same satsifying-assignment
	      (solve-for system '(c d) (tuple (a 3) (b 2)))))

    ;; Inconsistent data are not produced.
    (is (null (solve-for system '() (tuple (a 3) (b 2) (c 5) (d 10)))))
    
    ;; This one can't (currently) be solved.
    (is (same nil 
	      (solve-for system '(a c) (tuple (b 2) (d 18)))))))

(define-alias-constraint / (quotient (/ dividend divisor))
  "DIVIDEND / DIVISOR = QUOTIENT => DIVIDEND = QUOTIENT * DIVISOR"
  (dividend (* quotient divisor)))

(test division-constraint
  "Test CONSTRAINT-SYSTEM with a division constraint."
  (let* ((system (constraint-system ((z (/ x y)))))
	 (satisfying-assignment (tuple (z 3) (x 12) (y 4))))
    (is (same satisfying-assignment
	      (solve-for system '(z) (tuple (x 12) (y 4)))))
    (is (same satisfying-assignment
	      (solve-for system '(x) (tuple (y 4) (z 3)))))
    (is (same satisfying-assignment
	      (solve-for system '(y) (tuple (x 12) (z 3)))))

    ;; Inconsistent data are not produced.
    (is (null (solve-for system '() (tuple (z 3) (x 12) (y 5)))))
    ))

(define-constraint + (sum (+ a b))
    "SUM = A + B"
  ((transformation* ((a b) -> (sum)) == (+ a b))
   (transformation* ((a sum) -> (b)) == (- sum a))
   (transformation* ((b sum) -> (a)) == (- sum b))))

(test addition-constraint
  "Test CONSTRAINT-SYSTEM with two addition contraints."
  (let* ((system (constraint-system
		  ((c (+ a b))
		   (d (+ a c)))))
	 (satsifying-assignment (tuple (a 3) (b 2) (c 5) (d 8))))

    (is (same satsifying-assignment
	      (solve-for system '(a b) (tuple (c 5) (d 8)))))    
    (is (same satsifying-assignment
	      (solve-for system '(a d) (tuple (b 2) (c 5)))))

    (is (same satsifying-assignment
	      (solve-for system '(b c) (tuple (a 3) (d 8)))))
    (is (same satsifying-assignment
	      (solve-for system '(b d) (tuple (a 3) (c 5)))))
    
    (is (same satsifying-assignment
	      (solve-for system '(c d) (tuple (a 3) (b 2)))))

    ;; Inconsistent data are not produced.
    (is (null (solve-for system '() (tuple (a 3) (b 2) (c 5) (d 9)))))

    ;; This one can't (currently) be solved. TODO: Return relation of factorization tuples.
    (is (same nil 
	      (solve-for system '(a c) (tuple (b 2) (d 8)))))))

(define-alias-constraint - (difference (- minuend subtrahend))
  "MINUEND - SUBTRAHEND = DIFFERENCE => MINUEND = DIFFERENCE + SUBTRAHEND"
  (minuend (+ difference subtrahend)))

(test subtraction-constraint
  "Test CONSTRAINT-SYSTEM with a division constraint."
  (let* ((system (constraint-system ((z (- x y)))))
	 (satisfying-assignment (tuple (z 8) (x 12) (y 4))))
    (is (same satisfying-assignment
	      (solve-for system '(z) (tuple (x 12) (y 4)))))
    (is (same satisfying-assignment
	      (solve-for system '(x) (tuple (y 4) (z 8)))))
    (is (same satisfying-assignment
	      (solve-for system '(y) (tuple (x 12) (z 8)))))))

(define-constraint log (log (log n base))
    "LOG = (LOG N BASE)"
  ((transformation* ((n base) -> (log)) == (log n base))
   ;; TODO: (transformation ((,n ,log) -> (,base)) == ;; need log-th root of n) 
   (transformation* ((base log) -> (n)) == (expt base log))))

(test log-constraint
  "Test CONSTRAINT-SYSTEM with a log constraint."
  (let* ((system (constraint-system ((l (log n base)))))
	 (satisfying-assignment (tuple (base 2) (n 8.0) (l 3.0))))

    (is (same satisfying-assignment
	      (solve-for system '(l) (tuple (base 2) (n 8.0)))))
    (is (same satisfying-assignment
	      (solve-for system '(n) (tuple (base 2) (l 3.0)))))

    (is (not (same satisfying-assignment
		   (solve-for system '(l) (tuple (base 3) (n 8.0))))))

    ;; Inconsistent data are not produced.
    (is (null (solve-for system '() (tuple (base 3) (n 8.0) (l 5.0)))))
    ))

;; IT would probably be clearer to make this primary and define the log constraint as an alias.
(define-alias-constraint expt (result (expt base pow))
    "RESULT = (EXPT BASE POW)"
    (pow (log result base)))

(test expt-constraint
  "Test CONSTRAINT-SYSTEM with an exp constraint."
  (let* ((system (constraint-system ((p (expt base n)))))
	 (satisfying-assignment (tuple (base 2) (p 8.0) (n 3.0))))

    (is (same satisfying-assignment
	      (solve-for system '(p) (tuple (base 2) (n 3.0)))))
    (is (same satisfying-assignment
	      (solve-for system '(n) (tuple (base 2) (p 8.0)))))

    (is (not (same satisfying-assignment
		   (solve-for system '(p) (tuple (base 3) (n 8.0))))))

    ;; Inconsistent data are not produced.
    (is (null (solve-for system '() (tuple (base 3) (n 3.0) (p 8.0)))))
    ))

(defun must-integer (n)
  "Returns N if it is equivalent to an integer, otherwise NIL."
  (multiple-value-bind (div rem)
      (floor n)
    (and (zerop rem) div)))

(define-constraint integer (int (integer maybe-integer))
    "Returns a component which returns a relation binding INT to an integer with same value as MAYBE-INTEGER if it is equal to an integer,
 otherwise an empty relation."
  ((transformation* ((maybe-integer) => (int) :source integer) == (awhen (must-integer maybe-integer)
						    `((,it))))
   (transformation* ((int) => (maybe-integer) :source integer) == (progn (check-type int integer)
							 `((,int))))))

(test integer-constraint
  "Test CONSTRAINT-SYSTEM with an integer constraint."
  (let* ((system (constraint-system ((k (integer n))))))
    
    (is (same (relation (k n) (4 4.0))
     	      (solve-for system '(k) (tuple (n 4.0)))))
    (is (same (relation (k n) (4 4))
    	      (solve-for system '(k) (tuple (n 4)))))

    (is (same (relation (k n) (4 4))
    	      (solve-for system '(n) (tuple (k 4)))))
    (is (same (relation (k n) (4 4))
    	      (solve-for system '(n) (tuple (k 4)))))

    ;; It is a program error for the INTEGER input to the INTEGER constraint not to be an integer.
    (signals (type-error) (solve-for system '(n) (tuple (k 4.0))))

    ;; Inconsistent data are not produced. NOTE: Most such constraints yield NIL, but this explicitly produces an empty relation. Normalize?
    (is (same (relation (k n))
	      (solve-for system '() (tuple (k 3) (n 4.0)))))
    
    (is (same (relation (k n)) ;; Empty relation with expected heading.
    	      (solve-for system '(k) (tuple (n 4.1)))))))

(define-constraint == (a (== b))
  ;; TODO: in general, constraints must be able to function as restrictions -- but they are at least sometimes now short-circuited
    ;; by the planning process. (may be fixed -- verify)
    "Sets A to B or vice versa."
  ((transformation* ((a) -> (b)) == a)
   (transformation* ((b) -> (a)) == b)
   ;; This is no longer needed because apply-transformation eliminates inconsistent transformations *and* transformations
   ;; are eagerly matched. 
   #+(or)
   (transformation ((a b) => (a b)) == (awhen (same a b)
					 `((,a ,b))))
   ))

(test equality-constraint
  "Test CONSTRAINT-SYSTEM with an equality constraint."
  (let ((system (constraint-system ((a (== b)))))
	(satsifying-assignment (tuple (a 1) (b 1))))

    (is (same satsifying-assignment
	      (solve-for system '(a) (tuple (b 1)))))

    (is (same satsifying-assignment
	      (solve-for system '(b) (tuple (a 1)))))

    ;; Inconsistent data are not produced.
    (is (null (solve-for system '(a b) (tuple (a 1) (b 2)))))))

(define-constraint < (result (< a b))
  "RESULT == A < B." 
  ((transformation* ((a b) -> (result)) == (< a b))))

(test less-than-constraint
  "Test CONSTRAINT-SYSTEM with a LESS-THAN-CONSTRAINT."
  (let ((system (constraint-system ((x (< a b))))))
    (is (same (tuple (x t)) (ask system '(x) (tuple (a 1) (b 2)))))
    (is (same (tuple (x nil)) (ask system '(x) (tuple (a 2) (b 2)))))))

(define-constraint <= (result (<= a b))
    "RESULT == A <= B"
  ((transformation* ((a b) -> (result)) == (<= a b))))

(test less-than-or-equal-constraint
  "Test CONSTRAINT-SYSTEM with a LESS-THAN-OR-EQUAL-CONSTRAINT."
  (let ((system (constraint-system ((x (<= a b))))))
    (is (same (tuple (x t)) (ask system '(x) (tuple (a 1) (b 2)))))
    (is (same (tuple (x t)) (ask system '(x) (tuple (a 2) (b 2)))))
    (is (same (tuple (x nil)) (ask system '(x) (tuple (a 3) (b 2)))))))

(define-constraint > (result (> a b))
    "RESULT == A > B." 
  ((transformation* ((a b) -> (result)) == (> a b))))

(test greater-than-constraint
  "Test CONSTRAINT-SYSTEM with a GREATER-THAN-CONSTRAINT."
  (let ((system (constraint-system ((x (> a b))))))
    (is (same (tuple (x t)) (ask system '(x) (tuple (a 3) (b 2)))))
    (is (same (tuple (x nil)) (ask system '(x) (tuple (a 2) (b 2)))))))

(define-constraint >= (result (>= a b))
    "RESULT == A >= B" 
 ((transformation* ((a b) -> (result)) == (>= a b))))

(test greater-than-or-equal-constraint
  "Test CONSTRAINT-SYSTEM with a LESS-THAN-OR-EQUAL-CONSTRAINT."
  (let ((system (constraint-system ((x (>= a b))))))
    (is (same (tuple (x t)) (ask system '(x) (tuple (a 3) (b 2)))))
    (is (same (tuple (x t)) (ask system '(x) (tuple (a 2) (b 2)))))
    (is (same (tuple (x nil)) (ask system '(x) (tuple (a 1) (b 2)))))))

(defun expand-and-constraint (conjunction a b)
  "CONJUCTION = A && B"
  `(%component ((transformation ((,a ,b) -> (,conjunction)) == (and ,a ,b))
	       (transformation ((,a ,conjunction) => (,b)) == (if ,a
								  `((,(and ,conjunction ,a)))
								  '((t) (nil))))
	       (transformation ((,b ,conjunction) => (,a)) == (if ,b
								  `((,(and ,conjunction ,b)))
								  '((t) (nil)))))))

(define-constraint and (conjunction (and a b))
    "CONJUCTION = A && B"
  ((transformation* ((a b) -> (conjunction)) == (and a b))
   (transformation* ((a conjunction) => (b)) == (if a
						    `((,(and conjunction a)))
						    '((t) (nil))))
   (transformation* ((b conjunction) => (a)) == (if b
						      `((,(and conjunction b)))
						      '((t) (nil))))))

(test and-constraint
  "Test CONSTRAINT-SYSTEM with AND contraint."
  (let* ((system (constraint-system
		  ((c (and a b)))))
	 (sa-1 (tuple (a t) (b t) (c t)))
	 (sa-2 (tuple (a t) (b nil) (c nil)))
	 (sa-3 (tuple (a nil) (b nil) (c nil)))
	 (sa-4 (tuple (a nil) (b t) (c nil))))

    (is (same (rel sa-1) (solve-for system '(a) (tuple (b t) (c t)))))
    (is (same (rel sa-1) (solve-for system '(b) (tuple (a t) (c t)))))
    (is (same sa-1 (solve-for system '(c) (tuple (a t) (b t)))))

    (is (same (rel sa-2 sa-3) (solve-for system '(a) (tuple (b nil) (c nil)))))
    (is (same (rel sa-2) (solve-for system '(b) (tuple (a t) (c nil)))))
    (is (same sa-2 (solve-for system '(c) (tuple (a t) (b nil)))))

    (is (same (rel sa-2 sa-3) (solve-for system '(a) (tuple (b nil) (c nil)))))
    (is (same (rel sa-3 sa-4) (solve-for system '(b) (tuple (a nil) (c nil)))))
    (is (same sa-3 (solve-for system '(c) (tuple (a nil) (b nil)))))

    (is (same (rel sa-2 sa-3) (solve-for system '(a) (tuple (b nil) (c nil)))))
    (is (same (rel  sa-3 sa-4) (solve-for system '(b) (tuple (a nil) (c nil)))))
    (is (same sa-4 (solve-for system '(c) (tuple (a nil) (b t)))))
    
    ;; Inconsistent data are not produced.
    (is (null (solve-for system '() (tuple (a t) (b nil) (c t)))))
    ))

(test minimal-constraint-constants
  (is (same (apply-transformation
	     (let ((a 'a)
		   (sum 'c))
	       (transformation* ((a) -> (sum)) == (+ a 2)))
	     (tuple (a 3)))
	    (tuple (a 3)(c 5)))))

(test constraint-constants
  "Test CONSTRAINT-SYSTEM with some constants."
  (let* ((system (constraint-system ((c (+ a 2)))))
	 (satisfying-assignment (tuple (a 3) (c 5))))

    (is (same satisfying-assignment
	      (solve-for system '(a) (tuple (c 5)))))
    (is (same satisfying-assignment
	      (solve-for system '(c) (tuple (a 3)))))))

