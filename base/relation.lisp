(in-package orient)

(def-suite orient-relation-suite)
(in-suite orient-relation-suite)

(deftype tuple () 'fset:wb-map)

(defun make-tuple (&optional pairs dotted)
  (convert 'wb-map pairs :value-fn (if dotted #'cdr #'cadr)))

;; (defmacro tref (attribute tuple)
;;   "Get value of ATTRIBUTE in TUPLE."
;;   `(@ ,tuple ,attribute))

(defclass relation () ())

(deftype strict-data () '(or null tuple relation))

(defgeneric attributes (tuple)
  (:method ((null null)) nil)
  (:method ((d wb-map)) (domain d))
  (:method ((r relation))
    (awhen (arb (tuples r))
      (attributes it))))

(defmethod compare ((a relation) (b relation))
  (compare (tuples a) (tuples b)))

;; TODO: Make TUPLES accept an optional conversion type to simplifying getting list for iteration.
(defclass simple-relation (relation)
  ((tuples :initarg :tuples :initform nil :accessor tuples :type set)))

(defmethod print-object ((relation relation) (stream t))
  (format stream "<RELATION ~S ~S>" (sort (attributes relation) #'string<) (tuples relation)))

(defgeneric ensure-tuples (attributed)
  (:method ((tuple wb-map))
    (set tuple))
  (:method ((relation relation))
    (tuples relation))
  (:method ((null null)) nil))

(defgeneric make-relation (tuples)
  (:documentation
   "Create relation from tuples, removing duplicates. Returns NIL if tuples don't have all have same attributes.")
  ;; Rather than return NIL, should mismatch be an error?
  (:method ((tuples list)) (make-relation (convert 'set tuples)))
  (:method ((tuples set))
    (let ((attributes (awhen (arb tuples) (attributes it))))
      (and (every (lambda (tuple) (equal? (attributes tuple) attributes)) tuples)
	   (make-instance 'simple-relation :tuples tuples)))))

(defgeneric cardinality (relation)
  (:method ((r relation))
    (size (tuples r))))

(defgeneric degree (attributed)
  (:method ((tuple wb-map))
    (size tuple))
  (:method ((r relation))
    (size (attributes r))))

(defgeneric %join (relation-a relation-b)
  (:documentation "Helper function so JOIN can avoid expensive creation of relations which will be immediately stripped for their contained tuples.")
  (:method ((a wb-map) (b wb-map))
    (let* ((a-attributes (attributes a))
	   (b-attributes (attributes b))
	   (shared (intersection a-attributes b-attributes))
	   (all-matchp (every (lambda (attr)
				(same (tref attr a) (tref attr b)))
			      shared)))
      (when all-matchp (map-union a b))))
  (:method ((a wb-map) (b relation))
    (less (gmap:gmap :set
		     (lambda (x) (%join x a))
		     (:set (tuples b)))
	  nil))
  (:method ((a relation) (b wb-map))
    (%join b a))
  (:method ((a relation) (b relation))
    (reduce (lambda (acc tuple)
	      (union acc (%join tuple b)))
	    (tuples a)
	    :initial-value (empty-set))))

(defgeneric join- (a b)
  (:documentation "Binary JOIN. Returns a relation, a single tuple, or NIL.")
  (:method ((a wb-map) (b wb-map))
    (%join a b))
  (:method ((a wb-map) (b relation))
    (make-relation (%join a b)))
  (:method ((a relation) (b wb-map))
    (join- b a))
  (:method ((a relation) (b relation))
    (make-relation (%join a b))))

(defun join (&rest things) (reduce #'join- things))

(defgeneric rename-attributes (old-new-pairs attributed)
  (:method ((pairs list) (r relation))
    (make-relation (image (lambda (tuple) (rename-attributes pairs tuple))
			  (tuples r))))
  (:method ((pairs list) (tuple wb-map))
    (reduce (lambda (acc pair)
	      (destructuring-bind (old new) pair
		(with (less acc old) new (@ acc old))))
	    pairs
	    :initial-value tuple)))

(test rename-attributes "Test RENAME-ATTRIBUTES."
      (is (same (tuple (d 1) (e 2) (c 3))
		(rename-attributes '((a d) (b e))
				   (tuple (a 1) (b 2) (c 3)))))

      (is (same (relation (d e c) (1 2 3) (4 5 6))
		(rename-attributes '((a d) (b e))
				   (relation (a b c) (1 2 3) (4 5 6))))))

;; Example, filter tuples where b is not 5:
;; (restrict (tfn (b) (= b 5)) asdf)
(defgeneric restrict (tuple-predicate relation)
  (:method ((tpred function) (relation relation))
    (make-relation (filter tpred (tuples relation)))))

(test restrict "Test RESTRICT."
      (is (same (relation (a b c) (4 5 6))
		(restrict (tfn (b) (= b 5))
			  (relation (a b c) (1 2 3) (4 5 6) (7 8 9))))))

(defgeneric project (attributes attributed &key invert)
  (:method ((attributes list) (attributed t) &key invert)
    (project (convert 'set attributes) attributed :invert invert))
  (:method ((attributes set) (null null) &key invert)
    (declare (ignore invert))
    nil)
  (:method ((attributes set) (tuple wb-map) &key invert)
    (if invert
	(fset:restrict-not tuple attributes)
	(fset:restrict tuple attributes)))
  (:method ((attributes set) (relation relation) &key invert)
    (make-relation
     (image (lambda (tuple) (project attributes tuple :invert invert))
	    (tuples relation)))))

(test project-tuple "Test PROJECT on tuple."
      (is (same (tuple (b 2) (c 3))
		(project '(b c) (tuple (a 1) (b 2) (c 3))))))

(test project-relation "Test PROJECT on relation."
      (is (same (relation (b c) (2 3))
		(project '(b c) (relation (a b c)
					  (1 2 3)
					  (9 2 3))))))

(defgeneric extract (relation)
  (:documentation "Extract the sole tuple of a RELATION of cardinality 1.")
  (:method ((relation relation))
    (and (= (cardinality relation) 1)
	 (arb (tuples relation)))))


(defgeneric ensure-relation (potential-relation)
  (:method ((r relation)) r)
  (:method ((tuple wb-map))
    (make-relation (set tuple)))
  (:method ((list list))
    (check-type list (cons tuple)) ;; Not exhaustive, but a good sanity check.
    (make-relation list))
  (:method ((set set))
    (let ((tuple (arb set)))
      (check-type tuple tuple) ;; Not exhaustive, but a good sanity check.
      (make-relation set)))
  )

(test rename-tuple
  "Test tuple renaming."
  (is (same (rename ((a f)(b g))
		    (tuple (a 1) (b 2) (c 3)))
	    (tuple (f 1) (g 2) (c 3)))))

(test rename-relation
  "Test relation renaming."
  (is (same (rename ((a f)(b g))
		    (relation (a b c) (1 2 3)))
	    (relation (f g c) (1 2 3)))))

(defgeneric map-relation (f relation)
  (:method ((f function) (r relation))
    (make-relation (image f (tuples r)))))

(test map-relation
  (let ((r (relation (a b c)
		     (1 2 3)
		     (1 2 4)
		     (3 4 5)
		     (4 5 6))))
    (is (same (map-relation (tfn (a b c)
			      (tuple (a (1+ a))
				     (b (1+ b))
				     (c 3)))
			    r)
	      (relation (a b c)
			(2 3 3)
			(4 5 3)
			(5 6 3))))))

(defun wildcardp (symbol)
  (find #\* (symbol-name symbol)))

(defun make-matcher (symbol)
  (let* ((a (cl-ppcre:regex-replace-all "\\." (symbol-name symbol) "\\\\."))
	 (b (cl-ppcre:regex-replace-all "\\*" a ".*"))
	 (scanner (cl-ppcre:create-scanner b)))
    (lambda (sym)
      (let ((name (symbol-name sym)))
	(multiple-value-bind (start end)
	    (funcall scanner name 0 (length name))
	  ;; Must match complete name.
	  (and (eql start 0)
	       (eql end (length name))))))))

(test make-matcher
  (let ((matcher (make-matcher 'asdf.*)))
    (is (funcall matcher 'asdf.fdsa))
    (is (not (funcall matcher 'asdffdsa))))
  (let ((matcher (make-matcher 'asdf)))
    (is (funcall matcher 'asdf))
    (is (not (funcall matcher 'asdffda)))))

(defgeneric wildcard-matches (wildcard attributed)
  (:method ((maybe-wildcard symbol) (attributes set))
    (let ((matcher (make-matcher maybe-wildcard)))
      (filter matcher attributes)))
  (:method ((wildcard symbol) (relation relation))
    (wildcard-matches wildcard (attributes relation)))
  (:method ((wildcard symbol) (attributes t))
    (wildcard-matches wildcard (convert 'set attributes)))
  (:method ((wildcards list) (attribute-spec t))
    (reduce #'union
	    (mapcar (lambda (symbol) (wildcard-matches symbol attribute-spec))
		    wildcards)))
  (:method ((wildcards t) (attribute-spec t))
    ;; TODO: use GMAP or more idiomatic FSET method here.
    (wildcard-matches (convert 'list wildcards) attribute-spec)
    ))

(test wildcard-matches
  (is (equal? (set 'asdf)
	      (wildcard-matches 'asdf (set 'asdf.fdsa 'asdf.qwer 'fdsa.qwer 'asdffdsa 'asdf))))

  (is (equal? (set 'asdf.qwer 'asdf.fdsa)
	      (wildcard-matches 'asdf.* (set 'asdf.fdsa 'asdf.qwer 'fdsa.qwer 'asdffdsa 'asdf))))

  (is (equal? (set 'a 'b 'c)
	      (wildcard-matches '(a b c) (set 'a 'b 'c 'd)))))

(defgeneric compute-group-attributes (input-attributes by-attributes new-attribute &key invert group)
  (:method ((input-attributes set) (by-attributes set) (new-attribute symbol) &key invert group)
    (assert (not (and invert group)))
    (let* ((by-attributes (wildcard-matches by-attributes input-attributes))
	   (group-attributes (set-difference (wildcard-matches group input-attributes) by-attributes))
	   (other-attributes (if (and by-attributes group)
				 group-attributes
				 (set-difference input-attributes by-attributes)))
	   (project-attributes (cond
				 (invert other-attributes)
				 (group (set-difference input-attributes group-attributes))
				 (t by-attributes)))
	   (inner-attributes (if invert
				 by-attributes
				 other-attributes))
	   (outer-attributes (if new-attribute
				 (with project-attributes new-attribute)
				 project-attributes)))
      (values outer-attributes inner-attributes)))
  (:method ((input-attributes t) (by-attributes t) (new-attribute symbol) &key invert group)
    (compute-group-attributes (convert 'set input-attributes) (convert 'set by-attributes) new-attribute :invert invert :group group)))

(defgeneric compute-group-attributes (input-attributes by-attributes new-attribute &key invert)
  (:method ((input-attributes set) (by-attributes set) (new-attribute symbol) &key invert)
    (let* ((by-attributes (wildcard-matches by-attributes input-attributes))
	   (other-attributes (set-difference input-attributes by-attributes)))
      (multiple-value-bind (project-attributes inner-attributes)
	  (if invert
	      (values other-attributes by-attributes)
	      (values by-attributes other-attributes))
	(let ((outer-attributes (if new-attribute
				    (with project-attributes new-attribute)
				    project-attributes)))
	  (values outer-attributes inner-attributes)))))
  (:method ((input-attributes t) (by-attributes t) (new-attribute symbol) &key invert)
    (compute-group-attributes (convert 'set input-attributes) (convert 'set by-attributes) new-attribute :invert invert)))

(defgeneric compute-group-attributes (input-attributes by-attributes new-attribute &key invert)
  (:method ((input-attributes set) (by-attributes set) (new-attribute symbol) &key invert)
    (let* ((by-attributes (wildcard-matches by-attributes input-attributes))
	   (project-attributes (if invert
				   (set-difference input-attributes by-attributes)
				   by-attributes))
	   (inner-attributes (set-difference input-attributes project-attributes))
	   (outer-attributes (if new-attribute
				 (with project-attributes new-attribute)
				 project-attributes)))
      (values outer-attributes inner-attributes)))
  (:method ((input-attributes t) (by-attributes t) (new-attribute symbol) &key invert)
    (compute-group-attributes (convert 'set input-attributes) (convert 'set by-attributes) new-attribute :invert invert)))

(test compute-group-attributes
  (flet ((test-case (input &key expected-outer expected-inner)
	   (multiple-value-bind (outer inner)
	       (apply #'compute-group-attributes input)
	     (is (equal? (convert 'set expected-outer)
			 outer))
	     (is (equal? (convert 'set expected-inner)
			 inner)))))
    (test-case '((a b c d) (a b) x)
	       :expected-outer '(a b x)
	       :expected-inner '(c d))
    (test-case '((a b c d) (a b) nil)
	       :expected-outer  '(a b)
	       :expected-inner '(c d))
    (test-case '((a b c d) (a b) x :invert t)
	       :expected-outer '(c d x)
	       :expected-inner'(a b))
    (test-case '((a b c d) (a b) nil :invert t)
	       :expected-outer '(c d)
	       :expected-inner '(a b))
    ;; (test-case '((a b c d) (a b) x :group (c))
    ;; 	       :expected-outer '(a b d x)
    ;; 	       :expected-inner '(c))
    ;; ;; Any GROUP attributes specified but which are also in GROUP-BY will not be included in inner attributes. (Should they be?)
    ;; (test-case '((a b c d) (a b c) nil :group (c d))
    ;; 	       :expected-outer '(a b c)
    ;; 	       :expected-inner '(d))
    ;; (test-case '((a b c d) (a b c) nil :group (c b d))
    ;; 	       :expected-outer '(a b c)
    ;; 	       :expected-inner '(d))
    ))

#+(or)
(defgeneric group (relation by-attributes new-attribute &key invert group)
  (:method ((relation relation) (by-attributes set) (new-attribute symbol) &key invert group)
    (assert (not (and invert group)))
    (let* ((by-attributes (wildcard-matches by-attributes relation))
	   (x (wildcard-matches group relation))
	   (group-attributes (set-difference x;(wildcard-matches group relation)
					     by-attributes))
	   (other-attributes (if (and by-attributes group)
				 group-attributes
				 (set-difference (attributes relation) by-attributes)))
	   (projected (project (cond
				 (invert other-attributes)
				 (group (set-difference (attributes relation) group-attributes))
				 (t by-attributes))
			       relation)))
      (map-relation (lambda (tuple)
		      (with tuple new-attribute (project (if invert
							     by-attributes
							     other-attributes)
							 (join tuple relation))))
		    projected)))
  (:method ((relation relation) (by-attributes list) (new-attribute symbol) &key invert group)
    (group relation (convert 'set by-attributes) new-attribute :invert invert :group group)))

(defgeneric group (relation by-attributes new-attribute &key invert)
  (:method ((relation relation) (by-attributes set) (new-attribute symbol) &key invert)
    (multiple-value-bind (outer-attributes inner-attributes)
	(compute-group-attributes (attributes relation) by-attributes new-attribute :invert invert)
      (let* ((projected (project outer-attributes relation)))
      (map-relation (lambda (tuple)
		      (with tuple new-attribute (project inner-attributes (join tuple relation))))
		    projected))))
  (:method ((relation relation) (by-attributes list) (new-attribute symbol) &key invert)
    (group relation (convert 'set by-attributes) new-attribute :invert invert)))

(test group
  (let ((r (relation (a b c)
		      (1 2 3)
		      (4 5 6)
		      (7 8 3)
		      (1 2 4))))
    
    (is (same (relation (c g)
			(3 (relation (a b)
				     (1 2)
				     (7 8)))
			(4 (relation (a b)
				     (1 2)))
			(6 (relation (a b)
				     (4 5))))
	      (group r '(c) 'g)))

    (is (same (relation (a b g)
			(1 2 (relation (c)
				       (3)
				       (4)))
			(4 5 (relation (c)
				       (6)))
			(7 8 (relation (c)
				       (3))))
	      (group r '(c) 'g :invert t))))

  ;; (let ((r (relation (a b c d)
  ;; 		     (1 2 3 6)
  ;; 		     (4 5 6 5)
  ;; 		     (7 8 3 6)
  ;; 		     (1 2 4 4))))
  ;;   (is (same (relation (c d g)
  ;; 			(3 6 (relation (a b)
  ;; 				       (1 2)
  ;; 				       (7 8)))
  ;; 			(6 5 (relation (a b)
  ;; 				       (4 5)))
  ;; 			(4 4 (relation (a b)
  ;; 				       (1 2))))
  ;; 	      (group r '(c) 'g :group '(a b c)))))
  )

(defgeneric wrap (attributed attributes into-attribute)
  (:method ((attributed t) (attributes t) (into-attribute symbol))    
    (wrap attributed (convert 'set attributes) into-attribute))
  (:method ((tuple wb-map) (attributes set) (into-attribute symbol))
    (let ((new-tuple (project attributes tuple :invert t))
	  (wrapped (project attributes tuple)))
      (setf (tref into-attribute new-tuple) wrapped)
      new-tuple))
  (:method ((relation relation) (attributes set) (into-attribute symbol))
    (map-relation
     (lambda (tuple)
       (wrap tuple attributes into-attribute))
     relation)))

(test wrap
  (let ((tuple (tuple (a 1) (b 2) (c 3) (d 4))))
    (is (same (tuple (a 1) (b 2) (x (tuple (c 3) (d 4))))
	      (wrap tuple '(c d) 'x))))
  
  (let ((r (relation (a b c d)
		     (1 2 3 4)
		     (2 3 4 5))))
    (is (same (relation (a x)
			(1 (tuple (b 2) (c 3) (d 4)))
			(2 (tuple (b 3) (c 4) (d 5))))
	      (wrap r '(b c d) 'x)))

    ))

(defgeneric unwrap (attributed from-attribute)
  (:method ((tuple wb-map) (from-attribute symbol))
    (let ((new-tuple (project (set from-attribute) tuple :invert t))
	  (to-unwrap (tref from-attribute tuple)))
      (if to-unwrap
	  (join new-tuple to-unwrap)
	  new-tuple)))
  (:method ((relation relation) (from-attribute symbol))
    (map-relation (lambda (tuple) (unwrap tuple from-attribute)) relation)))

(test unwrap
  (let ((tuple (tuple (a 1) (b 2) (x (tuple (c 3) (d 4))))))
    (is (same (tuple (a 1) (b 2) (c 3) (d 4))
	      (unwrap tuple 'x))))

  (let ((r (relation (a x)
		     (1 (tuple (b 2) (c 3) (d 4)))
		     (2 (tuple (b 3) (c 4) (d 5))))))
    (is (same (relation (a b c d)
			(1 2 3 4)
			(2 3 4 5))
	      (unwrap r 'x)))))
