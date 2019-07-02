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

(defgeneric project (attributes attributed)
  (:method ((attributes list) (attributed t))
    (project (convert 'set attributes) attributed))
  (:method ((attributes set) (null null))
    nil)
  (:method ((attributes set) (tuple wb-map))
    (fset:restrict tuple attributes))
  (:method ((attributes set) (relation relation))
    (make-relation
     (image (lambda (tuple) (project attributes tuple))
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

