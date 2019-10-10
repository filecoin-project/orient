(in-package filecoin.theory)

(defparameter *n* 16)
(defparameter *p* (random-perm *n*))
(defparameter *q* (random-perm *n*))

(defun show-perm (perm)
  (list (perm-to-list (perm-identity (perm-size perm))) (perm-to-list perm)))

(defun node-list (&optional (n *n*))
  (loop for i from 1 to n collect i))

(defun nodes (n)
  (apply #'vector (node-list n)))

(defun parents (i perms)
  (remove-duplicates
   (sort (remove-if (partial #'<= i)
		    (mapcar (lambda (perm)
			      (perm-eval perm i))
			    perms))
	 #'<)))

(defun compute-parents (node-count perm)
  (loop for i from 1 to node-count
     for j = (perm-eval perm  i)
     if (< j i) collect j else collect ""))

(defun compute-all-parents (node-count perms)
  (loop for i from 1 to node-count
       collect (parents i perms)))

(defparameter *nodes* (nodes 16))

;;; Org-mode output

(defun parents-line (name parents)
  `(,name ,(length (remove "" parents :test #'equal)) ,@parents))

(defun self-line (name perm)
  `(,name ,(perm-size perm) ,@(perm-to-list perm)))

(defun all-parents-line (name all-parents)
  (let ((total (loop for parents in all-parents summing (length parents))))
    `(,name ,total ,@(substitute "" nil all-parents))))

(defun make-perm-table (node-count)
  (let* ((p (random-perm node-count))
	 (p-inv (perm-inverse p))
	 (q (random-perm node-count)))
    (list (list* :nodes node-count (node-list node-count))
	  (self-line :p-perm p)
	  (parents-line :p (compute-parents node-count p))
	  (parents-line :p-inv (compute-parents node-count p-inv))
	  (parents-line :q (compute-parents node-count q))
	  (all-parents-line :p+p-inv (compute-all-parents node-count (list p p-inv)))
	  (all-parents-line :p+q (compute-all-parents node-count (list p q))))))

(defun compare-perms (node-count iterations)
  (let ((p+p-inv-total 0)
	(p+q 0)
	(scoreboard 0))
    (loop for i below iterations
       for all = (make-perm-table node-count)
       for pp = (second (nth 5 all))
       for pq = (second (nth 6 all))
       do (incf p+p-inv-total pp)
       do (incf p+q pq)
       if (> pp pq) do (decf scoreboard)
       if (< pp pq) do (incf scoreboard))
    (values p+p-inv-total p+q scoreboard (- p+p-inv-total p+q))))

(defun mean (vals)
  (/ (reduce #'+ vals) (length vals)))

(defun variance (vals)
  (let ((m (mean vals)))
    (values (float (/ (loop for v in vals
			 summing (expt (- v m) 2))
		      m))
	    (float m))))

(defun parent-count-variance (node-count perms-list)
  (let ((totals (loop for perms in perms-list
		   for all-parents = (compute-all-parents node-count perms)
		   for parent-counts = (mapcar (lambda (x) (aif x (length x) 0)) all-parents)
		   collect (reduce #'+ parent-counts))))
    (variance totals)))

(defun compare-parent-count-variance (node-count trials)
  (let* ((p+p-inv-list (loop for i below trials collect (let ((p (random-perm node-count)))
							      (list p (perm-inverse p)))))
	 (p+q-list (loop for i below trials collect (list (random-perm node-count) (random-perm node-count))))
	 (p+p-inv-variance (multiple-value-list  (parent-count-variance node-count p+p-inv-list )))
	 (p+q-variance (multiple-value-list (parent-count-variance node-count p+q-list))))
    `((:p+p-inv-variance ,p+p-inv-variance)
      (:p+q-variance ,p+q-variance))))

(defparameter *default-graph-nodes* 10)

(defclass layer-graph-mixin ()
  ((layer :initarg :layer :initform 0 :accessor layer-graph-layer)
   (nodes :initarg :nodes :initform  *default-graph-nodes* :accessor layer-graph-nodes)
   (challenged-node :initarg :challenged-node :accessor layer-graph-challenged-node)
   (parent :initarg :parent :accessor layer-graph-parent)))

(defclass comm-d-layer-graph (layer-graph-mixin) ())

(defun make-comm-d-layer-graph (nodes challenged-node &key parent)
  (make-instance 'comm-d-layer-graph :layer 0 :nodes nodes :challenged-node challenged-node :parent parent))

(defclass replica-layer-graph (layer-graph-mixin) ())

(defun make-replica-layer-graph (nodes challenged-node &key parent layers)
  (make-instance 'replica-layer-graph :layer (1+ layers)  :nodes nodes :challenged-node challenged-node :parent parent))


(defclass layer-graph (layer-graph-mixin)
  ((reversed :initarg :reversed :initform nil :accessor layer-graph-reversed)
   (renumbered-parents :initarg :renumbered-parents :accessor layer-graph-renumbered-parents)
   (reversed-parents :initarg :reversed-parents :accessor layer-graph-reversed-parents)
   (renumbered-permutation :initarg :renumbered-permutation :accessor layer-graph-renumbered-permutation)
   (reversed-permutation :initarg :reversed-permutation :accessor layer-graph-reversed-permutation)))

(defun make-layer-graph (nodes &key layer reversed (reversed-permutation (random-perm nodes)) (renumbered-permutation (random-perm nodes))
                                 challenged-node parent (reversed-parents #'simple-reversed-parents) (renumbered-parents #'simple-renumbered-parents)
                                 &allow-other-keys)
  (make-instance 'layer-graph
		 :nodes nodes
		 :layer layer
		 :reversed reversed
                 :challenged-node challenged-node
                 :parent parent
		 :renumbered-permutation renumbered-permutation
		 :reversed-permutation reversed-permutation
		 :renumbered-parents renumbered-parents
		 :reversed-parents reversed-parents))

(defclass zigzag-graph ()
  ((nodes :initarg :nodes :initform  *default-graph-nodes* :accessor zigzag-graph-nodes)
   (challenged-node :initarg :challenged-node :accessor zigzag-graph-challenged-node)
   (layer-graphs :initarg :layer-graphs :initform '() :accessor zigzag-graph-layer-graphs)
   (renumbered-permutation :initarg :renumbered-permutation :accessor zigzag-graph-renumbered-permutation)
   (reversed-permutation :initarg :reversed-permutation :accessor zigzag-graph-reversed-permutation)))

(defclass sdr-graph ()
  ((nodes :initarg :nodes :initform  *default-graph-nodes* :accessor sdr-graph-nodes)
   (challenged-node :initarg :challenged-node :accessor sdr-graph-challenged-node)
   (layer-graphs :initarg :layer-graphs :initform '() :accessor sdr-graph-layer-graphs)
   (renumbered-permutation :initarg :renumbered-permutation :accessor sdr-graph-renumbered-permutation)
   (reversed-permutation :initarg :reversed-permutation :accessor sdr-graph-reversed-permutation)))

(defun make-zigzag-graph (nodes layers &key (reversed-permutation (random-perm nodes)) (renumbered-permutation (random-perm nodes)) challenged-node
                                         &allow-other-keys)
  (loop
     (let* ((layer-graphs (loop for i from 1 to layers
                             collect (make-layer-graph nodes                                                       
						      :layer i
						      :reversed (evenp i)
                                                      :challenged-node challenged-node
						      :renumbered-permutation renumbered-permutation
						      :reversed-permutation (when (not (= i 1)) reversed-permutation))))
	    (zigzag-graph (make-instance 'zigzag-graph
					 :nodes nodes
					 :layer-graphs layer-graphs
                                         :challenged-node challenged-node
					 :renumbered-permutation renumbered-permutation
					 :reversed-permutation reversed-permutation)))
       (dolist (lg layer-graphs)
         (setf (layer-graph-parent lg) zigzag-graph))
       (when (or challenged-node (choose-and-set-challenged-node zigzag-graph))
	 (return zigzag-graph))
       (setq reversed-permutation (random-perm nodes)
	     renumbered-permutation (random-perm nodes)))))

(defun make-sdr-graph (nodes layers &key (reversed-permutation (random-perm nodes)) (renumbered-permutation (random-perm nodes)) challenged-node
                                      &allow-other-keys)
  (loop
     (let* ((layer-graphs (loop for i from 1 to (1+ layers)
                             collect (make-layer-graph nodes
                                                       :layer i
                                                       :reversed nil
                                                       :challenged-node challenged-node
                                                       :renumbered-permutation renumbered-permutation
                                                       :reversed-permutation (when (not (= i 1)) reversed-permutation)
                                                       :reversed-parents #'challenged-reversed-parents
                                                       :renumbered-parents #'challenged-renumbered-parents)))
	    (sdr-graph (make-instance 'sdr-graph
					 :nodes nodes
					 :layer-graphs layer-graphs
                                         :challenged-node challenged-node
					 :renumbered-permutation renumbered-permutation
					 :reversed-permutation reversed-permutation)))
       (dolist (lg layer-graphs)
         (setf (layer-graph-parent lg) sdr-graph))
       (when (or challenged-node (choose-and-set-challenged-node sdr-graph))
	 (return sdr-graph))
       (setq reversed-permutation (random-perm nodes)
	     renumbered-permutation (random-perm nodes)))))

(defgeneric dump (thing) (:method-combination append)
  (:method append ((thing t)) `(:type ,(type-of thing)))
  (:method append ((graph zigzag-graph))
	   (list :nodes (zigzag-graph-nodes graph)
		 :challenged-node (zigzag-graph-challenged-node graph)
		 :layers (length (zigzag-graph-layer-graphs graph))
		 :renumbered-permutation (dump (zigzag-graph-renumbered-permutation graph))
		 :reversed-permutation (dump (zigzag-graph-reversed-permutation graph))))
    (:method append ((graph sdr-graph))
	   (list :nodes (sdr-graph-nodes graph)
		 :challenged-node (sdr-graph-challenged-node graph)
		 :layers (1- (length (sdr-graph-layer-graphs graph)))
		 :renumbered-permutation (dump (sdr-graph-renumbered-permutation graph))
		 :reversed-permutation (dump (sdr-graph-reversed-permutation graph))))
  (:method append ((perm perm))
	   `(:perm-list ,(perm-to-list perm))))

(defgeneric %load (type plist)
  (:method ((type (eql 'perm)) (plist list))
    (list-to-perm (getf plist :perm-list)))
  (:method ((type (eql 'zigzag-graph)) (plist list))
    (make-zigzag-graph (getf plist :nodes plist)
		       (getf plist :layers plist)
                       :challenged-node (getf plist :challenged-node)
		       :reversed-permutation (load-from-plist (getf plist :reversed-permutation))
		       :renumbered-permutation (load-from-plist (getf plist :renumbered-permutation))))
  (:method ((type (eql 'sdr-graph)) (plist list))
    (make-sdr-graph (getf plist :nodes plist)
                    (getf plist :layers plist)
                    :challenged-node (getf plist :challenged-node)
                    :reversed-permutation (load-from-plist (getf plist :reversed-permutation))
                    :renumbered-permutation (load-from-plist (getf plist :renumbered-permutation)))))

(defun load-from-plist (plist)
  (%load (getf plist :type) plist))

(defclass legend () ((parity :initarg :parity :initform :odd :accessor legend-parity)))

(defmethod cl-dot:graph-object-node ((graph legend) node)
  (destructuring-bind (shape color label) (ecase node
					    (:challenged (list :hexagon "green" "Challenge"))
					    (:parent (list :ellipse "black" "Parent"))
					    (:reversed-parent (list :octagon "blue" (format nil "~:(~A~) Expander Parent" (or (legend-parity graph) ""))))					    (:renumbered-parent (list :box "red" (format nil "~:(~A~) DRG Parent" (or (legend-parity graph) "")))))
    (make-instance 'cl-dot:node :attributes (list :label label :shape shape :color color))))

(defmethod cl-dot:graph-object-edges ((graph legend))
  `((:reversed-parent :challenged (:color "blue"))
    (:parent :challenged (:color "red"))    
    (:renumbered-parent :challenged (:color "red"))))

(defgeneric choose-and-set-challenged-node (graph)
  (:documentation "Choose a suitable challenged node and possibly generate new permutations, returning challenged-node index or NIL if no suitable choice was found. Calling again on returned graph is deterministic and stable (won't allocate if called on previous return value).")
  (:method ((zigzag-graph zigzag-graph))
    (let ((challenged-node nil)
          (forward-layer (third (zigzag-graph-layer-graphs zigzag-graph)))
          (reversed-layer (second (zigzag-graph-layer-graphs zigzag-graph))))
      (loop for i below (zigzag-graph-nodes zigzag-graph)
         until (and (has-all-parents-p forward-layer i)
                    (has-all-parents-p reversed-layer (renumber reversed-layer i))
                    (not (nodes-overlap-p forward-layer reversed-layer i (renumber reversed-layer i)))
                    (setf challenged-node i)))
      (awhen challenged-node
        (setf (zigzag-graph-challenged-node zigzag-graph) it)
        (loop for layer-graph in (zigzag-graph-layer-graphs zigzag-graph)
           do (setf (layer-graph-challenged-node layer-graph) (maybe-renumber layer-graph it)))
        it)))

  (:method ((sdr-graph sdr-graph))
    (let ((challenged-node nil)
          (forward-layer (third (sdr-graph-layer-graphs sdr-graph))))
      (loop for i below (sdr-graph-nodes sdr-graph)
         until (and (has-all-parents-p forward-layer i)
                    (setf challenged-node i)))
      (awhen challenged-node
        (setf (sdr-graph-challenged-node sdr-graph) it)
        (loop for layer-graph in (sdr-graph-layer-graphs sdr-graph)
           do (setf (layer-graph-challenged-node layer-graph) it))
        it))))

(defmethod has-all-parents-p ((graph layer-graph) (index integer))
  (and (simple-renumbered-parents graph index)
       (simple-reversed-parents graph index)))

(defun disjointp (&rest lists)
  (= (reduce #'+ (mapcar #'length lists))
     (length (reduce #'union lists))))

(defmethod nodes-overlap-p ((graph layer-graph) (other-graph layer-graph) (index integer) (renumbered-index integer))
  (not (disjointp (list index)
		  (list renumbered-index)	     
		  (simple-renumbered-parents graph index)
		  (simple-reversed-parents graph index)
		  (simple-renumbered-parents other-graph renumbered-index)
		  (simple-reversed-parents other-graph renumbered-index)
		  (mapcar (lambda (x) (renumber other-graph x))
			  (simple-reversed-parents other-graph renumbered-index))
		  )))


(defun classify (graph i)
  (cond
    ((= i (layer-graph-challenged-node graph)) :challenged)
    ((reversed-parent-p graph (layer-graph-challenged-node graph) i) :reversed-parent)
    ((renumbered-parent-p graph (layer-graph-challenged-node graph) i) :renumbered-parent)
    (t t)))

(defgeneric node-label (graph i &key format annotate classification)
  (:method ((graph layer-graph-mixin) (i integer) &key (format :latex) (annotate nil) classification)
    (let* ((layer (layer-graph-layer graph))
           (effective-layer (if (and (eql classification :reversed-parent)
                                    (typep (layer-graph-parent graph) 'sdr-graph))
                                (1- layer)
                               layer)))
      (case format
        (:latex (format nil "$e_~D^{(~D)}~@[~A~]$" i effective-layer (when annotate "^{*}")))
        (:plain (format nil "(~D, ~D)~@[~a~]" effective-layer i (when annotate "<sup>*</sup>")))
        (:simple (format nil "$(~D, ~D)~@[~a~]$" effective-layer i (when annotate "^{*}")))
        (:both (format nil "~a = ~a" (node-label graph i :format :latex) (node-label graph i :format :plain)))))))

(defmethod cl-dot:graph-object-node ((graph comm-d-layer-graph) i)
  (make-instance 'cl-dot:node
		 :attributes (list :label (node-label graph i :format :plain)
				   :shape (if (= i (layer-graph-challenged-node graph))
					      :hexagon
					      :ellipse)
				   :color (if (= i  (layer-graph-challenged-node graph))
					      "green"
					      "black"))))

(defmethod cl-dot:graph-object-node ((graph replica-layer-graph) i)
  (make-instance 'cl-dot:node
		 :attributes (list :label (node-label graph i :format :plain)
				   :shape (if (= i (layer-graph-challenged-node graph))
					      :hexagon
					      :ellipse)
				   :color (if (= i  (layer-graph-challenged-node graph))
					      "green"
					      "brown"))))

(defmethod cl-dot:graph-object-node ((graph layer-graph) i)  
  (make-instance 'cl-dot:node
		 :attributes (list :label (node-label graph i :format :plain :classification (classify graph i))
				   :shape (case (classify graph i)
					    (:challenged :hexagon)
					    (:reversed-parent :octagon)
					    (:renumbered-parent :box)
					    (t :ellipse))
				   :color (case (classify graph i)
					    (:challenged "green")
					    (:reversed-parent "blue")
					    (:renumbered-parent "red")
					    (t "black")))))

(defmethod firstp ((graph layer-graph) (node integer))
  (if (layer-graph-reversed graph)
      (= node (layer-graph-nodes graph))
      (= node 1)))

(defmethod direct-parent ((graph layer-graph) (node integer))
  (if (layer-graph-reversed graph)
      (1+ node)
      (1- node)))

(defmethod cl-dot:graph-object-edges ((graph comm-d-layer-graph))
  (loop for n from 2 to (layer-graph-nodes graph)
     collect (list (1- n) n `(:weight ,(+ 1000 n) :arrowhead :none :color "white"))))

(defmethod cl-dot:graph-object-edges ((graph replica-layer-graph))
  (loop for n from 2 to (layer-graph-nodes graph)
     collect (list (1- n) n `(:weight ,(+ 1000 n) :arrowhead :none :color "white"))))

(defmethod cl-dot:graph-object-edges ((graph layer-graph))
  (let* ((challenged-reversed-parents (funcall (layer-graph-reversed-parents graph) graph (layer-graph-challenged-node graph)))
         (following-reversed (mapcar #'1+ challenged-reversed-parents)))
    (loop for n from 1 to (layer-graph-nodes graph)
       for renumbered-parents = (funcall (layer-graph-renumbered-parents graph) graph n)
       for reversed-parents = (funcall (layer-graph-reversed-parents graph) graph n)
       unless (firstp graph n) collect (list (direct-parent graph n) n `(:weight ,(+ 1000 n)
                                                                                 ,@(if (and (typep (layer-graph-parent graph) 'sdr-graph)
                                                                                            (member n following-reversed))
                                                                                       (list :color "pink")
                                                                                       (list :color "red"))))
       append (loop for p in renumbered-parents collect (list p n (list :color "red")))
       append (loop for p in reversed-parents collect (list p n (list :color "blue"))))))

(defun maybe-graph-parent? (graph i candidate)
  "Returns true if candidate can be a computed parent of i. This excludes the direct predecessor (which will be added to the graph separately)."
  (if (layer-graph-reversed graph)
      (and (> candidate i)
	   (/= candidate (1+ i)))
      (and (< candidate i)
	   (/= candidate (1- i)))))

(defun renumber (graph i)
  (1+ (- (layer-graph-nodes graph) i)))

(defun maybe-renumber (graph i)
  (if (layer-graph-reversed graph)
      (renumber graph i)
      i))

(defun reversed-parent-p (graph i j)
  "Is J a reversed-parent of I in graph, GRAPH?"  
  (when (and (layer-graph-reversed-permutation graph)
	     (maybe-graph-parent? graph i j))
    (when (layer-graph-reversed graph)
      (psetq i j j i))
    (= (perm-eval (layer-graph-reversed-permutation graph) i) j)))

(defun simple-reversed-parents (graph x)
  (loop for i from 1 to (layer-graph-nodes graph)
     if (reversed-parent-p graph x i)
     collect (identity i)))

(defun challenged-reversed-parents (graph x)
  (when (eql x (layer-graph-challenged-node graph))
    (loop for i from 1 to (layer-graph-nodes graph)
       if (reversed-parent-p graph x i)
       collect (identity i))))

(defun renumbered-parent-p (graph i j)
  "Is J a renumbered-parent of I in graph, GRAPH?"
  (and (maybe-graph-parent? graph i j)
       (= (maybe-renumber graph (perm-eval (layer-graph-renumbered-permutation graph) (maybe-renumber graph i))) j)))

(defun simple-renumbered-parents (graph x)
  (loop for i from 1 to (layer-graph-nodes graph)
     if (renumbered-parent-p graph x i)
     collect (identity i)))

(defun challenged-renumbered-parents (graph x)
  (when (eql x (layer-graph-challenged-node graph))
    (loop for i from 1 to (layer-graph-nodes graph)
       if (renumbered-parent-p graph x i)
       collect (identity i))))

(defun emit-comm-d-layer-graph (layer-graph)
  (cl-dot:print-graph (cl-dot:generate-graph-from-roots layer-graph '()
							`(:rankdir "LR"))))

(defun emit-replica-layer-graph (layer-graph)
  (cl-dot:print-graph (cl-dot:generate-graph-from-roots layer-graph '()
							`(:rankdir "LR"))))

(defun emit-layer-graph (layer-graph)
  (cl-dot:print-graph (cl-dot:generate-graph-from-roots layer-graph '()
							`(:rankdir ,(if (layer-graph-reversed layer-graph) "RL" "LR")))))

(defun emit-legend (&optional (parity :odd))
  (let ((legend (make-instance 'legend :parity parity)))
    (cl-dot:print-graph (cl-dot:generate-graph-from-roots legend '()
							  `(:rankdir ,(case (legend-parity legend)
									((:odd nil) "LR")
									(:even "RL")))))))

(defgeneric columns (graph &key parity)
  (:method ((graph zigzag-graph) &key parity)
    (let ((third-graph (third (zigzag-graph-layer-graphs graph)))
	  (second-graph (second (zigzag-graph-layer-graphs graph)))
	  (odd-expander-index)
	  (even-expander-index))
      (list*
       (setq *dval* (loop for i from 1 to (layer-graph-nodes third-graph)
		       collect (ecase (classify third-graph i)
				 (:challenged "Challenges")
				 (:renumbered-parent "DRG Parents")
				 (:reversed-parent
				  (setq odd-expander-index i)
				  "Odd Expander Parents")
				 (t (case (classify second-graph (renumber second-graph i))
				      (:reversed-parent
				       (setq even-expander-index i)
				       "Even Expander Parents")
				      (t "~~~~~~"))))))

       (loop for graph in (butlast (zigzag-graph-layer-graphs graph))
	  for l from 1
	  unless (case parity
		   (:odd  (layer-graph-reversed graph))
		   (:even (not (layer-graph-reversed graph))))
	    
	  collect (loop for i from 1 to (layer-graph-nodes graph)
		     collect (node-label graph (maybe-renumber graph i)
					 :format :simple
					 :annotate (or (and (= l 1)
							    ;; First layer does not use expander parents.
							    (eql i odd-expander-index))
						       (eql i (if (layer-graph-reversed graph)
								  odd-expander-index
								  even-expander-index)))))))))
  (:method ((graph sdr-graph) &key parity)
    (let ((third-graph (third (sdr-graph-layer-graphs graph)))
	  (odd-expander-index)
	  (even-expander-index))
      (list*
       (setq *dval* (loop for i from 1 to (layer-graph-nodes third-graph)
		       collect (ecase (classify third-graph i)
				 (:challenged "Challenges")
				 (:renumbered-parent "DRG Parents")
				 (:reversed-parent
				  (setq odd-expander-index i)
				  "Expander Parents")
                                 (t "~~~~~~"))))

       (loop for graph in (butlast (sdr-graph-layer-graphs graph))
	  for l from 1	    
	  collect (loop for i from 1 to (layer-graph-nodes graph)
		     collect (node-label graph (maybe-renumber graph i)
					 :format :simple
					 :annotate (or (and (= l 1)
							    ;; First layer does not use expander parents.
							    (eql i odd-expander-index))
						       (eql i (if (layer-graph-reversed graph)
								  odd-expander-index
								  even-expander-index))))))))))

(defgeneric final-layer (graph)
  (:method ((graph zigzag-graph))
    (let ((last-graph (car (cl:last (zigzag-graph-layer-graphs graph)))))
      (list
       (loop for i from 1 to (layer-graph-nodes last-graph)
          collect (case (classify last-graph i)
                    (:challenged "Even Challenge")
                    (:reversed-parent "Even Expander Parent")
                    (:renumbered-parent "Even DRG Parent")
                    (t "~~~~~~")))
       (loop for i from 1 to (layer-graph-nodes last-graph)
          collect (node-label last-graph i :format :simple)))))
  (:method ((graph sdr-graph))
    (let ((last-graph (car (cl:last (sdr-graph-layer-graphs graph)))))
      (list
       (loop for i from 1 to (layer-graph-nodes last-graph)
          collect (case (classify last-graph i)
                    (:challenged "Challenge")
                    (t "~~~~~~")))
       (loop for i from 1 to (layer-graph-nodes last-graph)
          collect (node-label last-graph i :format :simple))))))

(defmethod initial-layer ((graph comm-d-layer-graph))
  (list
   (loop for i from 1 to (layer-graph-nodes graph)
      collect (if (= i (layer-graph-challenged-node graph))
		  (typecase (layer-graph-parent graph)
                    (zigzag-graph "(Odd) Challenge")
                    (t "Challenge"))
		  "~~~~~~"))
   (loop for i from 1 to (layer-graph-nodes graph)
      collect (node-label graph i :format :simple))))

(defun notation-table (graph)
  (loop for layer in (zigzag-graph-layer-graphs graph)
       for j from 1 
     collect (loop for i from 1 upto (zigzag-graph-nodes graph)
		collect (node-label layer i :format :simple))
     collect (loop for i from 1 upto (zigzag-graph-nodes graph)
		collect (node-label layer i :format :latex))))

(defgeneric notation-row (graph layer)
  (:method ((graph zigzag-graph) (layer number))
    (let ((layer (nth (1- layer) (zigzag-graph-layer-graphs graph))))
      (list (cons "Graph" (loop for i from 1 upto (zigzag-graph-nodes graph)
                             collect (node-label layer i :format :simple)))
            (cons "Notation" (loop for i from 1 upto (zigzag-graph-nodes graph)
                                collect (node-label layer i :format :latex))))))
  (:method ((graph sdr-graph) (layer number))
    (let ((layer (nth (1- layer) (sdr-graph-layer-graphs graph))))
      (list (cons "Graph" (loop for i from 1 upto (sdr-graph-nodes graph)
                             collect (node-label layer i :format :simple)))
            (cons "Notation" (loop for i from 1 upto (sdr-graph-nodes graph)
                                collect (node-label layer i :format :latex)))))))
