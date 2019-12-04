(defpackage orient.web
  (:use :common-lisp :orient :filecoin :orient.web.html :orient.base.util :it.bese.FiveAm)
  (:import-from :fset :wb-map :image :filter :contains? :convert)
  (:shadowing-import-from :fset :reduce  :union)
  (:nicknames :web)
  (:export :serve-report-page :start-web :stop-web :report-page-for *orient-web-port* *acceptor*))

(in-package :orient.web)
(def-suite web-suite)
(in-suite web-suite)

(defparameter *orient-web-port* 8888)

(eval-when
    (:compile-toplevel :load-toplevel :execute)
  (defvar *calculation-pages* '())

  (defstruct calc-page name title base-uri)

  (defun register-calc-page (name title base-uri)
    (pushnew (make-calc-page :name name :title title :base-uri base-uri) *calculation-pages* :key #'calc-page-name)))

(defvar *acceptor*)

(defun start-web (&key (port *orient-web-port*))
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor :port port)))
    (prog1 (hunchentoot:start acceptor)
      (setq *acceptor* acceptor))))

(defun stop-web ()
  (hunchentoot:stop *acceptor*))

(defmacro with-page ((title-var) &body body)
  `(let ((*package* (find-package :filecoin))
	 (title ,title-var))
     (when (boundp '*acceptor*) (setf (hunchentoot:content-type*) "text/html"))
     (with-output-to-string (*html-output-stream*)
       (html
	(:html
	 (:head (:title title))
	 (:body
	  (when title
	    `(:div (:h1 ,title)
		   ((:a href "/") "INDEX")))
	  ,@body))))))

(defmacro with-report-page ((title &key vars system initial-data override-data) &body body)
  `(serve-report-page :title ,title
		      :vars ',vars
		      :system ,system
		      :initial-data ,initial-data
		      :override-data ,override-data
		      :body-html (with-output-to-string (*html-output-stream*)
				   (html ,@body))))

(defun report-page (&key vars system initial-data override-data project-solution)
  (multiple-value-bind (report solution defaulted-data plan)
      (report-solution-for vars :system system :initial-data initial-data :format :html :override-data override-data :project-solution project-solution
			   :return-plan t :return-defaulted-data t)
    (let* ((signature (pipeline-signature plan))
	   (parameters (and signature (union (signature-input signature) (signature-output signature)))))
      (declare (ignore parameters)) ;; TODO: turn parameters into links in solution.
      `(:div
	,@(when override-data
	    (list `(:div (:p ,(format nil "Supplied: ~s" override-data))
			 (:hr))))
	((:div :style "background-color:lightgrey")
	 (:p (:h3 "Solution ") ,(present-data :html solution system :alt-bgcolor "white"))
	 (:hr))
	((:div :style "background-color:lightblue")
	 (:p (:h3 "Initial data ") ,(present-data :html defaulted-data system :alt-bgcolor "white"))
	 (:hr))
	((:div :style "background-color:lightyellow")
	 (:p  ,report)
	 (:hr))))))

(defun serve-report-page (&key title vars system initial-data override-data body-html project-solution)
  (with-page (title)
    body-html
    (:p "Solving for " (if vars (comma-list vars) "all vars") ".")
    (:hr)
    (:pre
     (report-page :vars vars :system system :initial-data initial-data :override-data override-data :project-solution project-solution))))

#+(or)
(defmethod synthesize-report-steps ((format (eql :html)) (steps list))
  (with-output-to-string (*html-output-stream*)
    (html-print
     (if (and (cdr steps) (car steps) steps)
	 `((:table :border 1)
	   "xxx"
	   ,@(loop for step in steps
		collect '(:tr (:td "yyy"))
		collect `(:tr (:td (:div ,@(loop for s in step collect s collect :hr))))
		collect '(:tr (:td))))
	 `(:div
	   ,@(loop for step in (car steps)
		collect step))))))

(defmethod synthesize-report-steps ((format (eql :html)) (steps list))
  (with-output-to-string (*html-output-stream*)
    (html-print
     `(:div
       ,@(loop for step in steps
	    if (cdr step)
	    collect `((:table :border 1) (:tr (:td (:div ,@(loop for s in step collect s collect :hr)))))

	    else
	    collect `(:div
		      ,@(loop for step in (car steps)
			   collect step
			   collect :hr)))))))

(defmethod synthesize-report-steps ((format (eql :html)) (steps list))
  (with-output-to-string (*html-output-stream*)
    (html-print
     `(:div
       ,@(loop for step in steps
	    if (cdr step)
	    collect `((:table :border 1) (:tr (:td (:div ,@(loop for s in step collect s collect :hr)))))

	    else
	    collect `(:div
		      ,@(loop for step in step
			   collect step
			   collect :hr)))))))

(defmethod format-value ((format (eql :html)) (value t))
  (if (keywordp value)
      `(:html-escape ,(format nil "~S" value))
      `(:html-escape ,(or value "FALSE"))))

(defmethod present-data ((format (eql :html)) (null null) (system system) &key)
   "NULL")

(defmethod present-data ((format (eql :html)) (thing t) (null null) &key)
  (format-value format thing))

(defmethod present-data ((format (eql :html)) (tuple wb-map) (system system) &key alt-bgcolor use-alt)
  (cons
   :html-escape
   (cons
    `(:div ,@(when (and use-alt alt-bgcolor)
	       `(:style ,(format nil "background-color:~A" alt-bgcolor))))
    (loop for attr in (sort (convert 'list (attributes tuple)) #'string-lessp)
       for desc = (lookup-description attr system)
       collect `(:div
		 ((:a :name ,(symbol-name attr)) (:b ,(symbol-name attr)))
		 " = "
		 ,(format nil "type: ~a " (type-of (tref attr tuple)))
		 ((:font :color "blue") ,(present-data format (tref attr tuple) nil))
		 ,(aif (and desc (not (equal desc "")))
		       `(:div "     " ((:font :color "green") (:i ,desc)))
					;'(:span "     XXXXXXXXXXXXX-DESCRIPTION MISSING-XXXXXXXXXXXXX")
		       )
		 :hr)))))

(defmethod present-data ((format (eql :html)) (list list) (system null) &key)
  (loop for elt in list
     collect (present-data format elt nil)))

(defmethod present-data ((format (eql :html)) (set fset:set) (system null) &key)
  (present-data format
		(convert 'list set) system))

(defmethod present-data ((format (eql :html)) (relation relation) (system system) &key alt-bgcolor)
  `(:div ,@(loop for
	      tuple in (convert 'list (tuples relation))
	      for i from 0
	      collect (present-data format tuple system :alt-bgcolor alt-bgcolor :use-alt (oddp i)))))


(defmethod present-data ((format (eql :html)) (relation relation) (system null) &key alt-bgcolor)
  (let ((columns (convert
		  'list (attributes relation))))
    `(:div
      ((:table :border 1)
       (:tr ,@(loop for attr in columns
		 collect `(:th ',attr))
	    ,@(loop for tuple in (convert 'list (tuples relation))
		 collect `(:tr ,@(loop for attr in columns
				    collect `(:td ,(present-data format (tref attr tuple) nil))))))))))

(defmethod create-tuple-report-step ((format (eql :html)) (tuple wb-map) (transformation transformation) (system system) &key n)
  (declare (ignore n))
  (reduce (lambda (acc attr val)
	    (if (private-attr-p attr)
		acc
		(cons `(:html-escape
			((:a :name ,(symbol-name attr)) (:b ,(symbol-name attr)))
			": "
			,(awhen (describe-transformation-calculation transformation)
			   `((:font :color "red") ,it))
			" = "
			((:font :color "blue") ,(present-data format (format-value format val) nil))
			,(let ((desc (and system (lookup-description attr system))))
			   (aif (and desc (not (equal desc "")))
				`(:div "     " ((:font :color "green") (:i ,desc)))
					; '(:div "     XXXXXXXXXXXXX-DESCRIPTION MISSING-XXXXXXXXXXXXX")
				)))
		      acc)))
	  (filter (lambda (attr val)
		    (declare (ignore val))
		    (contains? (signature-output (transformation-signature transformation)) attr))
		  tuple)
	  :initial-value '()))

(defmacro define-calculation-pages ((base-name &key uri title vars system initial-data override-parameters) (&rest parameters)
				    &body body)
  (let* ((graph-uri (format nil "~A-graph" uri))
	 (graph-name (symbolconc base-name '-graph))
	 (test-name (symbolconc 'test- base-name '-web))
	 (graph-namestring (symbol-name graph-name)))
    `(eval-when
	 (:compile-toplevel :load-toplevel :execute)
       (register-calc-page ',base-name  ,title ,uri)

       (hunchentoot:define-easy-handler (,base-name :uri ,uri) ,parameters
	 (with-report-page (,title
			    :vars ,vars
			    :system ,system
			    :initial-data ,initial-data
			    :override-data (make-override-data (list ,@(loop for parameter in override-parameters
									  collect `(list ',parameter  ,parameter)))))
	   (:div
	    (:p ,@body)
	    (:p ((:a :href ,graph-uri) "See a Graph"))
	    )))

       (hunchentoot:define-easy-handler (,graph-name :uri ,graph-uri) ()
	 (serve-graph (plan-for ,system ',vars ,initial-data)
		      ,graph-namestring
		      :base-url ,uri))

       (test ,test-name
	 (finishes
	   (report-solution-for ',vars :system ,system :initial-data ,initial-data :format :html :project-solution t
				:return-plan t)
	   )))))

(defun make-override-data (parameters)
  ;; Don't support explicit nulls -- remove.
  (let ((pairs (remove nil parameters :key #'cadr))) 
    (when pairs (make-tuple pairs))))

(defun serve-graph (plan tmp-name &key (layout "dot") (format "svg") base-url)
  ;; FIXME: There must be a better way.
  ;; Or maybe this is good, and we should cache (based on content).
  (let ((image-file (ensure-directories-exist (merge-pathnames (make-pathname :name tmp-name :type format)
							       (uiop:default-temporary-directory))))
	(dgraph (dot-graph-from-plan plan)))
    (cl-dot:dot-graph dgraph image-file :format format)
    (hunchentoot:handle-static-file image-file)))
 
(define-calculation-pages (economic-performance :uri "/filecoin/economic-performance"
						:title "Filecoin Economic Performance Requirements"
						:vars (gib-seal-cost gib-hour-seal-investment fgr-months total-up-front-cost up-front-compute-cost
								 one-year-fgr two-year-fgr three-year-fgr)
						:override-parameters (GiB-seal-cycles)
						:system (performance-system :isolated t))
    ((gib-seal-cycles :parameter-type 'integer))
  (format nil "The economic component of Filecoin performance requirements."))

(define-calculation-pages (zigzag :uri "/filecoin/zigzag"
				  :title "ZigZag Proof of Replication"
				  :vars (sector-GiB
					 replication-time
					 replication-time-per-gib
					 seal-time GiB-seal-time storage-to-proof-size-float ;GiB-seal-cycles
					 total-zigzag-constraints
					 total-zigzag-challenges
					 optimal-heights
					 max-beta-merkle-height
					 total-hashing-time
					 total-circuit-time
					 wall-clock-seal-time
					 seal-parallelism)
				  :system (zigzag-system)
				  :override-parameters (sector-GiB max-beta-merkle-height)
				  )
    ((sector-GiB :parameter-type 'integer)
     (max-beta-merkle-height :parameter-type 'integer))
  (format nil "ZigZag is how Filecoin replicates. ~@[sector-GiB: ~W~]" sector-GiB))

(define-calculation-pages (filecoin-security :uri "/filecoin/zigzag-security"
					     :title "ZigZag Security"
					     :system (zigzag-security-system :isolated t)
					     :vars (;total-zigzag-challenges
						    zigzag-layers
						    zigzag-soundness
						    zigzag-basic-layer-challenges
						    zigzag-basic-layer-challenge-factor
						    zigzag-lambda
						    zigzag-delta
						    zigzag-epsilon
						    zigzag-space-gap
						    zigzag-taper
						    total-untapered-challenges
						    layers
						    total-zigzag-challenges
						    ;;zigzag-layer-challenges
						    ))
    ()
  "ZigZag security")

(define-calculation-pages (filecoin :uri "/filecoin"
				    :title "Filecoin Writ Large"
				    :vars (gib-seal-cost gib-hour-seal-investment seal-time fgr-months total-up-front-cost
							 fc::filecoin-requirements-satisfied
							 one-year-fgr two-year-fgr three-year-fgr
					;storage-to-proof-size-float
							 )
				    :override-parameters (annual-income layers total-challenges sector-size)
				    :system (filecoin-system))
    ((annual-income :parameter-type 'integer)
     (layers :parameter-type 'integer)
     (total-challenges :parameter-type 'integer)
     (sector-size :parameter-type 'integer))
  "Filecoin is " ((:a :href "filecoin") "Filecoin") ".")

(hunchentoot:define-easy-handler (index :uri "/") ()
  (with-page ("Orient to Filecoin")
    (:ul
     `(:div
       ,@(loop for calc-page in *calculation-pages*
	    collect `(:li ((:a :href ,(calc-page-base-uri calc-page)) ,(calc-page-title calc-page))))))))
