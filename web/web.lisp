(defpackage orient.web
  (:use :common-lisp :orient :filecoin :orient.web.html :orient.base.util)
  (:nicknames :web)
  (:export :start-web :stop-web))

(in-package :orient.web)

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

(defmacro with-page ((title) &body body)
  `(let ((*package* (find-package :filecoin)))
     (setf (hunchentoot:content-type*) "text/html")
     (with-output-to-string (*html-output-stream*)
       (html
	(:html
	 (:head (:title ,title)
		(:h1 ,title)
		((:a href "/") "INDEX"))
	 (:body ,@body))))))

(defmacro with-report-page ((title &key vars system initial-data override-data) &body body)
  `(serve-report-page :title ,title
		      :vars ',vars
		      :system ,system
		      :initial-data ,initial-data
		      :override-data ,override-data
		      :body ',body))

(defun report-page (&key vars system initial-data override-data)
  (multiple-value-bind (report solution defaulted-data plan)
      (report-solution-for vars :system system :initial-data initial-data :format :html :override-data override-data :project-solution t
			   :return-plan t)
    (let* ((signature (pipeline-signature plan))
	   (parameters (union (signature-input signature) (signature-output signature))))
      (declare (ignore parameters)) ;; TODO: turn parameters into links in solution.
      `(:div (:html-escape ,(princ-to-string solution))
	     ,@(when override-data
		 (list :p (format nil "Supplied: ~s" override-data)))
	     (:hr)
	     (:p (:b "Initial data: " (:html-escape ,(princ-to-string defaulted-data))))
	     (:p ,report)))))

(defun serve-report-page (&key title vars system initial-data override-data body)
  (with-page (title)
    `(:div ,@body)
    (:p "Solving for " (comma-list vars) ".")
    (:hr)
    (:pre
     (report-page :vars vars :system system :initial-data initial-data :override-data override-data))))

(defmethod synthesize-report-steps ((format (eql :html)) (steps list))
  (with-output-to-string (*html-output-stream*)
    (html-print
     `(:div
       ,@(loop for step in steps
	    collect step
	    collect '(:hr))))))

(defun format-value (value)
  `(:html-escape ,(or value "FALSE")))

(defmethod create-tuple-report-step ((format (eql :html)) (tuple tuple) (transformation transformation) (system system) &key n)
  (declare (ignore n))
  (loop for (key value) in (tuple-pairs tuple)
     when (member key (signature-output (transformation-signature transformation)))
     ;; TODO: Handle N.
     ;; TODO: Extend DESCRIBE-TRANSFORMATION-CALCULATION to generate links to parameters.
     collect `(:html-escape
	       ((:a :name ,(symbol-name key)) (:b ,(symbol-name key)))
	       ": "
		,(awhen (describe-transformation-calculation transformation)
		   `((:font :color "red") ,it))
	       " = "
	       ((:font :color "blue") ,(format-value value))
	       ,(let ((desc (and system (lookup-description key system))))
	       	   (aif (and desc (not (equal desc "")))
			`(:div "     " ((:font :color "green") (:i ,desc)))
			;; '(:div "XXXXXXXXXXXXX-DESCRIPTION MISSING-XXXXXXXXXXXXX")
			)))))

(defmacro define-calculation-pages ((base-name &key uri title vars system initial-data override-parameters) (&rest parameters)
				    &body body)
  (let* ((graph-uri (format nil "~A-graph" uri))
	 (graph-name (symbolconc base-name '-graph))
	 (graph-namestring (symbol-name graph-name)))
    (register-calc-page base-name  title uri)
    `(eval-when
	 (:compile-toplevel :load-toplevel :execute)
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
		      :base-url ,uri)))))

(defun make-override-data (parameters)
  ;; Don't support explicit nulls -- remove.
  (let ((pairs (remove nil parameters :key #'cadr))) 
    (when pairs (make-tuple pairs))))
 
(defun serve-graph (plan tmp-name &key (layout "dot") (format "svg") base-url)
  ;; FIXME: There must be a better way.
  ;; Or maybe this is good, and we should cache (based on content).
  (let ((image-file (make-pathname :directory "tmp" :name tmp-name :type format)))
    (orient::dot
     (orient::dot-format
      (generate-directed-graph plan) nil :base-url base-url)
     :layout layout
     :format format
     :output-file image-file)
    (hunchentoot:handle-static-file image-file)))

(define-calculation-pages (economic-performance :uri "/filecoin/economic-performance"
						:title "Filecoin Economic Performance Requirements"
						:vars (seal-cost roi-months total-up-front-cost up-front-compute-cost
								 one-year-roi two-year-roi three-year-roi)
						:override-parameters	(GiB-seal-cycles)
						:system (performance-system :isolated t))
    ((gib-seal-cycles :parameter-type 'integer))
  (format nil "The economic component of Filecoin performance requirements." ))

(define-calculation-pages (zigzag :uri "/filecoin/zigzag"
				   :title "ZigZag Proof of Replication"
				   :vars (seal-time GiB-seal-time storage-to-proof-size-float ;GiB-seal-cycles
						    )
				   :system (zigzag-system)
				   :override-parameters (sector-size))
    ((sector-size :parameter-type 'integer))
  (format nil "ZigZag is how Filecoin replicates. sector-size: ~W" sector-size))

(define-calculation-pages (filecoin :uri "/filecoin"
				    :title "Filecoin Writ Large"
				    :vars (seal-cost seal-time roi-months total-up-front-cost fc::filecoin-requirements-satisfied
						     one-year-roi two-year-roi three-year-roi
						     ;storage-to-proof-size-float
						     )
				    :override-parameters (annual-income layers total-challenges sector-size)
				    :system (filecoin-system))
    ((annual-income :parameter-type 'integer)
     (layers :parameter-type 'integer)
     (total-challenges :parameter-type 'integer)
     (sector-size :parameter-type 'integer))
  "Filecoin is " ((:a :href "filecoin") "Filecoin") ".")

(define-calculation-pages (filecoin-security :uri "/filecoin/zigzag-security"
				    :title "ZigZag Security"
				    :system (zigzag-security-system :isolated t)
				    :vars (total-zigzag-challenges
					   zigzag-layers
					   zigzag-layer-challenges))
    ()
  "ZigZag security")

(hunchentoot:define-easy-handler (index :uri "/") ()
  (with-page ("Orient to Filecoin")    
    (:ul
     `(:div
       ,@(loop for calc-page in *calculation-pages*
	    collect `(:li ((:a :href ,(calc-page-base-uri calc-page)) ,(calc-page-title calc-page))))))))
