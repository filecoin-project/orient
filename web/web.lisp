(defpackage orient.web
  (:use :common-lisp :orient :filecoin :orient.web.html :orient.base.util)
  (:nicknames :web)
  (:export :start-web))

(in-package :orient.web)

(defparameter *orient-web-port* 8888)

(eval-when
    (:compile-toplevel :load-toplevel :execute)
  (defvar *calculation-pages* '())

  (defstruct calc-page name title base-uri)

  (defun register-calc-page (name title base-uri)
    (pushnew (make-calc-page :name name :title title :base-uri base-uri) *calculation-pages* :key #'calc-page-name)))

(defun start-web (&key (port *orient-web-port*))
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))

(defmacro with-page ((title) &body body)
  `(progn
     (setf (hunchentoot:content-type*) "text/html")
     (with-output-to-string (*html-output-stream*)
       (html
	(:html
	 (:head (:title ,title)
		(:h1 ,title)
		((:a href "/") "INDEX"))
	 (:body ,@body))))))

(defmacro with-report-page ((title &key vars system initial-data override-data) &body body)
  `(with-page (,title)
     (:div ,@body)
     (:p "Solving for " ',(comma-list vars) ".")
     (:hr)
     (:pre
      (nth-value 0 (report-solution-for '(,@vars) :system ,system :initial-data ,initial-data :format :html :override-data ,override-data)))))

(defun serve-report-page (title &key vars system initial-data override-data body-html)
  (with-page (title)
     (:div body-html)
     (:p "Solving for " (comma-list vars) ".")
     (:hr)
     (:pre
      (nth-value 0 (report-solution-for vars :system system :initial-data initial-data :format :html :override-data override-data)))))

(defmethod synthesize-report-steps ((format (eql :html)) (steps list))
  (with-output-to-string (*html-output-stream*)
    (html-print
     `(:div
       ,@(loop for step in steps
	    collect step
	    collect '(:hr))))))

(defmethod create-tuple-report-step ((format (eql :html)) (tuple tuple) (transformation transformation) (system system) &key n)
  (declare (ignore n))
  (loop for (key value) in (tuple-pairs tuple)
     when (member key (signature-output (transformation-signature transformation)))
     ;; TODO: Handle N.
     ;; TODO: Extend DESCRIBE-TRANSFORMATION-CALCULATION to generate links to parameters.
     collect `(:div
	       ((:a :name ,(symbol-name key)) (:b ,(symbol-name key)))
	       " "
	       ,(awhen (describe-transformation-calculation transformation)
		     `((:font :color "red") ,it))
	       " = "
	       ((:font :color "blue") (:html-escape ,value))
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
									  collect `(list ',parameter  ,parameter)
									    )))
			    )
	   (:div
	    (:p ,@body)
	    (:p ((:a :href ,graph-uri) "See a Graph")))))
       
       (hunchentoot:define-easy-handler (,graph-name :uri ,graph-uri) ()
	 (serve-graph (plan-for ,system ',vars ,initial-data)
		      ,graph-namestring)))))

(defun make-override-data (parameters)
  ;; Don't support explicit nulls -- remove.
  (let ((pairs (remove nil parameters :key #'cadr))) 
    (when pairs (make-tuple pairs))))
 
(defun serve-graph (plan tmp-name &key (layout "dot") (format "svg"))
  ;; FIXME: There must be a better way.
  (let ((image-file (make-pathname :directory "tmp" :name tmp-name :type format)))
    (orient::dot
     (orient::dot-format
      (generate-directed-graph plan) nil)
     :layout layout
     :format format
     :output-file image-file)
    (hunchentoot:handle-static-file image-file)))

(define-calculation-pages (economic-performance :uri "/filecoin/economic-performance"
						:title "Filecoin Economic Performance Requirements"
						:vars (seal-cost)
						:system (performance-system)
						:initial-data *performance-defaults*)
    ()
  "The economic component of Filecoin performance requirements")

(define-calculation-pages (zigzag :uri "/filecoin/zigzag"
				   :title "ZigZag Proof of Replication"
				   :vars (seal-time)
				   :system (zigzag-system)
				   :override-parameters (sector-size))
    ((sector-size :parameter-type 'integer))
  (format nil "ZigZag is how Filecoin replicates. sector-size: ~W" sector-size))

(hunchentoot:define-easy-handler (index :uri "/") ()
  (with-page ("Orient to Filecoin")    
    (:ul
     `(:div
       ,@(loop for calc-page in *calculation-pages*
	    collect `(:li ((:a :href ,(calc-page-base-uri calc-page)) ,(calc-page-title calc-page))))))))
