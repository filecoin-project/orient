(defpackage orient.web
  (:use :common-lisp :orient :filecoin :orient.web.html :orient.base.util)
  (:nicknames :web)
  (:export :start-web))

(in-package :orient.web)

(defparameter *orient-web-port* 8888)

(defun start-web (&key (port *orient-web-port*))
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))

(defmacro with-page ((title) &body body)
  `(progn
     (setf (hunchentoot:content-type*) "text/html")
     (with-output-to-string (*html-output-stream*)
       (html
	(:html
	 (:head (:title ,title)
		(:h1 ,title))
	 (:body ,@body))))))

(defmacro with-report-page ((title &key vars system initial-data override-data) &body body)
  `(with-page (,title)
     (:div ,@body)
     (:p "Solving for " ',(comma-list vars) ".")
     (:hr)
     (:pre
      (nth-value 0 (report-solution-for '(,@vars) :system ,system :initial-data ,initial-data :format :html :override-data ,override-data)))))

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

(hunchentoot:define-easy-handler (index :uri "/") ()
  (with-page ("Orient to Filecoin")
    (:ul
     (:li ((:a :href "filecoin/performance") "Filecoin Performance"))
     (:li ((:a :href "filecoin/zigzag") "ZigZag Proof of Replication ")))))

(hunchentoot:define-easy-handler (filecoin-performance :uri "/filecoin/performance") ()
  (with-report-page ("Filecoin Performance" :vars (seal-cost) :system (performance-system) :initial-data *performance-defaults*)
    "Performance is defined economically."))

;;; TODO: a single macro which generates the query page and the graph.
(hunchentoot:define-easy-handler (zigzag :uri "/filecoin/zigzag") ((sector-size :parameter-type 'integer))
  (with-report-page ("ZigZag Proof of Replication"
		     :vars (seal-time)
		     :system (zigzag-system)
		     :override-data (tuple (sector-size sector-size)))
    (:div
     (:p (format nil "ZigZag is how Filecoin replicates. sector-size: ~W" sector-size))
     (:p ((:a :href "zigzag-graph") "See a Graph")))))

(hunchentoot:define-easy-handler (zigzag-graph :uri "/filecoin/zigzag-graph") ()
  ;; FIXME: There must be a better way.
  (let ((image-file "/tmp/zigzag-graph.png"))
    (orient::dot
     (orient::dot-format
      (generate-directed-graph (plan-for (zigzag-system) '(seal-time))) nil)
     :layout "dot"
     :format "png"
     :output-file image-file)
  (hunchentoot:handle-static-file image-file)))
