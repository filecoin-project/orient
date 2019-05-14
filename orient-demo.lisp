#!/usr/local/bin/sbcl --script
(require "asdf")
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(asdf:load-system "orient")

(in-package "DEMO")

(deftransformation my-transf ((height width depth) -> (volume)) (values (* height width depth)))

(defun main() 
  (let* ((transformation (load-transformation "json/volumeOfBox.json"))
	 (data-map (data-map '((height 1) (width 2) (depth 10.25) (density 0.9995))))
	 (pipeline (load-pipeline "json/boxMassPipeline.json"))
	 (system (make-instance 'system :components (list (make-instance 'component :transformations pipeline))))
	 (problem (make-signature '(height width depth density) '(mass)))
	 (plan (plan system problem)))
    (format t "~&~%Evaluating data-map with transformationation: ")
    (encode-json (apply-transformation my-transf data-map))

    (format t "~&~%Evaluating data-map with pipeline: ")
    (encode-json (apply-transformation  pipeline data-map))

    (format t "~&~%Solve for box mass. Problem signature: ~S~%"  problem)
    (format t "~&~%Did this recreate our pipeline? ~A~%" (if (same pipeline plan) "YES" "NO"))
    (encode-json (solve system problem data-map))))

(main)
