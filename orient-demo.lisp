#!/usr/local/bin/sbcl --script
(load "orient.lisp")
(use-package "ORIENT")

(defparameter data-map (data-map '((height 1) (width 2) (depth 10.25) (density 0.9995))))

(deftransform my-transf ((height width depth) -> (volume)) (values (* height width depth)))

(print (apply-transform my-transf data-map))
