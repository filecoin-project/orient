(defpackage "DEMO"
  (:use "COMMON-LISP" "ORIENT" "INTERFACE" "CL-JSON")
  (:export :mass-of-solid :volume-of-box))

(in-package "DEMO")

(deftransform mass-of-solid ((volume density) -> (mass))
  (values (* volume density)))

(deftransform volume-of-box ((height width depth) -> (volume))
  (values (* height width depth)))
