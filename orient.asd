(defsystem "orient"
  :description "orient: Orientable Lisp."
  :version "0.0.1"
  :author "porcuquine <porcuquine@gmail.com>"
  :licence "MIT"
  :depends-on ("cl-json" "fiveam")
  :components ((:file "orient")
	       (:file "interface" :depends-on ("orient"))
	       (:file "demo" :depends-on ("interface"))))
