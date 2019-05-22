(defsystem "orient"
  :description "orient: Orientable Lisp."
  :version "0.0.1"
  :author "porcuquine <porcuquine@gmail.com>"
  :licence "MIT"
  :depends-on ("cl-json" "fiveam")
  :components ((:module "base"
			:components
			((:file "orient")
			 (:file "interface" :depends-on ("orient"))
			 (:file "demo" :depends-on ("interface")))
			:perform (test-op (o c) (symbol-call :fiveam :run! (find-symbol "ORIENT-SUITE" "ORIENT"))))
	       (:module "filecoin"
			:depends-on ("base")
			:components
			((:file "filecoin"))))
  :in-order-to ((test-op (load-op "orient")))
  :perform (test-op (o c)
		    (symbol-call :fiveam :run! (find-symbol "ORIENT-SUITE" "ORIENT"))
		    (symbol-call :fiveam :run! (find-symbol "FILECOIN-SUITE" "FILECOIN"))))
