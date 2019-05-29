(defsystem "orient"
  :description "orient: Orientable Lisp."
  :version "0.0.1"
  :author "porcuquine <porcuquine@gmail.com>"
  :licence "MIT"
  :depends-on ("cl-json" "fiveam")
  :components ((:module "base"
			:serial t
			:components			
			((:file "packages")
			 (:file "prelude")
			 (:file "macros")
			 (:file "orient")
			 (:file "interface")
			 (:file "demo"))
			:perform (test-op (o c) (symbol-call :fiveam :run! (find-symbol "ORIENT-SUITE" "ORIENT"))))
	       (:module "filecoin"
			:depends-on ("base")
			:components
			((:file "filecoin"))))
  :in-order-to ((test-op (load-op "orient")))
  :perform (test-op (o c)
		    (let ((orient (symbol-call :fiveam :run! (find-symbol "ORIENT-SUITE" "ORIENT")))
			  (filecoin (symbol-call :fiveam :run! (find-symbol "FILECOIN-SUITE" "FILECOIN"))))
		      (unless (and orient filecoin)
			(error "Some tests failed.")
			       ;; TODO: report which suites failed.
			      ))))
