(defsystem "orient"
  :description "orient: Orientable Lisp."
  :version "0.0.1"
  :author "porcuquine <porcuquine@gmail.com>"
  :licence "MIT"
  :depends-on ("cl-json" "fiveam" "hunchentoot" "uiop" "unix-options")
  :components ((:module "base"
			:serial t
			:components			
			((:file "packages")
			 (:file "prelude")
			 (:file "macros")
			 (:file "util")
			 (:file "orient")
			 (:file "interface")
			 (:file "demo"))
			:perform (test-op (o c) (symbol-call :fiveam :run! (find-symbol "ORIENT-SUITE" "ORIENT"))))
	       (:module "filecoin"
			:depends-on ("base")
			:components
			((:file "base")
			 (:file "performance")
			 (:file "zigzag")
			 (:file "post")
			 (:file "filecoin")))
	       (:module "web"
			:depends-on ("base" "filecoin")
			:serial t
			:components
			((:file "html")
			 (:file "web")))
	       (:module "cli"
			:depends-on ("base" "filecoin")
			:serial t
			:components
			((:file "cli")))
	       )
  :in-order-to ((test-op (load-op "orient")))
  :perform (test-op (o c)
		    (flet ((run-suite (name-spec package-spec)
			     (symbol-call :fiveam :run! (find-symbol (string name-spec) (string package-spec)))))
		      (let ((orient (run-suite :orient-suite :orient))
			    (filecoin (run-suite :filecoin-suite :filecoin))
			    (web (run-suite :web-suite :orient.web)))
		      (unless (and orient filecoin)
			(error "Some tests failed.")
			       ;; TODO: report which suites failed.
			      )))))
