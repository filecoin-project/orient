(defsystem "orient"
  :description "orient: Orientable Lisp."
  :version "0.2.0"
  :author "porcuquine <porcuquine@gmail.com>"
  :licence "MIT"
  :depends-on ("cl-json" "fiveam" "hunchentoot" "uiop" "unix-options" "fset" "cl-ppcre" "cl-dot" "cl-permutation" "cmu-infix" "dexador")
  :components ((:module "base"
			:serial t
			:components
			((:file "packages")
			 (:file "prelude")
			 (:file "macros")
			 (:file "util")
			 (:file "relation")
			 (:file "orient")
			 (:file "constraint")
			 (:file "interface")
			 (:file "lang")
			 (:file "presentation"))
			:perform (test-op (o c) (symbol-call :fiveam :run! (find-symbol "ORIENT-SUITE" "ORIENT"))))
	       (:module "filecoin"
			:depends-on ("base")
			:components
			((:file "base")
			 (:file "import")
			 (:file "performance")
			 (:file "zigzag")
			 (:file "zigzag-security")
			 (:file "filecoin")
			 (:file "systems")
			 (:file "publish")
			 (:file "spec")
			 ))
	       (:module "web"
			:depends-on ("base" "filecoin")
			:serial t
			:components
			((:file "html")
			 (:file "web")
                         (:file "api")))
	       (:module "cli"
			:depends-on ("base" "filecoin")
			:serial t
			:components
			((:file "cli"))))
  :in-order-to ((test-op (load-op "orient")))
  :perform (test-op (o c)
		    (flet ((run-suite (name-spec package-spec)
			     (symbol-call :fiveam :run! (find-symbol (string name-spec) (string package-spec)))))
		      (let ((orient-relation (run-suite :orient-relation-suite :orient))
			    (orient-constraint (run-suite :orient-constraint-suite :orient))
			    (orient (run-suite :orient-suite :orient))
			    (interface (run-suite :interface-suite :orient.interface))
			    (orient-lang (run-suite :orient-lang-suite :orient.lang))
			    (filecoin (run-suite :filecoin-suite :filecoin))
			    (web (run-suite :web-suite :orient.web)))
			(unless (and orient-relation orient-constraint orient interface orient-lang filecoin web)
			  (error "Some tests failed.")
			  ;; TODO: report which suites failed.
			  )))))
