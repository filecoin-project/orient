(defsystem "orient"
  :description "orient: Orientable Lisp."
  :version "0.0.1"
  :author "porcuquine <porcuquine@gmail.com>"
  :licence "MIT"
  :depends-on ("cl-json" "fiveam")
  :components ((:file "orient")
	       (:file "interface" :depends-on ("orient"))
	       (:file "demo" :depends-on ("interface")))
  :in-order-to ((test-op (test-op "orient/tests"))))

(defsystem "orient/tests"
  :depends-on ("orient" "fiveam")
  :components ((:file "orient"))
  :perform (test-op (o c) (symbol-call :fiveam :run! (find-symbol "ORIENT-TESTS" "ORIENT"))))

