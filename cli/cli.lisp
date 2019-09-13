(defpackage orient.cli
  (:use :common-lisp :orient :orient.interface :unix-options :orient.lang)
  (:shadow :orient :parameter)
  (:nicknames :cli)
  (:export :main))

(in-package :orient.cli)

(defun keywordize (string-designator)
  (intern (string-upcase (string string-designator)) :keyword))

(defun maybe-keywordize (thing)
  (and thing (keywordize thing)))

(defvar *out* *standard-output*)

(defmacro with-output ((output-spec) &body body)
  (let ((out (gensym "out")))
    `(let ((,out ,output-spec))
       (if ,out
	   (with-open-file (*out* (pathname ,out) :direction :output :if-exists :supersede)
	     ,@body)
	   (let ((*out* *standard-output*)) ,@body)))))

(defun main (&optional argv)
  (with-cli-options ((cli-options) t)
      (&parameters (in (in "FILE" "JSON input file, specify -- to use stdin"))
		   (out (out "FILE" "JSON output file, otherwise stdout"))
		   (system (system "FILE" "URI or filename of system to use"))
		   (port (port "port-number" "port to listen on"))
		   (merge (merge nil "merge inputs with (instead of replacing) defaults"))
		   (command (command "{dump, graph, solve, test, web}" "<COMMAND>: may be provided as free token (without flag)."))
		   (root (root "project root, so we can find json files"))
		   &free commands)
    (map-parsed-options (cli-options) nil '("in" "i"
					    "out" "o"
					    "port" "p"
					    "system" "s"
					    "command" "c"
					    "merge" "m"
					    "root" "r") ;; Need to include all parameters from WITH-CLI-OPTIONS here.
			(lambda (option value) (declare (ignore option value)))
			(lambda (free-val) (declare (ignore free-val))))
    (when root
      (setq orient.base.util:*project-root* (truename root)))
    
    (destructuring-bind (&optional arg0 free-command &rest subcommands) commands
      (declare (ignore arg0 subcommands))
      (let ((command (keywordize (if command
				     (progn (assert (not free-command))
					    command)
				     free-command))))
	(when (eql command :test)
	  (asdf:test-system :orient)
	  (return-from main t))
	(with-json-encoding ((find-package :orient.lang))
	  (let* ((*schema-package* (find-package :orient.lang))
		 (*package* (find-package :orient.lang))
		 (json:*json-symbols-package* :orient.lang) ;; FIXME: remove need to expose use of JSON package here.
		 (input (cond
			  ((equal in "--") (load-tuple *standard-input*))
			  (in (load-tuple in))))

		 (system (when system (get-system system))))
	    (with-output (out)
	      (case command
		((:web)
		 (let ((acceptor (if port
				     (web:start-web :port (parse-integer port))
				     (web:start-web))))
		   (when acceptor
		     (format *error-output* "Orient webserver started on port ~S" (hunchentoot:acceptor-port acceptor)))
		   (handler-case (bt:join-thread (find-if (lambda (th)
							    (search "hunchentoot" (bt:thread-name th)))
							  (bt:all-threads)))
		     ;; https://stackoverflow.com/questions/48103501/deploying-common-lisp-web-applications
		     ;; Catch a user's C-c
		     (#+sbcl sb-sys:interactive-interrupt
		       #+ccl  ccl:interrupt-signal-condition
		       #+clisp system::simple-interrupt-condition
		       #+ecl ext:interactive-interrupt
		       #+allegro excl:interrupt-signal
		       () (progn
			    (format *error-output* "Aborting.~&")
			    (web:stop-web)
			    (uiop:quit)))
		     (error (c) (format t "Woops, an unknown error occured:~&~a~&" c)))
		   (let ((*package* (find-package :orient.web)))
		     
		     (sb-impl::toplevel-repl nil))))
		((:solve)
		 (cond
		   (system
		    (let ((override-data (and merge input))
			  (input (and (not merge) input)))
		      (handle-system :system system :input input :override-data override-data)))
		   (t (format *error-output* "No system specified.~%"))))
		((:graph)
		 (cond
		   (system
		    (let ((override-data (and merge input))
			  (input (and (not merge) input)))
		      (graph-plan :system system :input input :override-data override-data)))
		   (t (format *error-output* "No system specified.~%")))
		 )
		((:dump)
		 (dump-json :system system *out* :expand-references t))
		(otherwise
		 ;; TODO: Generate this list and share with options doc.
		 (format t "Usage: ~A command~%  command is one of {dump, graph, solve, test, web}~%" (car argv))))
	      (terpri))))))))

(defun choose-system (spec)
  (case spec
    (:zigzag (zigzag-system))
    (:performance (performance-system))
    (:filecoin (filecoin-system))
    (:fc-no-zigzag (filecoin-system :no-zigzag t))))

(defun handle-system (&key system vars input override-data)
  (let ((solution (solve-for system vars input :override-data override-data)))
    (with-json-encoding ((find-package :filecoin))
      (cl-json:encode-json (ensure-tuples solution) *out*)
      (terpri))))

(defun graph-plan (&key system vars input override-data)
  (let ((plan (plan-for system vars input :override-data override-data)))
    (cl-dot:print-graph (dot-graph-from-plan plan))))
