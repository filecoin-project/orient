(defpackage orient.cli
  (:use :common-lisp :orient :orient.interface :unix-options :orient.lang :orient.web.html :it.bese.FiveAm)
  (:shadow :orient :parameter)
  (:nicknames :cli)
  (:export :main))

(in-package :orient.cli)

(def-suite orient-cli-suite)
(in-suite orient-cli-suite)

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


(defun emit-error-output (format-string &rest format-args)
  (apply #'format *error-output* format-string format-args))

(defun format-error (format-string &rest format-args)
  (apply #'emit-error-output format-args format-args)
  (signal 'error))

(defun main (&optional argv)
  (handler-case
      (progn
	(with-cli-options ((cli-options) t)
	    (&parameters (in (in "FILE" "JSON input file, specify -- to use stdin"))
			 (flags (flags "FLAGS" "provided flags select components so labeled in system."))
			 (out (out "FILE" "JSON output file, otherwise stdout"))
			 (system (system "FILE" "URI or filename of system to use"))
			 (port (port "port-number" "port to listen on"))
			 (merge (merge nil "merge inputs with (instead of replacing) defaults"))
			 (command (command "{dump, graph, solve, solve-many, report, test, web}" "<COMMAND>: may be provided as free token (without flag)."))
			 (root (root "project root, so we can find json files"))
			 &free commands)
	  (map-parsed-options (cli-options) nil '("in" "i"
						  "flags" "f"
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
                       (input-stream (if (equal in "--") *standard-input* in))
                       (raw-input (and in (get-json-data input-stream)))
		       (raw-flags (remove nil (mapcar #'interface:camel-case-to-lisp* (orient.base.util:string-split #\,  flags))))
		       (raw-system (when system (get-system system)))
		       (system (when raw-system (prune-system-for-flags raw-system raw-flags))))
		  (with-output (out)
		    (case command
		      ((:web)
		       (let ((acceptor  (if port
                                           (web:start-web :port (parse-integer port))
                                           (web:start-web))))
			 (when acceptor
			   (emit-error-output "Orient webserver started on port ~S" (hunchentoot:acceptor-port acceptor)))

                         (when system
                           (orient.web.api:register-primary-solver-endpoints
                            ;; TODO: Refactor to remove duplication between SOLVE and SOLVE-MANY closures.
                            (lambda (input-string)
                              (let* ((*schema-package* (find-package :orient.lang))
                                     (*package* *schema-package*)
                                     (json:*json-symbols-package* *schema-package*)
                                     (raw-input (get-json-data-from-string input-string)))
                                (with-output-to-string (*out*)
                                  (with-json-encoding (*schema-package*)
                                    (handle-solve-system :raw-input raw-input :system system :raw-system raw-system :raw-flags raw-flags :merge merge)))))

                            (lambda (input-string)
                              (let* ((*schema-package* (find-package :orient.lang))
                                     (*package* *schema-package*)
                                     (json:*json-symbols-package* *schema-package*)
                                     (raw-input (get-json-data-from-string input-string)))
                                (with-output-to-string (*out*)
                                  (with-json-encoding (*schema-package*)
                                    (handle-multi-solve-system :raw-input raw-input :system system :raw-system raw-system :raw-flags raw-flags :merge merge)))))))

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
			          (emit-error-output "Aborting.~&")
			          (web:stop-web)
			          (uiop:quit)))
			   (error (c) (format-error "Woops, an unknown error occured:~&~a~&" c)))

			 (let ((*package* (find-package :orient.web)))
			   (sb-impl::toplevel-repl nil))))
		      ((:solve)
 		       (cond
			 (system
                          (handle-solve-system :system system :raw-system raw-system :raw-flags raw-flags :merge merge :raw-input raw-input))
			 (t (format-error "No system specified.~%"))))
                      (:solve-many
                       (cond
			 (system
                          (unless (listp raw-input)
                            (format-error "Input to solve-many must be an array of inputs to be solved and reported independently."))
                          (handle-multi-solve-system :system system :raw-system raw-system :raw-flags raw-flags :merge merge :raw-input raw-input))
			 (t (format-error "No system specified.~%"))))
		      ((:report)
		       (cond
			 (system
			  (let* ((input (make-relation-list raw-input))
                                 (override-data (and merge input))
                                 (input (and (not merge) input)))
			    (report-system :system system :initial-data input :override-data override-data)))
			 (t (format-error "No system specified.~%"))))
		      ((:graph)
		       (cond
			 (system
			  (let* ((input (make-relation-list raw-input))
                                 (override-data (and merge input))
                                 (input (and (not merge) input)))
			    (graph-plan :system system :input input :override-data override-data)
			    (sb-ext:exit :code 0)))
			 (t (format-error "No system specified.~%"))))
		      ((:dump)
		       (dump-json :system system *out* :expand-references t))
		      (otherwise
		       ;; TODO: Generate this list and share with options doc.
		       (format-error "Usage: ~A command~%  command is one of {dump, graph, solve, report, test, web}~%" (car argv))))
		    (terpri)))))))
        (sb-ext:exit :code 0))
    (error (e)
      (format t "~%ERROR: ~A~%" e)
      (sb-ext:exit :code 1))
    (condition () (sb-ext:exit :code 0))))

(defun choose-system (spec)
  (case spec
    (:zigzag (zigzag-system))
    (:performance (performance-system))
    (:filecoin (filecoin-system))
    (:fc-no-zigzag (filecoin-system :no-zigzag t))))

(defun handle-solve-system (&key raw-system raw-flags merge raw-input system)
  (let* ((input (make-relation-list raw-input))
         (override-data (and merge input))
         (input (and (not merge) input))
         (defaulted (defaulted-initial-data system input :override-data override-data))
         (combinations (separate-by-flag-combinations defaulted)))
    ;; All the logic for generating flag combinations from data and instantiating multiple systems
    ;; should move into orient.lisp.
    (json:with-array 
     (*out*)
     (let* ((f (orient::tfn (flags relation)
                            (let* ((true-flags (remove nil (mapcar (lambda (f)
                                                                     (when (cdr f) (flag-symbol (car f))))
                                                                   (fset:convert 'list flags))))
                                   (merged-flags (union true-flags raw-flags))
                                   (flags-tuple (make-tuple (mapcar (lambda (f)
                                                                      (list (make-flag f) t))
                                                                    merged-flags)))
                                   (final-system (prune-system-for-flags raw-system merged-flags)))
                              (solve-system :system final-system :input (join flags-tuple orient::relation) :override-data override-data))))
            (combination-tuples (fset:convert 'list (tuples combinations))))
       (dolist (tuple combination-tuples)
         (funcall f tuple))))))

(defun handle-multi-solve-system (&key raw-system raw-flags merge raw-input system)
  "Like HANDLE-SOLVE-SYSTEM but treat INPUT as an array of inputs and emit a corresponding array of outputs."
  (json:with-array (*out*)
   (dolist (single-raw-input raw-input)
     (json:as-array-member (*out*)
         (handle-solve-system :raw-system raw-system :raw-flags raw-flags :merge merge :raw-input single-raw-input :system system)))))

(defun solve-system (&key system vars input override-data report)
  (let ((solution (solve-for system vars input :override-data override-data))
        (*alpha-sort-tuples* t))
    (with-json-encoding ((find-package :filecoin))
      (cl-json:encode-array-member (ensure-tuples solution) *out*)
      (terpri))))

(defun report-system (&key system vars initial-data override-data)
  (princ
   (web:serve-report-page :system system :vars vars :initial-data initial-data :override-data override-data)))

(defun graph-plan (&key system vars input override-data)
  (let ((plan (plan-for system vars input :override-data override-data)))
    (cl-dot:print-graph (dot-graph-from-plan plan))))
