(defpackage orient.cli
  (:use :common-lisp :orient :orient.interface :orient.cache :unix-options :orient.lang :orient.web.html :it.bese.FiveAm :lparallel :solver)
  (:shadow :orient :parameter)
  (:nicknames :cli)
  (:export :main))

(in-package :orient.cli)

(def-suite orient-cli-suite)
(in-suite orient-cli-suite)

(defparameter *cache* (make-instance 'mem-cache) "Cache to use, if non-NIL.")

(defmacro in-new-thread ((&body body) (&body error-case))
  `(bt:make-thread
    (lambda ()
      (let* ((*schema-package* (find-package :orient.lang))
             (*package* *schema-package*)
             (json:*json-symbols-package* *schema-package*))
        (handler-case
            (progn ,@body)
          (error (c)
            (format *error-output* "~&~A~%" c)
            (trivial-backtrace:print-backtrace-to-stream *error-output*)
            ,@error-case))))))

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

(defun init ()
  (awhen (uiop/os:getenv "ORIENT_CACHE_DIR")
    (setq *cache* (make-disk-backed-mem-cache :root (truename it))))

  (ensure-parallelism-initialized))

(defun main (&optional argv)
  (init)
  (handler-case
      (progn
	(with-cli-options ((cli-options) t)
	    (&parameters (in (in "FILE" "JSON input file, specify -- to use stdin"))
			 (flags (flags "FLAGS" "provided flags select components so labeled in system."))
			 (out (out "FILE" "JSON output file, otherwise stdout"))
			 (system (system "FILE" "URI or filename of system to use"))
			 (port (port "port-number" "port to listen on"))
			 (merge (merge nil "merge inputs with (instead of replacing) defaults"))
			 (command (command "{dump, dump-vars, graph, optimize, report, solve, solve-many, test, web}" "<COMMAND>: may be provided as free token (without flag)."))
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
                            (make-web-handler
                             (lambda (raw-input)
                               (handle-solve-system :raw-input raw-input
                                                    :system system
                                                    :raw-system raw-system
                                                    :raw-flags raw-flags
                                                    :merge merge)))
                            (make-web-handler
                             (lambda (raw-input)
                               (handle-multi-solve-system :raw-input raw-input
                                                          :system system
                                                          :raw-system raw-system
                                                          :raw-flags raw-flags
                                                          :merge merge)))

                            (make-web-handler
                             (lambda (raw-input)
                               (declare (ignore raw-input))
                               (dump-vars system)))))

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
                      ((:dump-vars)
                       (dump-vars system))
                      ((:optimize)
                       (let* ((metadata (input-metadata raw-input))
                              (optimize-meta (find-meta '(lang::optimize) metadata)))
                         (multiple-value-bind (objective vars guess)
                             (optimization-vars optimize-meta)
                           (let ((result (solver-optimize system raw-input objective vars
                                                          :guess (coerce guess 'vector))))
                         (cl-json:encode-json result *out*)))))
		      (otherwise
		       ;; TODO: Generate this list and share with options doc.
		       (format-error "Usage: ~A command~%  command is one of {dump, dump-vars, graph, optimize, report, solve, solve-many, test, web}~%" (car argv))))
		    (terpri)))))))
        (sb-ext:exit :code 0))
    (error (e)
      (format t "~%ERROR: ~A~%" e)
      (sb-ext:exit :code 1))
    (condition () (sb-ext:exit :code 0))))

(defun make-var (string &key (package (find-package :orient.lang)))
  (intern (string-upcase string) package))

(defun optimization-vars (optimization-meta)
  (let* (objective
         (guesses '())
         (vars (loop for (var . guess) in optimization-meta
                  if (equalp "OBJECTIVE" guess) do (setq objective var)
                  else do (push guess guesses)
                  and collect var)))
    (values objective vars (nreverse guesses))))

(defun choose-system (spec)
  (case spec
    (:zigzag (zigzag-system))
    (:performance (performance-system))
    (:filecoin (filecoin-system))
    (:fc-no-zigzag (filecoin-system :no-zigzag t))))

(defvar *multi-solve-lock* (bt:make-lock))
(defvar *solve-lock* (bt:make-lock))

(defun handle-multi-solve-system (&key raw-system raw-flags merge raw-input system system-cache-key)
  "Like HANDLE-SOLVE-SYSTEM but treat INPUT as an array of inputs and emit a corresponding array of outputs."
  (let* ((next-write-index 0)
         (results (make-array (list (length raw-input))))
         (threads (loop for single-raw-input in raw-input
                     collect (with-captured-bindings (single-raw-input)
                               (in-new-thread
                                ((let ((result (handle-solve-system :raw-system raw-system
                                                                    :raw-flags raw-flags
                                                                    :merge merge
                                                                    :raw-input single-raw-input
                                                                    :system system
                                                                    :system-cache-key system-cache-key
                                                                    :no-wrap t)))
                                   (bt:with-lock-held (*multi-solve-lock*)
                                     (setf (aref results next-write-index) result)
                                     (incf next-write-index))))
                                ((bt:with-lock-held (*multi-solve-lock*)
                                   (setf (aref results next-write-index) nil)
                                   (incf next-write-index))))))))
    (dolist (thread threads)
      (bt:join-thread thread))
    (json:with-array (*out*)
      (loop for thunk across results do (funcall thunk)))))

(defun handle-solve-system (&key raw-system raw-flags merge raw-input system system-cache-key no-wrap)
  (let* ((*alpha-sort-tuples* t)
         (input (make-relation-list raw-input))
         (override-data (and merge input))
         (input (and (not merge) input))
         (defaulted (defaulted-initial-data system input :override-data override-data))
         (combinations (separate-by-flag-combinations defaulted))
         ;; All the logic for generating flag combinations from data and instantiating multiple systems
         ;; should move into orient.lisp.
         (f (orient::tfn (flags relation)
              (let* ((true-flags (remove nil (mapcar (lambda (f)
                                                       (when (cdr f) (flag-symbol (car f))))
                                                     (fset:convert 'list flags))))
                     (merged-flags (union true-flags raw-flags))
                     (flags-tuple (make-tuple (mapcar (lambda (f)
                                                        (list (make-flag f) t))
                                                      merged-flags)))
                     (final-system (prune-system-for-flags raw-system merged-flags)))
                (solve-system :system final-system
                              :input (join flags-tuple orient::relation)
                              :override-data override-data
                              :system-cache-key system-cache-key))))
         (combination-tuples (fset:convert 'list (tuples combinations)))
         (next-write-index 0)
         (results (make-array (list (length combination-tuples))))
         (threads (loop for tuple in combination-tuples
                     do (with-captured-bindings (tuple)
                          (in-new-thread
                           ((let ((result (funcall f tuple)))
                              (bt:with-lock-held (*solve-lock*)
                                (setf (aref results next-write-index) result)
                                (incf next-write-index))))
                           ((bt:with-lock-held (*solve-lock*)
                              (setf (aref results next-write-index) nil)
                              (incf next-write-index)))))))
         (thunk (lambda ()
                  (dolist (thread threads)
                    (bt:join-thread thread))
                  (loop for i below (length results)
                       ;: FIXME: This busy-wait loop (seemingly) should not be needed,
                       ;; given the thread-joins above. Yet it is empirically necessary.
                       ;; What gives?
                     do (loop until (not (eql (aref results i) 0)))
                     do (cl-json:encode-array-member (aref results i) *out*))
                  (terpri *out*))))
    (if no-wrap
        thunk
        (json:with-array
            (*out*)
          (funcall thunk)))))

(defun solve-system (&key system vars input override-data system-cache-key)
  (let* ((system-cache-key (or system-cache-key (dump-json-to-string :system system)))
         (solution (solve-for system vars input
                              :override-data override-data
                              :cache *cache*
                              :system-cache-key system-cache-key
                              :values-serializer (lambda (values)
                                                   ;; Discard all but primary return value.
                                                   (list (ensure-tuples (car values))))
                              :values-deserializer (lambda (values)
                                                     (list (make-relation-list (fset:convert 'list (car values))))))))
    (ensure-tuples solution)))

(defun report-system (&key system vars initial-data override-data)
  (princ
   (web:serve-report-page :system system :vars vars :initial-data initial-data :override-data override-data)))

(defun graph-plan (&key system vars input override-data)
  (let ((plan (plan-for system vars input :override-data override-data)))
    (cl-dot:print-graph (dot-graph-from-plan plan))))

;; Takes a handler function of one json input argument and returns a 'web handler'.
(defun make-web-handler (handler)
  (lambda (input-string)
    (let* ((*schema-package* (find-package :orient.lang))
           (*package* *schema-package*)
           (json:*json-symbols-package* *schema-package*)
           (raw-input (when input-string (get-json-data-from-string input-string))))
      (with-output-to-string (*out*)
        (with-json-encoding (*schema-package*)
          (funcall handler raw-input))))))

(defun dump-vars (system)
  (let* ((schemas (all-system-schemas system))
         (all-parameters (reduce #'append (mapcar #'schema-parameters schemas))))
    (json:encode-json all-parameters *out*)))

(defun input-metadata (raw-input)
  (when raw-input
    (tref '- raw-input)))

(defun find-meta (path meta)
  (if path
      (let ((next (find-if (util:partial #'eql (car path)) meta :key #'car)))
        (find-meta (cdr path) (cdr next)))
      meta))
