(defpackage orient.cli
  (:use :common-lisp :orient :orient.interface :filecoin :unix-options)
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
		   (calc (calc  "{zigzag}"  "Calculator to use"))
		   (port (port "port-number" "porton to listen on"))
		   (command (command "{web, solve}" "<COMMAND>: may be provided as free token (without flag)."))
		   &free commands)
    (map-parsed-options (cli-options) nil '("in" "i"
					    "out" "o"
					    "calc" "c"
					    "port" "p"
					    "command" "c") ;; Need to include all parameters from WITH-CLI-OPTIONS here.
			(lambda (option value) (declare (ignore option value)))
			(lambda (free-val) (declare (ignore free-val))))
    (destructuring-bind (&optional arg0 free-command &rest subcommands) commands
      (declare (ignore arg0 subcommands))

      (let* ((*schema-package* (find-package :filecoin))
	     (command (if command
			  (progn (assert (not free-command))
				 command)
			  free-command))
	     (calc-spec (maybe-keywordize calc))
	     (json:*json-symbols-package* 'filecoin) ;; FIXME: remove need to expose use of JSON package here.
	     (input (cond
		      ((equal in "--") (load-tuple *standard-input*))
		      (in (load-tuple in))))

	     (system (choose-system calc-spec)))
	(with-output (out)
	  (case (keywordize command)
	    ((:web)
	     (let ((acceptor (if port
				 (web:start-web :port port)
				 (web:start-web))))
	       (when acceptor
		 (format *error-output* "Orient webserver started on port ~S" (hunchentoot:acceptor-port acceptor)))
	       (let ((*package* (find-package :orient.web)))
		 (sb-impl::toplevel-repl nil))))
	    ((:solve)
	     (case calc-spec
	       ((:zigzag)
		(handle-calc :system system :input input :vars '(GiB-seal-time))
		nil)
	       (otherwise
		(format *error-output* "No system specified.~%")
		nil)))
	    ((:dump)
	     (dump-json :system system *out*))
	    (otherwise
	     (format t "Usage: ~A command~%  command is one of {web, solve}~%" (car argv)))))))))

(defun choose-system (spec)
  (case spec
    (:zigzag (zigzag-system))))

(defun handle-calc (&key system vars input)
  (let ((solution (solve-for system vars nil :override-data input)))
    (cl-json:encode-json (tuples solution) *out*)
    (terpri)))

