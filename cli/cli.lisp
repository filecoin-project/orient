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

(defun main (&optional argv)
  (with-cli-options ((cli-options) t)
      (&parameters (in (in "FILE" "JSON input file"))
		   (calc (calc  "{zigzag}"  "Calculator to use"))
		   (port (port "port number for web server"))
		   (command (command "{web, solve}" "command: may be provided as free token"))
		   &free commands)
    (map-parsed-options (cli-options) nil '("calc" "in" "port" "command")
			(lambda (option value) (declare (ignore option value)))
			(lambda (free-val) (declare (ignore free-val))))
    (destructuring-bind (&optional arg0 free-command &rest subcommands) commands
      (declare (ignore arg0 subcommands))

      (let* ((command (if command
			  (progn (assert (not free-command))
				 command)
			  free-command))
	     (calc-spec (maybe-keywordize calc))
	     (json:*json-symbols-package* 'filecoin)
	     (input (cond
		      (in (and in (load-tuple in)))
		      (t nil))))

	(case (keywordize command)
	  ((:web)
	   (let ((acceptor (if port
			       (web:start-web :port port)
			       (web:start-web))))
	     (when acceptor
	       (format *standard-output* "Orient webserver started on port ~S" (hunchentoot:acceptor-port acceptor)))
	     (let ((*package* (find-package :orient.web)))
	       (sb-impl::toplevel-repl nil))))
	  ((:solve)
	   (let ((system (choose-system calc-spec)))
	     (case calc-spec
	       ((:zigzag)
		(handle-calc :system system :input input :vars '(GiB-seal-time))
		nil)
	       (otherwise
		(format t "No system specified.~%")
		nil))))
	  ((:dump)
	   (dump-json :system system out))
	  (otherwise
	   (format t "Usage: ~A command~%  command is one of {web, solve}~%" (car argv)))
	  )))))

(defun choose-system (spec)
  (case spec
    (:zigzag (zigzag-system))))

(defun handle-calc (&key system vars input)
  (let ((solution (solve-for system vars nil :override-data input)))
    (cl-json:encode-json (tuples solution))
    (terpri)
    ))

