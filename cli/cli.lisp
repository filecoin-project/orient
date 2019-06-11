(defpackage orient.cli
  (:use :common-lisp :orient :orient.interface :filecoin :unix-options)
  (:nicknames :cli)
  (:export :main))

(in-package :orient.cli)

(defun keywordize (string-designator)
  (intern (string-upcase (string string-designator)) :keyword))

(defun maybe-keywordize (thing)
  (and thing (keywordize thing)))

(defun main (&optional argv)
  (declare (ignore argv))
  
  (with-cli-options ((cli-options) t)
      (&parameters (in (in "FILE" "JSON input file"))
		   (calc (calc  "{zigzag}"  "Calculator to use"))
		   (port (port "port number for web server")))

    (let ((free-vals '()))
      (map-parsed-options (cli-options) nil nil 
			  (lambda (option value)
			    (declare (ignore option value)))
			  (lambda (free-val)
			    (push free-val free-vals)))
      (destructuring-bind (&optional arg0 command &rest subcommands) (nreverse free-vals)
	(declare (ignore arg0 subcommands))

	(let* ((json:*json-symbols-package* 'filecoin)
	       (input (cond
			(in (load-tuple in))
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
	     (case (maybe-keywordize calc)
	       ((:zigzag)
		(handle-calc :system (zigzag-system) :input input :vars '(GiB-seal-time))
		nil)
	       (otherwise
		(format t "No system specified.~%")
		nil)))))))))

(defun handle-calc (&key system vars input)
  (let ((solution (solve-for system vars nil :override-data input)))
    (cl-json:encode-json (tuples solution))
    (terpri)
    ))

