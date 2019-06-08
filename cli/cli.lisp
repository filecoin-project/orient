(defpackage orient.cli
  (:use :common-lisp :orient :interface :filecoin :unix-options)
  (:nicknames :cli)
  (:export :main))

(in-package :orient.cli)

(defun main (&optional argv)
  (declare (ignore argv))
  
  (with-cli-options ((cli-options) t)
      (&parameters (in (in "FILE" "JSON input file")) (calc (calc  "{zigzag}"  "Calculator to use")))
    (let* ((json:*json-symbols-package* 'filecoin)
	   (input (cond
		    (in (load-tuple in))
		    (t nil))))
    (case (intern (string-upcase calc) :keyword)
      ((:zigzag)
       (handle-calc :system (zigzag-system) :input input :vars '(GiB-seal-time))
       nil)
      (otherwise
       (format t "No system specified.~%")
       nil)))))

(defun handle-calc (&key system vars input)
  (let ((solution (solve-for system vars nil :override-data input)))
    (cl-json:encode-json (tuples solution))
    (terpri)
    ))

