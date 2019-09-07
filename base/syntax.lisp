(in-package :orient)

(defconstant +line-comment+ "//")

(defun read-infix (stream)
  (let ((*readtable* (editor-hints.named-readtables:ensure-readtable 'cmu-infix:syntax)))
    (read stream)))

(defun clean-line (string)
  (let* ((comment-start (search +line-comment+ string))
	 (without-comment (if comment-start
			      (subseq string 0 comment-start)
			      string))
	 (comment (when comment-start
		    (subseq string comment-start)))
	 (trimmed (string-right-trim '(#\Space #\Tab #\Newline #\;) without-comment))
	 (indent-level (position-if (lambda (x) (not (eql x #\space))) trimmed)))
    (values trimmed indent-level comment)))

(defun read-infix-from-string (string)
  (with-input-from-string (in (format nil "#I(~a)" string))
    (read-infix in)))

(defun parse-line (string)
  (multiple-value-bind (cleaned indent-level comment)
      (clean-line string)
    (when (not (zerop (length cleaned)))
	(values (read-infix-from-string cleaned) indent-level))))

(defun parse (stream)
  (loop for line = (read-line stream nil)
     while line
     collect (multiple-value-list (parse-line line))))

(defun %group-by-indentation (parsed-lines indent-level &optional (xxx '()))
  (loop
     (let ((parsed-line (pop parsed-lines)))
       (destructuring-bind (&optional parsed line-indent-level comment)
	   parsed-line
	 (declare (ignore comment))
	 (cond
	   ((not (and parsed line-indent-level))
	    (return (values (reverse xxx) parsed-lines)))
	   ((= line-indent-level indent-level)
	    (push parsed xxx))
	   ((> line-indent-level indent-level)
	    (multiple-value-bind (result remaining-parsed-lines)
		(%group-by-indentation parsed-lines line-indent-level (list parsed (pop xxx)))
	      (display parsed indent-level line-indent-level)
	      (push result xxx)
	      (setq parsed-lines remaining-parsed-lines)))
	   (t (push parsed-line parsed-lines)
	      (return (values (reverse xxx) parsed-lines))))))))

(defun parse-file (filespec &key (group t))
  (with-open-file (in filespec :direction :input)
    (let ((parsed-lines (parse in)))
      (if group
	  (group-by-indentation parsed-lines)
	  parsed-lines))))

