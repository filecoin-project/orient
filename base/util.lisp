(in-package :orient.base.util)

(defun comma-list (things &key (separator ", ") before after)
  (with-output-to-string (out)
    (when before (princ separator out))
    (loop for (thing . rest) on things
          do (princ thing out)
          when rest do (princ separator out))
    (when after (princ separator out))))


(defun string-split (delimiter string &key (start 0) end )
  (let* ((next-function (typecase delimiter
                          (string
                           (let ((length (length delimiter)))
                             (lambda (start end)
                               (let ((position (search delimiter string :start2 start :end2 end)))
                                 (values position (and position (+ position length)))))))
                          (character
                           (lambda (start end)
                             (let ((position (position delimiter string :start start :end end)))
                               (values position (and position (1+ position))))))))
         (real-end (or end (length string))))
    (loop with next-position = nil and next-start = nil
       do (multiple-value-setq (next-position next-start) (funcall next-function start end))
       collect (subseq string start (or next-position real-end))
       while next-position
       do (setq start next-start)
       when (> start real-end) do (loop-finish))))

(defvar *project-base-uri* "https://github.com/filecoin-project/orient")

(defun commit-base-uri (&optional (base-uri *project-base-uri*))
  (format nil "~A/commit" base-uri))

(defvar *project-root* nil "The project root should be a directory pathname and will override the calculated value of PROJECT-ROOT if set.")

(defun project-root ()
  (or *project-root*
      (let* ((root-file (asdf:system-source-file (asdf:find-system :orient)))
	     (project-dir (pathname-directory root-file)))
	(make-pathname :directory project-dir))))

(defun project-merge (pathspec)
  (merge-pathnames (pathname pathspec) (project-root)))

(defun project-commit (&optional (commit-base-uri (commit-base-uri)))
  (let* ((project-path (project-root))
	 (commit (uiop:run-program (format nil "cd ~a; git rev-parse HEAD" project-path) :output :string))
	 (uri (format nil "~A/~A" commit-base-uri commit)))
    (values commit uri)))

(defun map-tree (function tree)
  (typecase tree
    (null)
    (atom (funcall function tree))
    (cons (cons (map-tree function (car tree))
                (map-tree function (cdr tree))))))

(defun transform-tree (function tree &key test key)
  (let ((function (coerce function 'function))
        (test (when test (coerce test 'function)))
        (key (when key (coerce key 'function))))
    (map-tree (lambda (x)
                (if (and test 
                         (not (funcall test 
                                       (if key (funcall key x) x))))
                    x
                  (funcall function x)))
              tree)))

(defun transform-tree-if (test function tree)
  (when tree
    (cond
     ((funcall test tree)
      (funcall function tree))
     (t (typecase tree
          (cons (cons (transform-tree-if test function (car tree))
                      (transform-tree-if test function (cdr tree))))
          (t tree))))))

(defun keywordize (string-designator)
  (intern (string-upcase (string string-designator)) :keyword))

(defun partial (function &rest args)
  (lambda (&rest new-args)
    (apply function (append args new-args))))

(defun string-starts-with (prefix string)
  (let ((mismatch (mismatch prefix string)))
    (if mismatch
        (when (= mismatch (length prefix))
          mismatch)
      t)))

(defun flatten (list &optional depth)
  (if (eql 0 depth)
      list
      (loop for item in list
         if (listp item) append (flatten item (and depth (1- depth)))
         else collect item)))

(defun flatten1 (list) (flatten list 1))
(defun flatten2 (list) (flatten list 2))

(defun slurp-file (pathname &key (if-does-not-exist :error) (element-type 'character))
  (with-open-file (in pathname
                      :direction :input
                      :if-does-not-exist if-does-not-exist
                      :element-type element-type)
    (let* ((size (file-length in))
           (array (make-array size :element-type element-type :fill-pointer t))
           (actual-length (read-sequence array in)))
      (setf (fill-pointer array) actual-length)
      array)))

(defun slurp-file-lines (pathname &key (if-does-not-exist :error))
  "Read lines from file desginated by pathname.  Uses the first line terminator
 (#\newline, #\linefeed, or #\newline #\linefeed) found for the entire file."
  (with-open-file-or-stream (in pathname :direction :input :if-does-not-exist if-does-not-exist)
    (multiple-value-bind (first-line missing-p line-terminator)
                         (smart-read-line in nil nil)
      (declare (ignore missing-p))
      (values `(,@(when first-line (list first-line))
                ,@(loop for line = (smart-read-line in nil nil nil line-terminator)
                        while line collect line))
              line-terminator))))

;; TODO: This is a quick hack for now, use real URI parsing, etc. later.
(defun string-uri-p (string)
  (let ((upcased (string-upcase string)))
    (or (string-starts-with "HTTP://" upcased)
	(string-starts-with "HTTPS://" upcased)
	(string-starts-with "FILE://" upcased))))
  
(defun get-string (location-spec &key base-location)
  "Get text from LOCATION-SPEC. LOCATION-SPEC can be a URI with scheme HTTP, HTTPS, or FILE — or a pathspec. Second return value is pathname actually read from, if any." ;; Should also support URIs more generally and return those in second value.
  (typecase location-spec
    (pathname
     (let ((merged (if base-location
		       (merge-pathnames
			(make-pathname :name nil :type nil :defaults base-location)
			location-spec)
		       location-spec)))
       (values (slurp-file merged) merged)))
    (t (let ((upcased (string-upcase location-spec)))
	 (cond
	   ((or (string-starts-with "HTTP://" upcased)
		(string-starts-with "HTTPS://" upcased))
	    (dex:get location-spec))
	   ((string-starts-with "FILE://" upcased)
	    (let ((pathname (pathname (subseq location-spec 7))))
	      (values (slurp-file pathname) pathname)))
	   (t (values (slurp-file location-spec) (pathname location-spec))))))))

(defun resolve-json-input (spec)
  "Resolve STRING eiher to string content via URI lookup, pathname, or stream. For use with CL-JSON:DECODE-JSON."
  (let ((upcased (string-upcase spec)))
    (cond ((streamp spec) spec)
	  ((or (string-starts-with "HTTP://" upcased)
	       (string-starts-with "HTTPS://" upcased))
	   (dex:get spec))
	  ((string-starts-with "FILE://" upcased)
	   (slurp-file (subseq location-spec 7)))
	  (t (pathname spec)))))
 
(defun mklist (x) (if (listp x) x (list x)))
