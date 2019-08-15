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
