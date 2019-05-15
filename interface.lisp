(defpackage interface
  (:use "COMMON-LISP" "ORIENT" "CL-JSON")
  (:export :load-pipeline :load-transformation))

(in-package "INTERFACE")

(defun load-pipeline (json-pathspec)
  (let* ((json (decode-json-from-source (pathname json-pathspec)))
	 (transformations (mapcar #'transformation<-parsed-json json)))
    transformations))

(defun load-transformation (json-pathspec)
  (let* ((json (decode-json-from-source (pathname json-pathspec))))
    (transformation<-parsed-json json)))

(defun load-data-map (json-pathspec)
  (let ((json (decode-json-from-source (pathname json-pathspec))))
    json
    ;; FIXME: implement
    ))

(defmethod encode-json ((data-map data-map) &optional stream)
 (encode-json-alist (data-map-pairs data-map :dotted t) stream))

(defmethod encode-json ((signature signature) &optional stream)
  (encode-json-plist
   `(:input ,(signature-input signature) :output ,(signature-output signature)) stream))

;; TODO: roundtrip tests
(defun transformation<-parsed-json (transformation-spec)
  (let* ((transformation-alist (cdr (assoc :transformation transformation-spec))) ;; TODO: Should the spec just use :transformation?
	 (implementation-spec (cdr (assoc :implementation transformation-alist)))
	 (package-string (cdr (assoc :module implementation-spec)))
	 (package-name (when package-string (string-upcase package-string)))
	 (symbol-name (cdr (assoc :name implementation-spec)))
	 (symbol (find-symbol (camel-case-to-lisp symbol-name) package-name))
	 (implementation (symbol-value symbol))
	 (string-inputs (cdr (assoc :input transformation-alist	)))
	 (string-outputs (cdr (assoc :output transformation-alist)))
	 (input (if package-name
		    (mapcar (lambda (name)
			      (intern (string-upcase name) package-name)) string-inputs)
		     string-inputs))
	 (output (if package-name
		     (mapcar (lambda (name) (intern (string-upcase name) package-name)) string-outputs)
		     string-outputs))
	 (signature (make-instance 'signature :input input :output output)))
    (make-instance 'transformation :signature signature :implementation implementation)))
