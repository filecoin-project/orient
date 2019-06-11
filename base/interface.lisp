(defpackage interface
  (:use "COMMON-LISP" "ORIENT" "CL-JSON")
  (:import-from "FSET" :wb-map :convert)
  (:export :load-pipeline :load-transformation :load-tuple :load-json :<-json))

(in-package "INTERFACE")

(defgeneric load-json (type-spec json-pathspec)
  (:method ((type t) (json-pathspec t))
    (load-json type (pathname json-pathspec)))
  (:method ((type t)(json-pathname pathname))    
    (let ((json (decode-json-from-source (pathname json-pathname))))
      (<-json type json))))

(defgeneric <-json (type json)
  (:method ((type (eql :pipeline)) (json list))
    (mapcar #'transformation<-parsed-json json))
  (:method ((type (eql :transformation)) (json t))
    (transformation<-parsed-json json))
  (:method ((type (eql :tuple)) (json list))
    (make-tuple json t)))

(defun load-pipeline (json-pathspec)
  (load-json :pipeline json-pathspec))

(defun load-transformation (json-pathspec)
  (load-json :transformation json-pathspec))

(defun load-tuple (json-pathspec)
  (load-json :tuple json-pathspec))

(defmethod encode-json ((tuple wb-map) &optional stream)
 (encode-json-alist (convert 'list tuple) stream))

(defmethod encode-json ((signature signature) &optional stream)
  (encode-json-plist
   `(:input ,(signature-input signature) :output ,(signature-output signature)) stream))

;; TODO: roundtrip tests
;; TODO: there must be a cleaner/clearer way to do this.
(defun transformation<-parsed-json (transformation-spec)
  (let* ((transformation-alist (cdr (assoc :transformation transformation-spec))) ;; TODO: Should the spec just use :transformation?
	 (implementation-spec (cdr (assoc :implementation transformation-alist)))
	 (package-string (cdr (assoc :module implementation-spec)))
	 (package-name (when package-string (string-upcase package-string)))
	 (symbol-name (cdr (assoc :name implementation-spec)))
	 (symbol (find-symbol (camel-case-to-lisp symbol-name) package-name))
	 (implementation symbol)
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
