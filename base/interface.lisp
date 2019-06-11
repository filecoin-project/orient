(defpackage orient.interface
  (:use :common-lisp :orient :cl-json :it.bese.FiveAm)
  (:import-from :fset :wb-map :convert)
  (:shadowing-import-from :fset :set)
  (:export :load-pipeline :load-transformation :load-tuple :load-json :<-json)
  (:nicknames :interface))

(in-package "INTERFACE")
(def-suite interface)
(in-suite interface)

(defgeneric load-json (type-spec json-pathspec)
  (:method ((type t) (json-pathspec t))
    (load-json type (pathname json-pathspec)))
  (:method ((type t) (json-pathname pathname))    
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

(defun signature-plist (signature)
  `(:input ,(convert 'list (signature-input signature)) :output ,(convert 'list (signature-output signature))))

(defmethod encode-json ((signature signature) &optional stream)
  (encode-json-plist
   (signature-plist signature)
   stream))

(defmethod encode-json ((transformation transformation) &optional stream)
  ;; TODO: handle SOURCE.
  (with-object (stream)
    (as-object-member (:transformation stream)
      (encode-json-plist
       ;; TODO: It would be better to package signatures as first-class items rather than unwrapping into the transformation. Try to have that changed.
       `(,@(signature-plist (transformation-signature transformation))
	   :implementationdecod
	   ,(transformation-implementation transformation))
       stream))))

;; TODO: roundtrip tests
;; TODO: there must be a cleaner/clearer way to do this.
(defun transformation<-parsed-json (transformation-spec)
  (let* ((transformation-alist (cdr (assoc :transformation transformation-spec))) ;; TODO: Should the spec just use :transformation?
	 (implementation-spec (cdr (assoc :implementation transformation-alist)))
	 (package-string (cdr (assoc :module implementation-spec)))
	 (package-name (if package-string
			   (string-upcase package-string)
			   (package-name *package*)))
	 (symbol-name (cdr (assoc :name implementation-spec)))
	 (symbol (find-symbol (simplified-camel-case-to-lisp symbol-name) package-name))
	 (implementation (make-instance 'implementation :name symbol-name :module package-name))
	 (string-inputs  (cdr (assoc :input transformation-alist)))
	 (string-outputs (cdr (assoc :output transformation-alist)))
	 (input (if package-name
		    (mapcar (lambda (name)
			      (intern (string-upcase name) package-name))
			    string-inputs)
		    string-inputs))
	 (output (if package-name
		     (mapcar (lambda (name) (intern (string-upcase name) package-name)) string-outputs)
		     string-outputs))

	 (signature (make-signature input output)))
    (make-instance 'transformation :signature signature :implementation implementation)))

(test transformation
  (let* ((transformation (transformation ((a b c) ~> (d e f)) == implementation))
	 (json-string (encode-json-to-string transformation))
	 (json (decode-json-from-string json-string))
	 (returned-transformation (<-json :transformation json)))
    (is (same returned-transformation transformation))	 
  ))
