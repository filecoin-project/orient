(defpackage interface
  (:use "COMMON-LISP" "ORIENT" "CL-JSON")
  (:export :load-pipeline :load-transform))

(in-package "INTERFACE")

(defun load-pipeline (json-pathspec)
  (let* ((json (decode-json-from-source (pathname json-pathspec)))
	 (transforms (mapcar #'transform<-parsed-json json)))
    transforms))

(defun load-transform (json-pathspec)
  (let* ((json (decode-json-from-source (pathname json-pathspec))))
    (transform<-parsed-json json)))

(defun load-data-map (json-pathspec)
  (let ((json (decode-json-from-source (pathname json-pathspec))))
    json
    ;; FIXME: implement
    ))

(defmethod encode-json ((data-map data-map) &optional stream)
 (encode-json-alist (data-map-pairs data-map :dotted t) stream))

(defmethod encode-json ((signature signature) &optional stream)
  (encode-json-plist
   `(:inputs ,(signature-inputs signature) :outputs ,(signature-outputs signature)) stream))

;; TODO: roundtrip tests
(defun transform<-parsed-json (transform-spec)
  (let* ((transform-alist (cdr (assoc :transform transform-spec)))
	 (implementation-spec (cdr (assoc :implementation transform-alist)))
	 (package-string (cdr (assoc :module implementation-spec)))
	 (package-name (when package-string (string-upcase package-string)))
	 (symbol-name (cdr (assoc :name implementation-spec)))
	 (symbol (find-symbol (camel-case-to-lisp symbol-name) package-name))
	 (implementation (symbol-value symbol))
	 (string-inputs (cdr (assoc :input transform-alist)))
	 (string-outputs (cdr (assoc :output transform-alist)))
	 (inputs (if package-name
		     (mapcar (lambda (name)
			       (intern (string-upcase name) package-name)) string-inputs)
		     string-inputs))
	 (outputs (if package-name
		     (mapcar (lambda (name) (intern (string-upcase name) package-name)) string-outputs)
		     string-outputs))
	 (signature (make-instance 'signature :inputs inputs :outputs outputs)))
    
    (make-instance 'transform :signature signature :implementation implementation)))


