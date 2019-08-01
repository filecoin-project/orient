(in-package orient)

(def-suite presentation-suite)
(in-suite presentation-suite)

(defgeneric format-value (format value)
  (:method ((format (eql :org)) (thing t))
    (princ-to-string (representation thing)))
  (:method ((format (eql :org)) (relation relation))
    ;; Remove the RELATION prefix and we should have a list of lists, to be formatted as an org-mode table.
    ;; AKA the world's fanciest CDR.
    (destructuring-bind (ignore attributes &rest rows)
	(representation relation)
      (declare (ignore ignore))
      `(,attributes ,@rows)))
  (:method ((format (eql :org)) (tuple wb-map))
    (format-value format (ensure-relation tuple))))

(defgeneric present-data (format thing system &rest keys)
  (:method ((format t) (thing t) (null null) &key) (format-value format thing))
  
  (:method ((format t) (thing t) (null null) &key)
    "NULL"))
 

(defun org-present (&rest args)
  (apply #'present-data :org args))
