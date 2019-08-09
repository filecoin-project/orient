(in-package :filecoin)
(in-suite filecoin-suite)

;;;; System constructors.

(defun performance-system (&key isolated)
  (make-instance 'system
		 :subsystems (list (find-system 'performance-constraint-system))
		 :data (list *performance-defaults* (if isolated
							*isolated-performance-defaults*
							*integrated-performance-defaults*))))

(defun zigzag-security-system (&key isolated no-aggregate)
  (make-instance 'system
		 :components `(,(component ('compute-zigzag-tapered-layers))
				,@(when (not no-aggregate)
				    (list (component ('compute-zigzag-tapered-layers))
					  (component ('compute-total-zigzag-challenges)))))
		 :subsystems (list (find-system 'zigzag-security-constraint-system))
		 :data (if isolated
			   (list (tuple (layers 10)) *default-zigzag-security*)
			   (list *default-zigzag-security*))))

(defun zigzag-system (&key no-aggregate)
  (make-instance 'system
		 :components (when (not no-aggregate)
			       (list
				(component ('compute-total-zigzag-performance))))
		 :subsystems (list (find-system 'zigzag-constraint-system)
				   ;; FIXME: If these subsystems are provided in the opposite order,
				   ;; something breaks.
				   (zigzag-security-system :no-aggregate no-aggregate))
		 :data (list *defaults*
			     *zigzag-defaults*
			     (zigzag-bench-data))))
