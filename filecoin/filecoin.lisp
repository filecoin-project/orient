(in-package :filecoin)
(in-suite filecoin-suite)

;;; TODO: Add a function to check data against schema -- which will make more sense once schema is typed.
;;; Include option to validate that provided parameters exclude those which must be computed.

(defschema filecoin-requirements-schema
    "Filecoin Requirements -- sine qua non (without which not)"
  (must-have-filecoin "WE MUST HAVE FILECOIN -- SWEET, SWEET FILECOIN.")
  (space-gap-satisfied "Is the actual space gap less than or equal to the maximum allowable space gap?")
  (filecoin-requirements-satisfied "Are the Filecoin requirements all satisfied?")
  (minimum-one-year-ROI "Minimum allowable ROI after one year.")
  (minimum-two-year-ROI "Minimum allowable ROI after two years.")
  (minimum-three-year-ROI "Minimum allowable ROI after three years.")
  (minimum-storage-to-proof-size-ratio "Minimum necessary storage to on-chain proof ratio.")
  (profit "… Profit."))

(defparameter *filecoin-requirements*
  (tuple (must-have-filecoin t)
	 (maximum-allowable-space-gap 0.02)
	 (minimum-one-year-ROI -2)
	 (minimum-two-year-ROI .25)
	 (minimum-three-year-ROI 1.0)
	 ;; TODO: Justify this legacy requirement with grounded calculations.
	 (minimum-storage-to-proof-size-ratio (/ (* 1024 1024 1024 1024)  25600))
	 ))

(defconstraint-system filecoin-requirements-constraint-system
    ((space-gap-satisfied (<= zigzag-space-gap maximum-allowable-space-gap))
     (one-year-ROI-satisfied (>= one-year-ROI minimum-one-year-ROI))
     (two-year-ROI-satisfied (>= two-year-ROI minimum-two-year-ROI))
     (three-year-ROI-satisfied (>= three-year-ROI minimum-three-year-ROI))
     (filecoin-ROI-requirement-1 (and one-year-ROI-satisfied two-year-ROI-satisfied))
     (filecoin-ROI-requirement (and filecoin-ROI-requirement-1 three-year-ROI-satisfied))
     (filecoin-storage-ratio-satisfied (>= storage-to-proof-size-ratio minimum-storage-to-proof-size-ratio))
     (filecoin-porep-security-requirements-satisfied (== space-gap-satisfied))
     (filecoin-porep-requirements-satisfied (and filecoin-porep-security-requirements-satisfied filecoin-storage-ratio-satisfied))
     (filecoin-requirements-satisfied (and filecoin-ROI-requirement filecoin-porep-requirements-satisfied))
     (profit (and must-have-filecoin filecoin-requirements-satisfied))))

(defun filecoin-requirements-system ()
  (make-instance 'system
		 :subsystems (list (find-system 'filecoin-requirements-constraint-system))
		 :schema 'filecoin-requirements-schema
		 :data (list *filecoin-requirements*)))

(test zigzag-system
  "Test ZigZag constraint system."
  (let* ((result (ask (zigzag-system) '(seal-time))))
    (is (same (relation (seal-time) (289372.25))
	      result))))

(defun filecoin-system (&key no-zigzag)
  (let* ((subsystems (if no-zigzag
			 (list (performance-system) (filecoin-requirements-system))
		         (list (performance-system) (zigzag-system) (filecoin-requirements-system)))))
    (make-instance 'system :subsystems subsystems)))

(test filecoin-defaults
  "Test and assert results of solving with defaults."
  (let* ((result (ask (filecoin-system) '(seal-cost seal-time)))
	 (expected
	  (relation (SEAL-COST SEAL-TIME) (108.01221 289372.25))))
    (is (same expected result))))

