(in-package :filecoin)
(in-suite filecoin-suite)

;;; TODO: Add a function to check data against schema -- which will make more sense once schema is typed.
;;; Include option to validate that provided parameters exclude those which must be computed.

(defschema filecoin-requirements-schema
    "Filecoin Requirements -- sine qua non (without which not)"
  (must-have-filecoin "WE MUST HAVE FILECOIN -- SWEET, SWEET FILECOIN.")
  (space-gap-satisfied "Is the actual space gap less than or equal to the maximum allowable space gap?")
  (filecoin-requirements-satisfied "Are the Filecoin requirements all satisfied?")
  (minimum-one-year-FGR "Minimum allowable FGR after one year.")
  (minimum-two-year-FGR "Minimum allowable FGR after two years.")
  (minimum-three-year-FGR "Minimum allowable FGR after three years.")
  (minimum-storage-to-proof-size-ratio "Minimum necessary storage to on-chain proof ratio.")
  (profit "… Profit."))

(defparameter *filecoin-requirements*
  (tuple (must-have-filecoin t)
	 (maximum-allowable-space-gap 0.02)
	 (minimum-one-year-FGR -2)
	 (minimum-two-year-FGR .25)
	 (minimum-three-year-FGR 1.0)
	 ;; TODO: Justify this legacy requirement with grounded calculations.
	 (minimum-storage-to-proof-size-ratio (/ (* 1024 1024 1024 1024)  25600))
	 ))

(defconstraint-system filecoin-requirements-constraint-system
    ((space-gap-satisfied (<= zigzag-space-gap maximum-allowable-space-gap))
     (one-year-FGR-satisfied (>= one-year-FGR minimum-one-year-FGR))
     (two-year-FGR-satisfied (>= two-year-FGR minimum-two-year-FGR))
     (three-year-FGR-satisfied (>= three-year-FGR minimum-three-year-FGR))
     (filecoin-FGR-requirement-1 (and one-year-FGR-satisfied two-year-FGR-satisfied))
     (filecoin-FGR-requirement (and filecoin-FGR-requirement-1 three-year-FGR-satisfied))
     (filecoin-storage-ratio-satisfied (>= storage-to-proof-size-ratio minimum-storage-to-proof-size-ratio))
     (filecoin-porep-security-requirements-satisfied (== space-gap-satisfied))
     (filecoin-porep-requirements-satisfied (and filecoin-porep-security-requirements-satisfied filecoin-storage-ratio-satisfied))
     (filecoin-requirements-satisfied (and filecoin-FGR-requirement filecoin-porep-requirements-satisfied))
     (profit (and must-have-filecoin filecoin-requirements-satisfied))))

(defun filecoin-requirements-system ()
  (make-instance 'system
		 :subsystems (list (find-system 'filecoin-requirements-constraint-system))
		 :schema 'filecoin-requirements-schema
		 :data (list *filecoin-requirements*)))

(defun filecoin-system (&key no-zigzag)
  (let* ((subsystems (if no-zigzag
			 (list (performance-system) (filecoin-requirements-system))
		         (list (performance-system) (zigzag-system) (filecoin-requirements-system)))))
    (make-instance 'system :subsystems subsystems)))

;; FIXME: Get JSON working again.
#+(or)
(test filecoin-system-json
  ;; TODO: Transformation interfaces, so we don't have to specify :PARSING-ONLY.
  (interface:test-roundtrip :system (filecoin-system) :parsing-only t))

