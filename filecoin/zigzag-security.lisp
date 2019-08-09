(in-package :filecoin)
(in-suite filecoin-suite)

(defschema zigzag-security-schema
    "ZigZag Security"
  (zigzag-soundness "ZigZag soundness: Unit fraction")
  (zigzag-lambda "ZigZag soundness: Unit bits")
  (zigzag-epsilon "Maximum allowable deletion (space tightness): Unit: fraction")
  (zigzag-delta "Maximum allowable cheating on labels (block corruption)")
  (zigzag-basic-layer-challenges "Multiple of lambda challenges per layer, without tapering optimization.")
  (zigzag-basic-layer-challenge-factor "Number of challenges which, when multiplied by lambda, yields the number of challenges per layer without tapering optimization.")
  (zigzag-space-gap "Maximum allowable gap between actual and claimed storage. Unit: fraction")
  (zigzag-layer-challenges "Number of challenges in this (indexed) layer of ZigZag PoRep. Unit: integer")

  (layers "Number of layers specified for this construction (not necessarily same as calculated from security parameters).")
  )

(defconstraint-system zigzag-security-constraint-system
    ((zigzag-lambda (log zigzag-soundness 0.5))
     (zigzag-space-gap (+ zigzag-epsilon zigzag-delta))
     (zigzag-basic-layer-challenge-factor (/ 1 zigzag-delta))
     (zigzag-basic-layer-challenges (* zigzag-lambda zigzag-basic-layer-challenge-factor))
     (zigzag-layers (compute-zigzag-layers zigzag-epsilon zigzag-delta))
     (total-untapered-challenges (* zigzag-layers zigzag-basic-layer-challenges))

     #+(or) ;; TODO: Allow specifying like this.
     (zigzag-layers (+ (log (/ 1
			       (* 3 (- zigzag-epsilon (* 2 zigzag-delta))))
			    2)
		       4))
     (total-challenges (== total-zigzag-challenges)))
  :schema 'zigzag-security-schema)

(defparameter *default-zigzag-security*
  (tuple
   (zigzag-lambda 8)
   (zigzag-taper (/ 1 3))
   (zigzag-epsilon 0.007)
   (zigzag-delta 0.003)))
