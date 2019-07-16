(defpackage filecoin
  (:use :common-lisp :orient :it.bese.FiveAm)
  (:import-from :fset :with)
  (:nicknames :fc)
  (:export
   :GiB-seal-cycles :sector-GiB
   :replication-time :replication-time-per-GiB
   :roi-months
   :seal-cost :seal-time :GiB-seal-time :sector-size :up-front-compute-cost :total-up-front-cost :monthly-income :annual-income :layers

   :total-challenges :total-zigzag-challenges :total-zigzag-constraints :total-zigzag-constraints-x
   :storage-to-proof-size-ratio :storage-to-proof-size-float

   ;;Security
   :zigzag-delta :zigzag-lambda :zigzag-epsilon :zigzag-taper :zigzag-soundness
   :zigzag-basic-layer-challenges :zigzag-basic-layer-challenge-factor
   :zigzag-space-gap :total-untapered-challenges
   
   :zigzag-layers :zigzag-layer-challenges :one-year-roi :two-year-roi :three-year-roi
   :filecoin-system :performance-system :zigzag-system :zigzag-security-system
   :*performance-defaults*))

(in-package :filecoin)

(def-suite filecoin-suite)
(in-suite filecoin-suite)

(defconstant KiB 1024)
(defconstant MiB (* KiB 1024))
(defconstant GiB (* MiB 1024))

(defparameter *defaults*
  (tuple
   (node-bytes 32)
   (sector-GiB 32)))
