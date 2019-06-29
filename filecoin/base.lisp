(defpackage filecoin
  (:use :common-lisp :orient :it.bese.FiveAm)
  (:import-from :fset :with)
  (:nicknames :fc)
  (:export
   :GiB-seal-cycles :sector-GiB
   :roi-months
   :seal-cost :seal-time :GiB-seal-time :sector-size :up-front-compute-cost :total-up-front-cost :monthly-income :annual-income :layers
   :total-challenges :total-zigzag-challenges :storage-to-proof-size-ratio :storage-to-proof-size-float
   :zigzag-layers :zigzag-layer-challenges :one-year-roi :two-year-roi :three-year-roi
   :zigzag-soundness
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
