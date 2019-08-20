(defpackage filecoin
  (:use :common-lisp :orient :orient.base.util :it.bese.FiveAm)
  (:import-from :fset :with)
  (:nicknames :fc)
  (:export
   :constraints
   :GiB-seal-cycles :sector-GiB
   :replication-time :replication-time-per-GiB
   :fgr-months
   :gib-seal-cost :gib-hour-seal-investment :seal-time :GiB-seal-time :sector-size :up-front-compute-cost :total-up-front-cost :monthly-income
   :annual-income :seal-cost
   :layers :layer-index :lowest-time :circuit-time :hashing-time
   :optimal-beta-merkle-height
   
   :total-challenges :total-zigzag-challenges :total-zigzag-constraints :total-zigzag-constraints-x
   :storage-to-proof-size-ratio :storage-to-proof-size-float
   :total-hashing-time :total-circuit-time :wall-clock-seal-time :wall-clock-seal-time-per-gib :seal-parallelism
   
   ;;Security
   :zigzag-delta :zigzag-lambda :zigzag-epsilon :zigzag-taper :zigzag-soundness
   :zigzag-basic-layer-challenges :zigzag-basic-layer-challenge-factor
   :zigzag-space-gap :total-untapered-challenges

   :zigzag-layers :zigzag-layer-challenges :one-year-fgr :two-year-fgr :three-year-fgr
   :filecoin-system :performance-system :zigzag-system :zigzag-security-system
   :max-beta-merkle-height

   :*performance-defaults* :*spedersen* :*gpu-speedup* :*blake2s-speedup*

   ))

(defpackage filecoin.theory
  (:use :common-lisp :orient :orient.base.util :it.bese.FiveAm :cl-permutation)
    (:shadowing-import-from :fset
			  ;; Shadowed type/constructor names
			  #:set #:map
			  ;; Shadowed set operations
			  #:union #:intersection #:set-difference #:complement
			  ;; Shadowed sequence operations
			  #:first #:last #:subseq #:reverse #:sort #:stable-sort
			  #:reduce
			  #:find #:find-if #:find-if-not
			  #:count #:count-if #:count-if-not
			  #:position #:position-if #:position-if-not
			  #:remove #:remove-if #:remove-if-not
			  #:substitute #:substitute-if #:substitute-if-not
			  #:some #:every #:notany #:notevery)
    (:shadowing-import-from :cl-permutation :cardinality)
    (:nicknames :fct))

(defpackage filecoin.scratch
  (:use :common-lisp :orient :orient.base.util :it.bese.FiveAm :filecoin.theory :fct)
  (:nicknames :fc-scratch))
  
(in-package :filecoin)

(def-suite filecoin-suite)
(in-suite filecoin-suite)

(defparameter *baseline-mhz* 5000.0 "Clock speed on which original benchmarks were based. Unit: MHz")
(defparameter *baseline-ghz* (* 1e-3 *baseline-mhz*))

(defconstant KiB 1024)
(defconstant MiB (* KiB 1024))
(defconstant GiB (* MiB 1024))

(defparameter *defaults*
  (tuple
   (node-bytes 32)
   (sector-GiB 32)))

(defun filecoin-json-directory () (project-merge "filecoin/json/"))

  

