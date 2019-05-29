(defpackage filecoin
  (:use :common-lisp :orient :it.bese.FiveAm)
  (:nicknames :fc))

(in-package :filecoin)

(def-suite filecoin-suite)
(in-suite filecoin-suite)

(defconstant KiB 1024)
(defconstant MiB (* KiB 1024))
(defconstant GiB (* MiB 1024))

(defparameter *hash-functions* (relation (hash-function-name hash-function-time hash-function-constraints hash-function-size)
					 (:pedersen 0.000017993 1152 32)
					 (:blake2s 1.6055e-7 10324 32)))

(defparameter *performance-defaults*
  (tuple
   (investment 100000.0)
   (comparable-monthly-income 50000.0)
;   (seal-cost 10) ;; We should derive this or reject/refine it.
   (aws-storage-price 23.0)
   (miner-months-to-capacity 3)
   (roi-interval-months 6)
   (TiB-drive-cost 300.0)
   (cpu-ghz-cost 10.0)
   (GiB-replication-cycles (* 13000 4300.0)) ;; This will eventually calculated elsewhere.
   ))

(defparameter *alt-performance-defaults*
  (tuple
   (investment 100000.0)
   (comparable-monthly-income 50000.0)
   (seal-cost 661.7256) ;; Here, use the previously calculated value and try to calculate something else (GiB-replication-cycles) backward.
   ;;(seal-cost 640)
   (aws-storage-price 23.0)
   (miner-months-to-capacity 3)
   (roi-interval-months 6)
   (TiB-drive-cost 300.0)
   (cpu-ghz-cost 10.0)
;   (GiB-replication-cycles (* 13000 4300))
   ))

(defparameter *zigzag-defaults* (tuple
				 (merkle-hash-function-name :pedersen)
				 (kdf-hash-function-name :blake2s)
				 (partition-challenges 100)
				 (single-circuit-proof-size 192) ;; Groth16 -- eventually should allow selection of proving systems.
				 (sloth-iter 0)
				 (layers 10)
				 (degree 5)
				 (base-degree 8)

				 (total-challenges 8000)

				 (total-zigzag-non-hashing-constraints 0) ;; TODO
				 ;; Need from benchmarks
				 (single-sloth-iteration-time 123) ;; BOGUS
				 (bench-circuit-proving-time (* 2.785 60))
				 (bench-circuit-constraints 16e6)
				       ;;; TEMP
					;(single-node-encoding-time 123)
				 ))

(defparameter *defaults*
  (tuple
   ;; FIXME: This depends on the processor speed of the machine that produced the benchmark. We should express benchmarks in cycles.
   ;; (pedersen-hash-seconds 0.000017993)
   ;; (pedersen-hash-constraints 1152)

   ;; (blake-hash-seconds 1.6055e-7)
   ;; (blake-hash-constraints 10324)

   ;; (hash-functions (relation (hash-function-name hash-function-time hash-function-constraints)
   ;; 			     (:pedersen 0.000017993 1152)
   ;; 			     (:blake2s 1.6055e-7 10324)))

   (node-bytes 32)
   (sector-size (* 1 GiB))))

(defschema filecoin
    "Everything Filecoin"
  (investment "Dollar cost of infrastructure purchase required to mine at scale.")
  (comparable-monthly-income "Expected dollar income for selling storage equivalent to what can be sealed for `investment`.")
  (seal-cost "Dollar cost of investment required to seal one GiB in one hour at scale.")
  (aws-storage-price "Dollar cost of one TiB storage from AWS S3 for one month.")
  (miner-months-to-capacity "Months it should take a miner to reach full storage capacity.")
  (roi-interval-months "Months over which a miner should see return on investment.")
  
  (pedersen-hash-seconds "Seconds required to hash 64 bytes with pedersen hashing.")
  (pedersen-constraints "Number of circuit constraints required to prove pedersen hashing of 64 bytes.")

  (blake2s-hash-second "Seconds required to hash 64 bytes with blake2s.")
  (blake2s-constraints "Number of circuit constraints required to prove blake2s hashing of 64 bytes.")

  ;;;; Optional parameters.
  (merkle-hash-function-name "Hash function name. Type is an enumeration of { PEDERSEN, BLAKE2S } (may change when types are implemented).")

  (hash-functions "Relation containing hash function names, times (should be cycles), and constraints.")

  (sector-size "Size in bytes of a sealed sector."))

#|
From Übercalc Compoments document:

Performance: for every $10 spent (at scale), it must be possible to seal 1GiB/hour.

Notes on how this is derived:

Miner needs a reasonable return, say $50k/yr for investment.
Compare with pure storage service business.
Upper bound on storage pricing is AWS S3.
S3 = ~$23/TiB-month = $276 / TiB-year [Tune this variable down to something more realistic.]
$50k / $276 = ~181TiB
Miner should be at capacity in ‘3 months’.
So must seal 181TiB/3months = 60TiB/month = ~2TiB/day = ~85GiB/hr
Pick some ROI interval: say 6 months.
Assume $10 / GiB/hr sealing: then sealing 85GiB/hr costs $850 up-front.
18 10TiB drives at $300 = $5400 up-front.
Should amortize costs over 6 months. Compare with profits.
Express up-front investment in terms of number of months of profit.
This is one of the variables we can tune.
TODO: block reward profitability can/should be folded into this as an incremental improvement, but ignoring block reward is the right way to get best long-term numbers.

|#

(defschema filecoin-price-performance
    "Filecoin price performance."
  (aws-price-TiB-year  "Dollar cost of one TiB storage from AWS S3 for one year.")
  (investment "Dollar cost of infrastructure purchase required to mine at scale.")
  (comparable-monthly-income "Expected dollar income for selling storage equivalent to what can be sealed for `investment`.")
  (seal-cost "Dollar cost of investment required to seal one GiB in one hour at scale.")
  (aws-storage-price "Dollar cost of one TiB storage from AWS S3 for one month.")
  (miner-months-to-capacity "Months it should take a miner to reach full storage capacity.")
  (roi-interval-months "Months over which a miner should see return on investment.")

  (annual-TiB "Amount of storage, in TiB, which must be brought online per year.")
  (monthly-TiB "Amount of storage, in TiB, which must be brought online per month.")
  (daily-TiB "Amount of storage, in TiB, which must be brought online per day.")
  (hourly-TiB "Amount of storage, in TiB, which must be brought online per hour.")
  (hourly-GiB "Amount of storage, in GiB, which must be brought online per hour.")

  (replication-cycles-per-hour "CPU cycles required to replicate at required rate for one hour.")
  (replication-cycles-per-minute "CPU cycles required to replicate at required rate for one minute.")
  (replication-cycles-per-second "CPU cycles required to replicate at required rate for one second.")
  (GiB-replication-cycles "Total CPU cycles required to replicate 1 GiB.")
  (needed-ghz "Total GhZ capacity needed to seal at the required rate.")
  (up-front-drive-cost "Dollar cost of hard drives required to generate MONTHLY-INCOME.")
  (up-front-compute-cost "Dollar cost of investment needed to purchase sufficient compute power to generate MONTHLY-INCOME.")
  (total-up-front-cost "Total dollar cost of investment needed to generate MONTHLY-INCOME."))

(defconstraint-system performance-constraint-system
    ((aws-price-TiB-year (* aws-storage-price 12))
     (annual-TiB (/ comparable-monthly-income aws-price-TiB-year))
     (monthly-TiB (/ annual-TiB miner-months-to-capacity))
     (daily-TiB (/ monthly-TiB (/ 365 12)))
     (hourly-TiB (/ daily-Tib 24))
     (hourly-GiB (* hourly-TiB 1024))
     (up-front-drive-cost (* TiB-drive-cost annual-TiB))
     (replication-cycles-per-hour (* hourly-GiB GiB-replication-cycles))
     (replication-cycles-per-minute (* replication-cycles-per-hour 60))
     (replication-cycles-per-second (* replication-cycles-per-minute 60))
     (needed-ghz (/ replication-cycles-per-second 1e9))
     (total-up-front-cost (+ up-front-compute-cost up-front-drive-cost))
     (up-front-compute-cost (/ needed-ghz cpu-ghz-cost))
     (seal-cost (/ total-up-front-cost hourly-GiB)))
  :schema (find-schema 'filecoin-price-performance))

(defun performance-system ()
  (let ((pcs (find-system 'performance-constraint-system)))
    (make-instance 'system
		   :components (system-components pcs)
		   :schema (system-schema pcs)
		   :data (list *performance-defaults*))))

(test performance-test
  "Test performance system, with default values -- a sanity/regression test for now."
  (is (same (tuple (seal-cost 661.7256))
	    (ask (performance-system) '(seal-cost)))
      "correctly calculates SEAL-COST"))

;;; TODO: Add a function to check data against schema -- which will make more sense once schema is typed.
;;; Include option to validate that provided parameters exclude those which must be computed.

(defschema merkle-tree-schema
    "PoRep  Merkle Trees"
  (node-bytes "The number of bytes in a node -- must also be the hash digest size.") ; TODO: Move to more general schema.
  (merkle-tree-leaves "Number of leaves in the merkle tree.")
  (merkle-tree-height "Height of the merkle tree, including leaves and root.")
  (merkle-tree-hash-count "Total number of hashes required to construct the merkle tree (leaves are not hashed).")
  (merkle-inclusion-proof-hash-length "Number of hashes required for a merkle inclusion proof."))

(defconstraint-system merkle-tree-constraint-system
    ((merkle-tree-leaves (/ sector-size node-bytes))
     (merkle-inclusion-proof-hash-length-raw (log merkle-tree-leaves 2))
     (merkle-inclusion-proof-hash-length (integer merkle-inclusion-proof-hash-length-raw))
     (merkle-tree-height (== merkle-inclusion-proof-hash-length))
     (merkle-tree-hash-count (- merkle-tree-leaves 1))))

(test merkle-tree-constraint-system
  "Test merkle tree constraint system."

  ;; Compute MERKLE-TREE-HEIGHT from SECTOR-SIZE.
  (is (same
       (tuple (sector-size 32)
	      (node-bytes 4)
	      (merkle-tree-leaves 8)
	      (merkle-inclusion-proof-hash-length 3)
	      (merkle-inclusion-proof-hash-length-raw 3.0)
	      (merkle-tree-hash-count 7)
	      (merkle-tree-height 3))
       (solve-for 'merkle-tree-constraint-system '(merkle-tree-height) (tuple (sector-size 32) (node-bytes 4)))))

  ;; Compute SECTOR-SIZE from MERKLE-TREE-HEIGHT.
  (is (same
       (tuple (sector-size 32)
	      (node-bytes 4)
	      (merkle-tree-leaves 8)
	      (merkle-inclusion-proof-hash-length 3)
	      (merkle-inclusion-proof-hash-length-raw 3.0)
	      (merkle-tree-hash-count 7)
	      (merkle-tree-height 3))
       (solve-for 'merkle-tree-constraint-system '(sector-size) (tuple (merkle-tree-height 3) (node-bytes 4)))))

  ;; Compute SECTOR-SIZE from MERKLE-INCLUSION-PROOF-HASH-LENGTH.
  (is (same
       (tuple (sector-size 32)
	      (node-bytes 4)
	      (merkle-tree-leaves 8)
	      (merkle-inclusion-proof-hash-length 3)
	      (merkle-inclusion-proof-hash-length-raw 3)
	      (merkle-tree-hash-count 7)
	      (merkle-tree-height 3))
       (solve-for 'merkle-tree-constraint-system '(sector-size) (tuple (merkle-inclusion-proof-hash-length 3) (node-bytes 4))))))

(deftransformation select-merkle-hash-function
    ((merkle-hash-function-name hash-function-name hash-function-time hash-function-size hash-function-constraints)
     => (merkle-hash-function-constraints merkle-hash-function-time merkle-hash-function-size))
  (when (eql hash-function-name merkle-hash-function-name)
    `((,hash-function-constraints ,hash-function-time ,hash-function-size))))

#+(or)
(test select-merkle-hash-function
  (let* ((data (join (tuple (merkle-hash-function-name :pedersen)) *hash-functions*))
	 (result (apply-transformation 'select-merkle-hash-function data))
	 (expected (make-relation
		    (list
		     (tuple (HASH-FUNCTION-CONSTRAINTS 1152)
			    (HASH-FUNCTION-NAME :PEDERSEN)
			    (HASH-FUNCTION-SIZE 32)
			    (HASH-FUNCTION-TIME 1.7993e-5)
			    (MERKLE-HASH-FUNCTION-CONSTRAINTS 1152)
			    (MERKLE-HASH-FUNCTION-NAME :PEDERSEN)
			    (MERKLE-HASH-FUNCTION-SIZE 32)
			    (MERKLE-HASH-FUNCTION-TIME 1.7993e-5))))))
    (setq asdf expected fdsa result)
    (is (same expected result))))

#+(or)
(test select-kdf-hash-function
  (let* ((data (join (tuple (kdf-hash-function-name :blake2s)) *hash-functions*))
	 (result (apply-transformation 'select-kdf-hash-function data))
	 (expected (make-relation
		    (list
		     (TUPLE (HASH-FUNCTION-CONSTRAINTS 10324) (HASH-FUNCTION-NAME :BLAKE2S)
			    (HASH-FUNCTION-SIZE 32) (HASH-FUNCTION-TIME 1.6055e-7)
			    (KDF-HASH-FUNCTION-CONSTRAINTS 10324)
			    (KDF-HASH-FUNCTION-NAME :BLAKE2S) (KDF-HASH-FUNCTION-SIZE 32)
			    (KDF-HASH-FUNCTION-TIME 1.6055e-7))))))
    (setq asdf expected fdsa result)
    (is (same expected result))))


(deftransformation select-kdf-hash-function
    ((kdf-hash-function-name hash-function-name hash-function-time hash-function-size hash-function-constraints)
     => (kdf-hash-function-constraints kdf-hash-function-time kdf-hash-function-size))
  (when (eql hash-function-name kdf-hash-function-name)
    `((,hash-function-constraints ,hash-function-time ,hash-function-size))))

(defmacro define-hash-function-selector (prefix)
  (let ((selector-name (symbolconc 'select- prefix '-hash-function))
	(extractor-name (symbolconc 'extract- prefix '-hash-function-components))
	(-hash-function-name (symbolconc prefix '-hash-function-name))
	(-hash-function (symbolconc prefix '-hash-function))
	(-hash-function-constraints (symbolconc prefix '-hash-function-constraints))
	(-hash-function-time (symbolconc prefix '-hash-function-time))
	(-hash-function-size (symbolconc prefix '-hash-function-size)))
    `(progn
       (deftransformation ,selector-name ((hash-functions ,-hash-function-name)
					  -> (,-hash-function))
	 (extract (join (tuple (hash-function-name ,-hash-function-name)) hash-functions)))
       (deftransformation ,extractor-name ((,-hash-function) -> (,-hash-function-constraints ,-hash-function-time ,-hash-function-size))
	 (values (tref 'hash-function-constraints ,-hash-function)
	       (tref 'hash-function-time ,-hash-function)
	       (tref 'hash-function-size ,-hash-function))))))

(define-hash-function-selector merkle)
(define-hash-function-selector kdf)

(test select-merkle-hash-function
  (let* ((data (join (tuple (merkle-hash-function-name :pedersen))
		     (tuple (hash-functions *hash-functions*))))
	 (result (apply-transformation 'select-merkle-hash-function data))
	 (result (apply-transformation 'extract-merkle-hash-function-components result)))
    (is (same (tuple (HASH-FUNCTIONS *hash-functions*)
		     (MERKLE-HASH-FUNCTION-NAME :PEDERSEN)
		      (MERKLE-HASH-FUNCTION-CONSTRAINTS 1152)
		      (MERKLE-HASH-FUNCTION-SIZE 32)
		      (MERKLE-HASH-FUNCTION-TIME 1.7993e-5)
		      (MERKLE-HASH-FUNCTION (TPL (HASH-FUNCTION-NAME HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-SIZE)
						(:PEDERSEN 0.000017993 1152 32))))
	      result))))

(test select-kdf-hash-function
  (let* ((data (join (tuple (kdf-hash-function-name :blake2s))
		     (tuple (hash-functions *hash-functions*))))
	 (result (apply-transformation 'select-kdf-hash-function data))
	 (result (apply-transformation 'extract-kdf-hash-function-components result)))
    (is (same (tuple (HASH-FUNCTIONS *hash-functions*)
		     (KDF-HASH-FUNCTION-NAME :blake2s)
		     (KDF-HASH-FUNCTION-CONSTRAINTS 10324)
		     (KDF-HASH-FUNCTION-SIZE 32)
		     (KDF-HASH-FUNCTION-TIME 1.6055e-7)
		     (KDF-HASH-FUNCTION (TPL (HASH-FUNCTION-NAME HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-SIZE)
					     (:BLAKE2S 1.6055e-7 10324 32))))
	      result))))

(test multiple-hash-function-selectors
  (let* ((data (join (tuple (kdf-hash-function-name :blake2s)
			    (merkle-hash-function-name :pedersen))
		     (tuple (hash-functions *hash-functions*))))
	 (result (apply-transformation '(select-merkle-hash-function
					 select-kdf-hash-function
					 extract-merkle-hash-function-components
					 extract-kdf-hash-function-components)
				       data)))
    (is (same 
	 (tuple (HASH-FUNCTIONS *hash-functions*)
		(KDF-HASH-FUNCTION-NAME :BLAKE2S)
		(KDF-HASH-FUNCTION (TPL (HASH-FUNCTION-NAME HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-SIZE)
					(:BLAKE2S 1.6055E-7 10324 32)))
		(KDF-HASH-FUNCTION-NAME :blake2s)
		(KDF-HASH-FUNCTION-CONSTRAINTS 10324)
		(KDF-HASH-FUNCTION-SIZE 32)
		(KDF-HASH-FUNCTION-TIME 1.6055e-7)
		(MERKLE-HASH-FUNCTION-NAME :PEDERSEN)
		(MERKLE-HASH-FUNCTION-CONSTRAINTS 1152)
		(MERKLE-HASH-FUNCTION-SIZE 32)
		(MERKLE-HASH-FUNCTION-TIME 1.7993e-5)
		(MERKLE-HASH-FUNCTION (TPL (HASH-FUNCTION-NAME HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-SIZE)
					   (:PEDERSEN 0.000017993 1152 32))))
	 result))))

#|
(apply-transformation select-merkle-hash-function-inner (join *defaults* (tuple (hash-functions *hash-functions*))))

Feeling our way toward a constraint?

(() (== merkle-hash-function.name hash-function.name))

Which is 

|#


(defschema zigzag-schema
    "ZigZag"
  (comm-d-size "")
  (comm-r-size "")
  (comm-r-star-size "")
  (comm-rs-size "")
  (commitments-size "")
  (proof-size "")
  (degree "")
  (base-degree "")
  (expansion-degree "")
  (sloth-iter "")
  (partitions "")
  ;; TODO: hierarchical namespacing of some parameters?
  (replication-time "")
  (sealing-time "")
  (zigzag-vanilla-proving-time "")
  (zigzag-groth-proving-time "")
  (zigzag-total-proving-time "")
  (total-seal-time "")
  
  (replication-cycles "")
  (sealing-cycles "")
  (zigzag-vanilla-proving-cycles "")
  (zigzag-groth-proving-cycles "")
  (zigzag-total-proving-cycles "")
  (total-seal-cycles "")
  
  (zigzag-constraints "")
  (zigzag-hashing-constraints "")
  (zigzag-non-hashing-constraints "")

  (single-circuit-proof-size "")
  (total-circuit-proof-size "")

  (total-challenges "")
  (partition-challenges "")
  )

(defconstraint-system zigzag-constraint-system
    ;; TODO: Make variadic version of + and ==.
    ((comm-d-size (== merkle-hash-function-size))
     (comm-r-size (== merkle-hash-function-size))
     (comm-r-star-size (== merkle-hash-function-size))     
     (comm-rs-size (+ comm-r-size comm-r-star-size))
     (commitments-size (+ comm-rs-size comm-d-size))
     (total-circuit-proof-size (* single-circuit-proof-size partitions))
     (proof-size (+ commitments-size total-circuit-proof-size))
     (total-challenges (* partitions partition-challenges))

     (degree (+ base-degree expansion-degree))
     (total-parents (== degree))

     (parents-plus (+ total-parents 1))
     (kdf-hashes (/ parents-plus 2))
     (single-kdf-time (* kdf-hashes kdf-hash-function-time))
     
     (single-node-sloth-time (* sloth-iter single-sloth-iteration-time))
     (single-node-encoding-time (+ single-kdf-time single-node-sloth-time))
     (layer-replication-time (* single-node-encoding-time merkle-tree-leaves))
     (replication-time (* layers layer-replication-time))
     (vector-commitment-time (* merkle-tree-hash-count merkle-hash-function-time))
     (non-circuit-time (+ replication-time vector-commitment-time))
     (circuit-proving-time-per-constraint (/ bench-circuit-proving-time bench-circuit-constraints))
     (total-zigzag-circuit-hashes (* total-challenges merkle-inclusion-proof-hash-length))
     (total-zigzag-hashing-constraints (* total-zigzag-circuit-hashes merkle-hash-function-constraints))
     (total-zigzag-constraints (+ total-zigzag-hashing-constraints total-zigzag-non-hashing-constraints))
     (circuit-proving-time (* total-zigzag-constraints circuit-proving-time-per-constraint))
     (seal-time (+ non-circuit-time circuit-proving-time)))
  :schema (find-schema 'zigzag-schema))

(defun zigzag-system ()
  (make-instance 'system
		 :components (list (component ((find-transformation 'select-merkle-hash-function)))
				   (component ((find-transformation 'extract-merkle-hash-function-components)))
				   (component ((find-transformation 'select-kdf-hash-function)))
				   (component ((find-transformation 'extract-kdf-hash-function-components))))
		 :subsystems (list (find-system 'zigzag-constraint-system)
				   (find-system 'merkle-tree-constraint-system))
		 :data (list *defaults*
			     *zigzag-defaults*
			     (tuple (hash-functions *hash-functions*)))))

(test zigzag-system
  "Test ZigZag constraint system."
  (let* ((result (ask (zigzag-system) '(seal-time))))
    (is (same (tuple (seal-time  3171.5999))
	      result))))

(defun filecoin-system ()
  (make-instance 'system :subsystems (list (performance-system) (zigzag-system))))

(test filecoin-defaults
  "Test and assert results of solving with defaults."
  (let* (;(initial-relation (join *hash-functions* *defaults*))
	 (result (ask (filecoin-system) '(seal-cost seal-time)))
	 (expected
	  (tuple  (SEAL-COST 661.7256) (SEAL-TIME 3171.5999))))
    (is (same expected result))))

(test examples
  "Some examples placed in a test to ensure they don't break."
  (finishes
    (report-solution-for '(seal-cost) :system (performance-system) :initial-data *performance-defaults*)
    (solve-for (performance-system) '(aws-price-TiB-year))
    (ask (performance-system) '(aws-price-TiB-year annual-TiB))
    (ask (performance-system) '(seal-cost))
    
    ;;; Interaction example:

    (use-construction 'performance-constraint-system :data *performance-defaults*)
    (report-data)
    (report-solution-for '(seal-cost))
    (forget gib-replication-cycles)
    (report-solution-for '(seal-cost))
    (report-solution-for '(gib-replication-cycles))
    (try-with seal-cost 661.7256)
    (report-solution-for '(gib-replication-cycles))))
