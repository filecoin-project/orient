(defpackage filecoin
  (:use :common-lisp :orient :it.bese.FiveAm)
  (:nicknames :fc))

(in-package :filecoin)

(def-suite filecoin-suite)
(in-suite filecoin-suite)

(defconstant KiB 1024)
(defconstant MiB (* KiB 1024))
(defconstant GiB (* MiB 1024))

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

  (sector-size "Size in bytes of a sealed sector.")

  ;;;; Optional parameters.
  (merkle-hash-function-name "Hash function name. Type is an enumeration of { PEDERSEN, BLAKE2S } (may change when types are implemented).")

  (hash-functions "Relation containing hash function names, times (should be cycles), and constraints.")

  ;;;; Parameters below should be excluded from defaults.

  #| TODO: add descriptions.

  merkle-tree-leaves
  merkle-tree-height
  merkle-tree-hash-count
  merkle-inclusion-proof-hash-length

  |#
  )

(defparameter *hash-functions* (relation (hash-function-name hash-function-time hash-function-constraints)
					 (:pedersen 0.000017993 1152)
					 (:blake2s 1.6055e-7 10324)))

(defparameter *defaults*
  (tuple
   ;; FIXME: This depends on the processor speed of the machine that produced the benchmark. We should express benchmarks in cycles.
   (pedersen-hash-seconds 0.000017993)
   (pedersen-hash-constraints 1152)

   (blake-hash-seconds 1.6055e-7)
   (blake-hash-constraints 10324)

   ;; (hash-functions (relation (hash-function-name hash-function-time hash-function-constraints)
   ;; 			     (:pedersen 0.000017993 1152)
   ;; 			     (:blake2s 1.6055e-7 10324)))

   (merkle-hash-function-name :pedersen)

   (sector-size (* 1 GiB))))

(defun filecoin-system () (sys ((component (merkle-trees)) (component (select-merkle-hash-function)))))

(test defaults
  "Test and assert results of solving with defaults."
  (let* ((initial-relation (join *hash-functions* *defaults*))
	 (result (solve-for (filecoin-system) '(merkle-tree-leaves
						merkle-tree-height
						merkle-tree-hash-count
						merkle-hash-function-time
						merkle-hash-function-constraints)
			    initial-relation))
	 (expected
	  (make-relation
	   (list
	    (tuple
	     (PEDERSEN-HASH-SECONDS 1.7993e-5)
	     (PEDERSEN-HASH-CONSTRAINTS 1152)
	     (BLAKE-HASH-SECONDS 1.6055e-7)
	     (BLAKE-HASH-CONSTRAINTS 10324)
	     (SECTOR-SIZE 1073741824)
	     (HASH-FUNCTION-NAME :PEDERSEN)
	     (MERKLE-HASH-FUNCTION-NAME :PEDERSEN)
	     (MERKLE-HASH-FUNCTION-CONSTRAINTS 1152)
	     (MERKLE-HASH-FUNCTION-TIME 0.000017993)
	     (HASH-FUNCTION-CONSTRAINTS 1152)
	     (HASH-FUNCTION-TIME 0.000017993)
	     (MERKLE-TREE-LEAVES 33554432)
	     (MERKLE-TREE-HEIGHT 25)
	     (MERKLE-TREE-HASH-COUNT 33554431)
	     (MERKLE-INCLUSION-PROOF-HASH-LENGTH 24))))))
    (is (same expected result))))

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

(deftransformation performance ((aws-storage-price comparable-monthly-income miner-months-to-capacity TiB-drive-cost
						   cpu-ghz-cost GiB-replication-cycles)
				  ->
				(aws-price-TiB-year annual-TiB monthly-TiB daily-TiB hourly-TiB hourly-GiB up-front-drive-cost
						    up-front-compute-cost total-up-front-cost seal-cost))
  (let* ((aws-price-TiB-year (* aws-storage-price 12))
	 (annual-TiB (/ comparable-monthly-income aws-price-TiB-year))
	 (monthly-TiB (/ annual-TiB miner-months-to-capacity)) ;; Rate at which we must seal.
	 (daily-TiB (/ monthly-TiB (/ 365 12)))
	 (hourly-TiB (/ daily-Tib 24))
	 (hourly-GiB (* hourly-TiB 1024))
	 (up-front-drive-cost (* TiB-drive-cost annual-TiB))

	 (replication-cycles-per-hour (* hourly-GiB GiB-replication-cycles))
	 (replication-cycles-per-minute (* replication-cycles-per-hour 60))
	 (replication-cycles-per-second (* replication-cycles-per-minute 60))
	 (needed-ghz (/ replication-cycles-per-second 1e9))
	 (up-front-compute-cost (/ needed-ghz cpu-ghz-cost))
	 (total-up-front-cost (+ up-front-compute-cost up-front-drive-cost))
	 (seal-cost (/ total-up-front-cost hourly-GiB)))
    (values aws-price-TiB-year
	    (float annual-TiB) (float monthly-TiB) (float daily-TiB) (float hourly-TiB) (float hourly-GiB)
	    (float up-front-drive-cost) (float up-front-compute-cost) (float total-up-front-cost) (float seal-cost))))

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
  :schema filecoin-price-performance)

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

(defun performance-system ()
  (make-instance 'system
		 :components (list (component (performance)))
		 :schema filecoin-price-performance
		 :data (list *performance-defaults*)))

(test performance-test
  "Test performance system, with default values -- a sanity/regression test for now."
  (let ((result (solve-for (performance-system) '(seal-cost)))
	(expected (tuple (ANNUAL-TIB 181.15942) (AWS-PRICE-TIB-YEAR 276.0)
			 (AWS-STORAGE-PRICE 23.0) (COMPARABLE-MONTHLY-INCOME 50000.0)
			 (CPU-GHZ-COST 10.0) (DAILY-TIB 1.9853088)
			 (GIB-REPLICATION-CYCLES 5.59e7) (HOURLY-GIB 84.706505)
			 (HOURLY-TIB 0.082721196) (INVESTMENT 100000.0)
			 (MINER-MONTHS-TO-CAPACITY 3) (MONTHLY-TIB 60.386475)
			 (ROI-INTERVAL-MONTHS 6) (SEAL-COST 661.7256) (TIB-DRIVE-COST 300.0)
			 (TOTAL-UP-FRONT-COST 56052.46) (UP-FRONT-COMPUTE-COST 1704.6338)
			 (UP-FRONT-DRIVE-COST 54347.83))))
    (is (same expected result) "produces all expected results")

    (is (same (tuple (seal-cost 661.7256))
    	      (ask (performance-system) '(seal-cost)))
    	"correctly calculates SEAL-COST")))

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
     (merkle-tree-height (== merkle-inclusion-proof-hash-length))))

(test merkle-tree-constraint-system
  "Test merkle tree constraint system."

  ;; Compute MERKLE-TREE-HEIGHT from SECTOR-SIZE.
  (is (same
       (tuple (sector-size 32)
	      (node-bytes 4)
	      (merkle-tree-leaves 8)
	      (merkle-inclusion-proof-hash-length 3)
	      (merkle-inclusion-proof-hash-length-raw 3.0)
	      (merkle-tree-height 3))
       (solve-for merkle-tree-constraint-system '(merkle-tree-height) (tuple (sector-size 32) (node-bytes 4)))))

  ;; Compute SECTOR-SIZE from MERKLE-TREE-HEIGHT.
  (is (same
       (tuple (sector-size 32)
	      (node-bytes 4)
	      (merkle-tree-leaves 8)
	      (merkle-inclusion-proof-hash-length 3)
	      (merkle-inclusion-proof-hash-length-raw 3.0)
	      (merkle-tree-height 3))
       (solve-for merkle-tree-constraint-system '(sector-size) (tuple (merkle-tree-height 3) (node-bytes 4)))))

  ;; Compute SECTOR-SIZE from MERKLE-INCLUSION-PROOF-HASH-LENGTH.
  (is (same
       (tuple (sector-size 32)
	      (node-bytes 4)
	      (merkle-tree-leaves 8)
	      (merkle-inclusion-proof-hash-length 3)
	      (merkle-inclusion-proof-hash-length-raw 3.0)
	      (merkle-tree-height 3))
       (solve-for merkle-tree-constraint-system '(sector-size) (tuple (merkle-inclusion-proof-hash-length 3) (node-bytes 4))))))

(deftransformation select-merkle-hash-function
    ((merkle-hash-function-name hash-function-name hash-function-time hash-function-constraints &all tuple)
     => (merkle-hash-function-constraints merkle-hash-function-time))
  (when (eql hash-function-name merkle-hash-function-name)
    `((,hash-function-constraints ,hash-function-time))))

(defschema zigzag-schema "ZigZag")

(defconstraint-system zigzag-constraint-system
    ()
    :schema zigzag-schema)

(test examples
  "Some examples placed in a test to ensure they don't break."
  (finishes
    (report-solution-for '(seal-cost) :system (performance-system) :initial-data *performance-defaults*)
    (solve-for (performance-system) '(aws-price-TiB-year))
    (ask (performance-system) '(aws-price-TiB-year annual-TiB))
    (ask (performance-system) '(seal-cost))
    
    ;;; Interaction example:

    (use-construction performance-constraint-system :data *performance-defaults*)
    (report-data)
    (report-solution-for '(seal-cost))
    (forget gib-replication-cycles)
    (report-solution-for '(seal-cost))
    (report-solution-for '(gib-replication-cycles))
    (try-with seal-cost 661.7256)
    (report-solution-for '(gib-replication-cycles))))
