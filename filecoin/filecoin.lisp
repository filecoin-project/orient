(defpackage filecoin
  (:use :common-lisp :orient :it.bese.FiveAm)
  (:nicknames :fc))

(in-package :filecoin)

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

(defschema filecoin-price-performance
    "Filecoin price performance."
  (investment "Dollar cost of infrastructure purchase required to mine at scale.")
  (comparable-monthly-income "Expected dollar income for selling storage equivalent to what can be sealed for `investment`.")
  (seal-cost "Dollar cost of investment required to seal one GiB in one hour at scale.")
  (aws-storage-price "Dollar cost of one TiB storage from AWS S3 for one month.")
  (miner-months-to-capacity "Months it should take a miner to reach full storage capacity.")
  (roi-interval-months "Months over which a miner should see return on investment.")

  (annual-TiB "Amount of storage, in TiB, which must be brought online per year."))

(defparameter *performance-defaults*
  (tuple
   (investment 100000)
   (comparable-monthly-income 50000)
   (seal-cost 10)
   (aws-storage-price 23)
   (miner-months-to-capacity 3)
   (roi-interval-months 6)
   (TiB-drive-cost 300)
   (cpu-ghz-cost 10) ;; FIXME: Get a better starter value.
   (GiB-replication-cycles (* 13000 4300))
   ))

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

(deftransformation performance-a ((aws-storage-price comparable-monthly-income miner-months-to-capacity TiB-drive-cost seal-cost
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
	 (cycles-per-hour (* hourly-GiB GiB-replication-cycles))
	 (cycles-per-minute (* cycles-per-hour 60))
	 (cycles-per-second (* cycles-per-minute 60))
	 (needed-ghz (/ cycles-per-second 1e9))
	 (up-front-compute-cost (/ needed-ghz cpu-ghz-cost))
	 (total-up-front-cost (+ up-front-compute-cost up-front-drive-cost))
	 (seal-cost (/ total-up-front-cost hourly-GiB)))
    (values aws-price-TiB-year
	    (float annual-TiB) (float monthly-TiB) (float daily-TiB) (float hourly-TiB) (float hourly-GiB)
	    (float up-front-drive-cost) (float up-front-compute-cost) (float total-up-front-cost) (float seal-cost))))

(defun performance-system ()
  (make-instance 'system
		 :components (list (component (performance-a)))
		 :schema filecoin-price-performance
		 :data (list *performance-defaults*)))

#|
(solve-for (performance-system) '(aws-price-TiB-year))

(ask (performance-system) '(aws-price-TiB-year annual-TiB))


|#


;;; TODO: Add a function to check data against schema -- which will make more sense once schema is typed.
;;; Include option to validate that provided parameters exclude those which must be computed.

(deftransformation merkle-trees
    ((sector-size) -> (merkle-tree-leaves
		       merkle-tree-height
		       merkle-tree-hash-count
		       merkle-inclusion-proof-hash-length))
  (let* ((leaves (/ sector-size 32)) ;; FIXME: check power of two or round up.
	 (height (ceiling (+ (log leaves 2)) 1))
	 (hash-count (- leaves 1))
	 (proof-hash-length (- height 1)))
    (values leaves
	    height
	    hash-count
	    proof-hash-length)))

;; With explicit hash-function inputs. NOTE: This might be the right answer for this problem.
(deftransformation= select-merkle-hash-function-x
    ((merkle-hash-function-name hash-functions &all tuple) -> (merkle-hash-function-constraints merkle-hash-function-time))
  (!> (join tuple (tpl (hash-function-name) merkle-hash-function-name) hash-functions)
      (remv (hash-function-name))
      (rename ((hash-function-time merkle-hash-function-time)
	       (hash-function-constraints merkle-hash-function-constraints)))))

(deftransformation= select-merkle-hash-function
    ((merkle-hash-function-name hash-function-name hash-function-time hash-function-constraints &all tuple) -> (merkle-hash-function-constraints merkle-hash-function-time ;&remove hash-function-constraints hash-function-time
																		 ))
  (!> (make-relation (list tuple))
      (where ((hash-function-name merkle-hash-function-name) (eql hash-function-name merkle-hash-function-name)))
       (rename ((hash-function-time merkle-hash-function-time)
		(hash-function-constraints merkle-hash-function-constraints)))
       (join (make-relation (list  tuple)))
      ))

#|
Incremental previous attempts.

(deftransformation select-merkle-hash-function
    ((merkle-hash-function-name hash-functions) -> (merkle-hash-function-constraints merkle-hash-function-time))
  (let* ((d (tuple (hash-function-name merkle-hash-function-name)))
	 (r (join d hash-functions))
	 (q (first (tuples r))) ;; This should extract a guaranteed single tuple from a relationship cardinality 1. TODO: add that operator.
	 )
    (values (tref 'hash-function-constraints q) (tref 'hash-function-time q))))

(deftransformation= select-merkle-hash-function
    ((merkle-hash-function-name hash-functions &all tuple) ->ppp (merkle-hash-function-constraints merkle-hash-function-time))
  (rename ((hash-function-time merkle-hash-function-time)
	   (hash-function-constraints merkle-hash-function-constraints))
	  (remv (hash-function-name)
		(join tuple (tuple (hash-function-name merkle-hash-function-name)) hash-functions))))

|#

(defun filecoin-system () (sys ((component (merkle-trees)) (component (select-merkle-hash-function)))))
(defparameter *system* (filecoin-system))

(def-suite filecoin-suite)
(in-suite filecoin-suite)

(test defaults-test
  "Test and assert results of solving with defaults."
  (let* ((initial-relation (join *hash-functions* *defaults*))
	 (result (solve-for (filecoin-system) '(merkle-tree-leaves
						merkle-tree-height
						merkle-tree-hash-count
						merkle-hash-function-time
						merkle-hash-function-constraints)
			    initial-relation))
	 (expected
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
	   (MERKLE-INCLUSION-PROOF-HASH-LENGTH 24))))
    (is (same expected result))))

(test performance-test
  "Test performance system, with default values -- a sanity/regression test for now."
  (let ((result (solve-for (performance-system) '(seal-cost)))
	(expected (tuple  (ANNUAL-TIB 181.15942) (AWS-PRICE-TIB-YEAR 276) (AWS-STORAGE-PRICE 23)
			  (COMPARABLE-MONTHLY-INCOME 50000) (CPU-GHZ-COST 10)
			  (DAILY-TIB 1.9853088) (GIB-REPLICATION-CYCLES 55900000)
			  (HOURLY-GIB 84.706505) (HOURLY-TIB 0.082721196) (INVESTMENT 100000)
			  (MINER-MONTHS-TO-CAPACITY 3) (MONTHLY-TIB 60.386475)
			  (ROI-INTERVAL-MONTHS 6) (SEAL-COST 661.7255) (TIB-DRIVE-COST 300)
			  (TOTAL-UP-FRONT-COST 56052.457) (UP-FRONT-COMPUTE-COST 1704.6338)
			  (UP-FRONT-DRIVE-COST 54347.824))))
    (is (same expected result) "produces all expected results")

    (is (same (tuple (seal-cost 661.7255))
	      (ask (performance-system) '(seal-cost)))
	"correctly calculates SEAL-COST")))

#|
(run! 'filecoin-suite)
|#
