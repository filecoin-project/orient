(defpackage filecoin
  (:use :common-lisp :orient :it.bese.FiveAm)
  (:nicknames :fc)
  (:export
   :GiB-seal-cycles
   :roi-months
   :seal-cost :seal-time :sector-size :up-front-compute-cost :total-up-front-cost :monthly-income :annual-income :layers :total-challenges
   :filecoin-system :performance-system :zigzag-system
   :*performance-defaults*))

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
   (annual-income 50000.0)
   (aws-glacier-price 0.004)
   (commodity-storage-discount 0.9)
   (miner-months-to-capacity 6)
   (TiB-drive-cost 30.0)
   (cpu-ghz-cost 10.0)
   (up-front-memory-cost 0.0) ;; FIXME: Incorporate.
   )
  )

(defparameter *isolated-performance-defaults* (tuple (GiB-seal-cycles 2.3510403e8)))
(defparameter *integrated-performance-defaults* (tuple (seal-GHz 4300)))

(defparameter *zigzag-defaults* (tuple
				 (alpha-merkle-hash-function-name :pedersen)
				 (merkle-hash-function-name :pedersen)
				 (beta-hash-function-name :blake2s)
				 (kdf-hash-function-name :blake2s)
				 (partition-challenges 400)
				 (single-circuit-proof-size 192) ;; Groth16 -- eventually should allow selection of proving systems.
				 (sloth-iter 0)
				 (layers 10)
				 (base-degree 5)
				 (expansion-degree 8)

				 (total-challenges 8000)

				 ;; TODO: account for other constraint sources.
				 (total-zigzag-other-constraints 0) 

				 ;; Need from benchmarks
				 (single-sloth-iteration-time 123) ;; BOGUS
				 (single-sloth-iteration-constraints 321) ;; BOGUS
				 (bench-circuit-proving-time (* 2.785 60))
				 (bench-circuit-constraints 16e6)

				 (beta-merkle-height 0)
				 ))

(defparameter *defaults*
  (tuple
   (node-bytes 32)
   (sector-size (* 1 GiB))))

(defschema filecoin-price-performance
    "Filecoin price performance."  
  (aws-glacier-price "Cost of one GiB storage from AWS glacier for one month. Unit: dollars")
  (annual-income "Annual income from selling storage on the storage market. Unit: dollars")
  (monthly-income "Monthly income from selling storage on the storage market. Unit: dollars")
  (comparable-monthly-cost "Expected cost of purchasing monthly storage from commodity provider. Unit: dollars")
  (seal-cost "Cost of investment required to seal one GiB in one hour at scale. Unit: dollars")
  (commodity-storage-discount "Fraction of commodity storage pricing expected as income from storage market. Unit: decimal fraction")
  (miner-months-to-capacity "Months it should take a miner to reach full storage capacity. Unit: months")

  (GiB-capacity "GiB of storage at full capacity. Unit: GiB")
  (TiB-capacity "TiB of storage at full capacity. Unit: TiB")
  
  (annual-TiB "TiB of storage which must be brought online per year. Unit: TiB")
  (monthly-TiB "TiB of storage which must be brought online per month. Unit: TiB")
  (daily-TiB "TiB of storage which must be brought online per day. Unit: TiB")
  (hourly-TiB "TiB of storage which must be brought online per hour. Unit: TiB")
  (hourly-GiB "GiB of storage which must be brought online per hour. Unit: GiB")

  (seal-cycles-per-hour "CPU required to seal at required rate for one hour. Unit: cycles")
  (seal-cycles-per-minute "CPU required to seal at required rate for one minute. Unit: cycles")
  (seal-cycles-per-second "CPU required to seal at required rate for one second. Unit: cycles")
  (GiB-seal-cycles "Total CPU cycles required to seal 1 GiB.")
  (needed-ghz "Total GhZ capacity needed to seal at the required rate.")

  (up-front-drive-cost "Up-front investment in hard drives required to store sufficient sealed data. Unit: dollars.")
  (up-front-memory-cost "Up-front investment in memory (RAM) required to seal at necessary rate. Unit: dollars")
  (up-front-compute-cost "Up-front investement in compute hardware required to seal at necessary rate. Unit: dollars")
  (up-front-sealing-cost "Up-front investment in total hardware require to seal at necessary rate. Unit: dollars")
  (total-up-front-cost "Total up-front investment required to generate MONTHLY-INCOME. Unit: dollars")
  
  (average-monthly-income-during-ramp-up "Average monthly income before miner reaches capacity (assuming linear growth). Unit: dollars")
  (income-during-ramp-up "Total income during ramp-up period (before reaching capacity). Unit: dollars")
  (income-to-roi-at-capacity "Income still required to reach return on investment after reaching capacity. Unit: dollars")
  (roi-months-at-capacity "Months needed after reaching capacity before return on investment.")
  (roi-months "Months over which a miner should see return on investment.")
  )

(defconstraint-system performance-constraint-system    
    ((GiB-seal-cycles (* seal-time seal-GHz))
     (monthly-income (/ annual-income 12))
     (monthly-income (* comparable-monthly-cost commodity-storage-discount)) 
     (GiB-capacity (/ comparable-monthly-cost aws-glacier-price))
     (TiB-capacity (/ GiB-capacity 1024))
     (monthly-TiB (/ TiB-capacity miner-months-to-capacity)) 
     (daily-TiB (/ monthly-TiB (/ 365 12)))
     (hourly-TiB (/ daily-Tib 24))
     (hourly-GiB (* hourly-TiB 1024)) 
     (up-front-drive-cost (* TiB-drive-cost TiB-capacity))
     (seal-cycles-per-hour (* hourly-GiB GiB-seal-cycles))
     (seal-cycles-per-minute (* seal-cycles-per-hour 60))
     (seal-cycles-per-second (* seal-cycles-per-minute 60))
     (needed-ghz (/ seal-cycles-per-second 1e9))
     (up-front-sealing-cost (+ up-front-compute-cost up-front-memory-cost))
     (total-up-front-cost (+ up-front-sealing-cost up-front-drive-cost))
     (up-front-compute-cost (/ needed-ghz cpu-ghz-cost))
     (seal-cost (/ total-up-front-cost hourly-GiB))
     (average-monthly-income-during-ramp-up (/ monthly-income 2))
     (income-during-ramp-up (* average-monthly-income-during-ramp-up miner-months-to-capacity))
     (income-to-roi-at-capacity (- total-up-front-cost income-during-ramp-up))
     (roi-months-at-capacity (/ income-to-roi-at-capacity monthly-income))
     (roi-months (+ roi-months-at-capacity miner-months-to-capacity)))
  :schema 'filecoin-price-performance)

(defun performance-system (&key isolated)
  (make-instance 'system
		 :subsystems (list (find-system 'performance-constraint-system))
		 :data (list *performance-defaults* (if isolated
							*isolated-performance-defaults*
							*integrated-performance-defaults*))))

(test performance-test
  "Test performance system, with default values -- a sanity/regression test for now."
  (is (same (tuple (seal-cost 212.95776))
	    (ask (performance-system :isolated t) '(seal-cost)))))

;;; TODO: Add a function to check data against schema -- which will make more sense once schema is typed.
;;; Include option to validate that provided parameters exclude those which must be computed.

(defschema merkle-tree-schema
    "PoRep  Merkle Trees"
  (node-bytes "The number of bytes in a node -- must also be the hash digest size.") ; TODO: Move to more general schema.
  (merkle-tree-leaves "Number of leaves in the merkle tree.")
  (merkle-tree-height-raw "Height of the merkle tree. Unit: float which MUST be integer-valued")
  (merkle-tree-height "Height of the merkle tree, including leaves and root.")
  (merkle-tree-hash-count "Total number of hashes required to construct the merkle tree (leaves are not hashed).")
  (merkle-inclusion-proof-hash-length "Number of hashes required for a merkle inclusion proof.")
  (merkle-inclusion-proof-hash-length-raw "Number of hashes required for a merkle inclusion proof. Unit: float which MUST be integer-valued."))

(defconstraint-system merkle-tree-constraint-system
    ((merkle-tree-leaves (/ sector-size node-bytes))
     (merkle-tree-height-raw (log merkle-tree-leaves 2))
     (merkle-tree-height (integer merkle-tree-height-raw))
     (merkle-inclusion-proof-hash-length (== merkle-tree-height))
     (merkle-tree-hash-count (- merkle-tree-leaves 1)))
  :schema 'merkle-tree-schema)

(test merkle-tree-constraint-system
  "Test merkle tree constraint system."

  ;; Compute MERKLE-TREE-HEIGHT from SECTOR-SIZE.
  (is (same
       (make-relation
	(list
	 (tuple (sector-size 32)
		(node-bytes 4)
		(merkle-tree-leaves 8)
		(merkle-inclusion-proof-hash-length 3)
		(merkle-tree-height-raw 3.0)
		(merkle-tree-hash-count 7)
		(merkle-tree-height 3))))
       (solve-for 'merkle-tree-constraint-system '(merkle-tree-height) (tuple (sector-size 32) (node-bytes 4)))))

  ;; Compute SECTOR-SIZE from MERKLE-TREE-HEIGHT.
  (is (same
       (make-relation
	(list 
	 (tuple (sector-size 32)
		(node-bytes 4)
		(merkle-tree-leaves 8)
		(merkle-inclusion-proof-hash-length 3)
		(merkle-tree-height-raw 3.0)
		(merkle-tree-hash-count 7)
		(merkle-tree-height 3))))
       (solve-for 'merkle-tree-constraint-system '(sector-size) (tuple (merkle-tree-height 3) (node-bytes 4)))))

  ;; Compute SECTOR-SIZE from MERKLE-INCLUSION-PROOF-HASH-LENGTH.
  (is (same
       (make-relation
	(list 
	 (tuple (sector-size 32)
		(node-bytes 4)
		(merkle-tree-leaves 8)
		(merkle-inclusion-proof-hash-length 3)
		(merkle-tree-height-raw 3)
		(merkle-tree-hash-count 7)
		(merkle-tree-height 3))))
       (solve-for 'merkle-tree-constraint-system '(sector-size) (tuple (merkle-inclusion-proof-hash-length 3) (node-bytes 4))))))

(deftransformation select-merkle-hash-function
    ((merkle-hash-function-name hash-function-name hash-function-time hash-function-size hash-function-constraints)
     => (merkle-hash-function-constraints merkle-hash-function-time merkle-hash-function-size))
  (when (eql hash-function-name merkle-hash-function-name)
    `((,hash-function-constraints ,hash-function-time ,hash-function-size))))

(deftransformation select-beta-merkle-hash-function
    ((beta-merkle-hash-function-name hash-function-name hash-function-time hash-function-size hash-function-constraints)
     => (beta-merkle-hash-function-constraints beta-merkle-hash-function-time beta-merkle-hash-function-size))
  (when (eql hash-function-name beta-merkle-hash-function-name)
    `((,hash-function-constraints ,hash-function-time ,hash-function-size))))

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
  (sector-size "Size of one sector. Unit: bytes")
  (comm-d-size "Size of the data commitment (CommD). Unit: bytes")
  (comm-r-size "Size of the replica commitment (CommR). Unit: bytes")
  (comm-r-star-size "Size of the aggregated commitment to each layer's replica (CommR*). Unit: bytes")
  (comm-rs-size "Size of all replica commitments. Unit: bytes")
  (commitments-size "Size of all commitments returned by Seal. Unit: bytes")
  (on-chain-porep-size "On-chain size of one Seal proof plus commitments. Unit: bytes")
  (degree "Total in-degree of the ZigZag graph.")
  (base-degree "In-degree of the base depth-robust graph (DRG).")
  (expansion-degree "Maximum in-degree of the bipartite expander graph component of a ZigZag graph.")
  (sloth-iter "Number of iterations of sloth verifiable delay encoding (VDE) to perform.")
  (partitions "Number of circuit partitions into which a proof is divided.")
  ;; TODO: hierarchical namespacing of some parameters?
  (replication-time "Time to replicate one sector. Unit: seconds")
  (replication-time-per-byte "Time to replicate one byte. Unit: seconds / byte")
  (replication-time-per-GiB "Time to replicate one GiB. Unit: seconds / GiB")
  (sealing-time "Time to seal (replicate + generate proof of replication) one sector. Unit: seconds")
  (non-circuit-proving-time "Time to generate a non-circuit proof of replication. Unit: seconds")
  (vector-commitment-time "Time to generate the vector commitments used in a non-circuit proof of replication. Unit: seconds")
  (circuit-proving-time-per-constraint "Groth16 circuit proving time (from benchmarks) per constraint. Unit: seconds")
  (circuit-proving-time "Time to generate a circuit proof of replication using Groth16. Unit: seconds")
  (zigzag-total-proving-time "Total time to generate a proof of replication (circuit and non-circuit). Unit: seconds")
  (seal-time "Total time to seal (replication + proving) one sector. Unit: seconds")
  (total-parents "Number of parents (or padding) each node uses when performing key derivation.")
  (single-kdf-hashes "Number of hashes performed as part of a single application of the key-derivation function (KDF).")
  (kdf-hashes "Number of hashes performed as part of the key-derivation function (KDF).") 
  (single-kdf-time "Hashing time to perform a single KDF. Unit: seconds")
  (single-layer-merkle-hashing-time "Merkle hashing time for a single layer. Unit: seconds")
  (total-merkle-trees "Total merkle trees which must be generated.")
  (total-merkle-hashing-time "Total time to generate all merkle trees. Unit: seconds")

  (total-nodes-to-encode "Total nodes to encode across all layers.")
  (single-node-sloth-time "Time to perform sloth (VDE) for a single node. Unit: seconds")
  (single-node-encoding-time "Time to encode a single node. Unit: seconds")
  
  (single-challenge-inclusion-proofs "Number of inclusion proofs which must be verified for a single challenge.")
  (single-challenge-merkle-hases "Number of merkle hashes which must be verified for a single challenge.")
  (single-challenge-kdf-hashes "Number of KDF hashes which must be verified for a single challenge.")
  (single-challenge-sloth-verifications "Number of sloth iterations which must be verified for a single challenge.")
  (total-kdf-hashes "Total number of KDF (key-derivation function) required during replication.")
  (total-zigzag-merkle-hashing-constraints "Total number of merkle hashing constraints in a ZigZag circuit.")
  (total-zigzag-kdf-hashing-constraints "Total number of kdf hashing constraints in a ZigZag circuit.")
  (total-zigzag-hashing-constraints "Total number of hashing constraints in a ZigZag circuit.")
  (total-zigzag-non-hashing-constraints "Total number of hashes which must be verified in a ZigZag circuit.")
  (total-zigzag-circuit-merkle-hashes "Total number of merkle hashes which must be verified in a ZigZag circuit.")
  (total-zigzag-circuit-kdf-hashes "Total number of KDF hashes which must be verified in a ZigZag circuit.")

  (total-zigzag-sloth-constraints "Total number of constraints due to sloth verification.")
  (total-zigzag-constraints "Total number of constraints which must be verified in a ZigZag circuit.")
  (layer-replication-time "Time to replicate one layer. Unit: seconds")

  (replication-cycles "")
  (sealing-cycles "")
  (zigzag-vanilla-proving-cycles "")
  (zigzag-groth-proving-cycles "")
  (zigzag-total-proving-cycles "")
  (total-seal-cycles "")
  
  (zigzag-constraints "")
  (zigzag-hashing-constraints "")
  (zigzag-non-hashing-constraints "")

  (single-circuit-proof-size "Size of a single Groth16 Proof. Unit: bytes")
  (total-circuit-proof-size "Total size of a single circuit proof. Unit: bytes")

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
     (on-chain-porep-size (+ commitments-size total-circuit-proof-size))
     (total-challenges (* partitions partition-challenges))

     (degree (+ base-degree expansion-degree))
     (total-parents (== degree))

     (single-kdf-hashes (== total-parents))
     (single-kdf-time (* single-kdf-hashes kdf-hash-function-time))
     (total-nodes-to-encode (* merkle-tree-leaves layers))
     (single-node-sloth-time (* sloth-iter single-sloth-iteration-time))
     (single-node-encoding-time (+ single-kdf-time single-node-sloth-time)) ;; Excludes parent loading time.
     
     (single-challenge-inclusion-proofs (+ total-parents 2))
     (single-challenge-kdf-hashes (* single-kdf-hashes 1))
     (single-challenge-sloth-verifications (== sloth-iter))
     
     (total-zigzag-circuit-kdf-hashes (* single-challenge-kdf-hashes total-challenges))
     
     (layer-replication-time (* single-node-encoding-time merkle-tree-leaves))
     (replication-time (* layers layer-replication-time))
     (replication-time-per-byte (/ replication-time sector-size))
     (replication-time-per-GiB (* replication-time-per-byte (* 1024 1024 1024)))

     (single-layer-merkle-hashing-time (* merkle-tree-hash-count merkle-hash-function-time))
     (total-merkle-trees (+ layers 1))
     (total-merkle-hashing-time (* total-merkle-trees single-layer-merkle-hashing-time))
     
     (non-circuit-proving-time (+ replication-time total-merkle-hashing-time))
     (circuit-proving-time-per-constraint (/ bench-circuit-proving-time bench-circuit-constraints))

     (total-zigzag-circuit-inclusion-proofs (* total-challenges single-challenge-inclusion-proofs))
     (total-zigzag-circuit-merkle-hashes (* total-zigzag-circuit-inclusion-proofs merkle-inclusion-proof-hash-length))
     (total-zigzag-merkle-hashing-constraints (* total-zigzag-circuit-merkle-hashes merkle-hash-function-constraints))

     (total-zigzag-kdf-hashing-constraints (* total-zigzag-circuit-kdf-hashes kdf-hash-function-constraints))
     (total-zigzag-hashing-constraints (+ total-zigzag-merkle-hashing-constraints total-zigzag-kdf-hashing-constraints))

     (total-zigzag-sloth-constraints (* total-challenges single-sloth-iteration-constraints))
     
     (total-zigzag-non-hashing-constraints (+ total-zigzag-sloth-constraints total-zigzag-other-constraints))
     
     (total-zigzag-constraints (+ total-zigzag-hashing-constraints total-zigzag-non-hashing-constraints))
     (circuit-proving-time (* total-zigzag-constraints circuit-proving-time-per-constraint))
     (seal-time (+ non-circuit-proving-time circuit-proving-time)))
  :schema 'zigzag-schema)

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
    (is (same (relation (seal-time) (54675.355))
	      result))))

(defun filecoin-system ()
  (make-instance 'system :subsystems (list (performance-system) (zigzag-system))))

(test filecoin-defaults
  "Test and assert results of solving with defaults."
  (let* ((result (ask (filecoin-system) '(seal-cost seal-time)))
	 (expected
	  (relation (SEAL-COST SEAL-TIME) (212.95776 54675.355))))
    (is (same expected result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
