(in-package :filecoin)
(in-suite filecoin-suite)


(defparameter *hash-functions* (relation (hash-function-name hash-function-time hash-function-constraints hash-function-size)
					 (:pedersen 0.000017993 1152 32)
					 (:blake2s 1.6055e-7 10324 32)))

(defparameter *zigzag-defaults* (tuple
				 (alpha-merkle-hash-function-name :pedersen)
				 (merkle-hash-function-name :pedersen)
				 (beta-hash-function-name :blake2s)
				 (kdf-hash-function-name :blake2s)
				 ;;(partition-challenges 400)
				 (partitions 1)
				 (single-circuit-proof-size 192) ;; Groth16, BLS12-381 -- eventually should allow selection of proving systems/curves.
				 ;(single-circuit-proof-size 520) ;; Groth16, SW6 -- eventually should allow selection of proving systems/curves.
				 (sloth-iter 0)
				 (layers 10)
				 (base-degree 5)
				 (expansion-degree 8)

;				 (total-challenges 8000)

				 ;; TODO: account for other constraint sources.
				 (total-zigzag-other-constraints 0)

				 ;; Need from benchmarks
				 (single-sloth-iteration-time 123) ;; BOGUS
				 (single-sloth-iteration-constraints 321) ;; BOGUS
				 (bench-circuit-proving-time (* 2.785 60))
				 (bench-circuit-constraints 16e6)

				 (beta-merkle-height 0)
				 ))

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
		(merkle-tree-height-raw 3)
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

;; -- is not assigned internally, so this constraint will not produce a 'return value'.
;; TODO: Consider a more explicit way to do this -- at least a strong convention, if not a syntax
;; allowing to explicitly forego a return value.
(define-system-constraint merkle-tree (-- (merkle-tree sector-size node-bytes))
  ((leaves (/ sector-size node-bytes)
	   :description "Number of leaves in the merkle tree.")
   (height-raw (log leaves 2)
	       :description "Height of the merkle tree. Unit: float which MUST be integer-valued")
   (height (integer height-raw)
	   :description "Height of the merkle tree, including leaves and root.")
   (inclusion-proof-hash-length (== height)
				:description "Number of hashes required for a merkle inclusion proof.")
   (hash-count (- leaves 1)
	       :description "Total number of hashes required to construct the merkle tree (leaves are not hashed).")))

(test merkle-tree-system-constraint
  (let* ((cs (constraint-system ((mt (merkle-tree sector-size node-bytes)))))
	 (expected (rel (tuple (MT.HEIGHT 5)
                               (MT.LEAVES 32)
                               (NODE-BYTES 32)
                               (SECTOR-SIZE 1024)
                               (MT.HASH-COUNT 31)
                               (MT.HEIGHT-RAW 5.0)
                               (MT.NODE-BYTES 32)
                               (MT.SECTOR-SIZE 1024)
                               (MT.INCLUSION-PROOF-HASH-LENGTH 5)))))
    (is (same expected
	      (solve-for cs '() (tuple (sector-size 1024) (node-bytes 32)))))))

(defmacro define-hash-function-selector (prefix)
  (let ((selector-name (symbolconc 'select- prefix '-hash-function))
	(extractor-name (symbolconc 'extract- prefix '-hash-function-components))
	(-hash-function-name (symbolconc prefix '-hash-function-name))
	(-hash-function (symbolconc prefix '-hash-function))
	(-hash-function-constraints (symbolconc prefix '-hash-function-constraints))
	(-hash-function-time (symbolconc prefix '-hash-function-time))
	(-hash-function-size (symbolconc prefix '-hash-function-size))
	(-hash-function-system (symbolconc prefix '-hash-function-system)))
    `(progn
       (deftransformation ,selector-name ((hash-functions ,-hash-function-name)
					  -> (,-hash-function))
	 (extract (join (tuple (hash-function-name ,-hash-function-name)) hash-functions)))
       (deftransformation ,extractor-name ((,-hash-function) -> (,-hash-function-constraints ,-hash-function-time ,-hash-function-size))
	 (values (tref 'hash-function-constraints ,-hash-function)
		 (tref 'hash-function-time ,-hash-function)
		 (tref 'hash-function-size ,-hash-function)))
       (defun ,-hash-function-system ()
	 (make-instance 'system
			:components (list
				     (component ((find-transformation ',selector-name)))
				     (component ((find-transformation ',extractor-name)))))))))

(define-hash-function-selector merkle)
(define-hash-function-selector alpha-merkle)
(define-hash-function-selector beta-merkle)
(define-hash-function-selector kdf)

(test select-merkle-hash-function
  (let* ((data (join (tuple (merkle-hash-function-name :pedersen))
		     (tuple (hash-functions *hash-functions*))))
	 (result (solve-for (merkle-hash-function-system)
			    '(merkle-hash-function-constraints merkle-hash-function-size merkle-hash-function-time merkle-hash-function)
			    data)))
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
	 (result (solve-for (kdf-hash-function-system)
			    '(kdf-hash-function-constraints kdf-hash-function-size kdf-hash-function-time kdf-hash-function)
			    data)))
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
	 (result (solve-for (make-instance 'system :subsystems (list (merkle-hash-function-system) (kdf-hash-function-system)))
			    '()
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
  (sector-GiB "Size of one sector. Unit: GiB")
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
  (sealing-time "Total CPU time to seal (replicate + generate proof of replication) one sector. Unit: seconds")
  (non-circuit-proving-time "Time to generate a non-circuit proof of replication. Unit: seconds")
  (vector-commitment-time "Time to generate the vector commitments used in a non-circuit proof of replication. Unit: seconds")
  (circuit-proving-time-per-constraint "Groth16 circuit proving time (from benchmarks) per constraint. Unit: seconds")
  (circuit-proving-time "Time to generate a circuit proof of replication using Groth16. Unit: seconds")
  (zigzag-total-proving-time "Total time to generate a proof of replication (circuit and non-circuit). Unit: seconds")
  (seal-time "Total time to seal (replication + proving) one sector. Unit: seconds")
  (sector-GiB "Number of GiB in one sector. Unit: GiB")
  (GiB-seal-time "Total time to seal (replication + proving) one GiB. Unit: seconds")
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

  (storage-to-proof-size-ratio "Ratio of sealed sector size to on-chain PoRep size.")
  (storage-to-proof-size-float "Ratio of sealed sector size to on-chain PoRep size -- expressed as a float.")

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
    ((sector-size (* sector-GiB #.GiB))
     (comm-d-size (== merkle-hash-function-size))
     (comm-r-size (== merkle-hash-function-size))
     (comm-r-star-size (== merkle-hash-function-size))     
     (comm-rs-size (+ comm-r-size comm-r-star-size))
     (commitments-size (+ comm-rs-size comm-d-size))
     (total-circuit-proof-size (* single-circuit-proof-size partitions))
     (on-chain-porep-size (+ commitments-size total-circuit-proof-size))
     (total-challenges (* partitions partition-challenges))

     (degree (+ base-degree expansion-degree))
     (total-parents (== degree))

     (merkle-tree (merkle-tree sector-size node-bytes))
     
     (single-kdf-hashes (== total-parents))
     (single-kdf-time (* single-kdf-hashes kdf-hash-function-time))
     (total-nodes-to-encode (* merkle-tree.leaves layers))
     (single-node-sloth-time (* sloth-iter single-sloth-iteration-time))
     (single-node-encoding-time (+ single-kdf-time single-node-sloth-time)) ;; Excludes parent loading time.
     
     (single-challenge-inclusion-proofs (+ total-parents 2))
     (single-challenge-kdf-hashes (* single-kdf-hashes 1))
     (single-challenge-sloth-verifications (== sloth-iter))
     
     (total-zigzag-circuit-kdf-hashes (* single-challenge-kdf-hashes total-challenges))
     
     (layer-replication-time (* single-node-encoding-time merkle-tree.leaves))
     (replication-time (* layers layer-replication-time))
     (replication-time-per-byte (/ replication-time sector-size))
     (replication-time-per-GiB (* replication-time-per-byte #.(* 1024 1024 1024)))

     (single-layer-merkle-hashing-time (* merkle-tree.hash-count merkle-hash-function-time))
     (total-merkle-trees (+ layers 1))
     (total-merkle-hashing-time (* total-merkle-trees single-layer-merkle-hashing-time))
     
     (non-circuit-proving-time (+ replication-time total-merkle-hashing-time))
     (circuit-proving-time-per-constraint (/ bench-circuit-proving-time bench-circuit-constraints))

     (total-zigzag-circuit-inclusion-proofs (* total-challenges single-challenge-inclusion-proofs))
     (total-zigzag-circuit-merkle-hashes (* total-zigzag-circuit-inclusion-proofs merkle-tree.inclusion-proof-hash-length))
     (total-zigzag-merkle-hashing-constraints (* total-zigzag-circuit-merkle-hashes merkle-hash-function-constraints))

     (total-zigzag-kdf-hashing-constraints (* total-zigzag-circuit-kdf-hashes kdf-hash-function-constraints))
     (total-zigzag-hashing-constraints (+ total-zigzag-merkle-hashing-constraints total-zigzag-kdf-hashing-constraints))

     (total-zigzag-sloth-constraints (* total-challenges single-sloth-iteration-constraints))
     
     (total-zigzag-non-hashing-constraints (+ total-zigzag-sloth-constraints total-zigzag-other-constraints))
     
     (total-zigzag-constraints (+ total-zigzag-hashing-constraints total-zigzag-non-hashing-constraints))
     (circuit-proving-time (* total-zigzag-constraints circuit-proving-time-per-constraint))
     (seal-time (+ non-circuit-proving-time circuit-proving-time))
     (sector-GiB (/ sector-size #.GiB))
     (GiB-seal-time (/ seal-time sector-GiB))
     (storage-to-proof-size-ratio (/ sector-size on-chain-porep-size))
     (storage-to-proof-size-float (* 1.0 storage-to-proof-size-ratio)))
  :schema 'zigzag-schema)

(defschema zigzag-security-schema
    "ZigZag Security"
  (zigzag-soundness "ZigZag soundness: Unit fraction")
  (zigzag-lambda "ZigZag soundness: Unit bits")
  (zigzag-epsilon "Maximum allowable deletion (space tightness): Unit: fraction")
  (zigzag-delta "Maximum allowable cheating on labels (block corruption)")
  (zigzag-basic-layer-challenges "Multiple of lambda challenges per layer, without tapering optimization.")
  (zigzag-space-gap "Maximum allowable gap between actual and claimed storage. Unit: fraction"))

(defconstraint-system zigzag-security-constraint-system
    ((zigzag-lambda (log zigzag-soundness #.(/ 1 2)))
     (zigzag-space-gap (+ zigzag-epsilon zigzag-delta))
     (zigzag-basic-layer-challenge-factor (/ 1 zigzag-delta))
     (zigzag-basic-layer-challenges (* zigzag-lambda zigzag-basic-layer-challenge-factor))
     (total-untapered-challenges (* layers zigzag-basic-layer-challenges)) ;; TODO: TOTAL-CHALLENGES and LAYERS should have zigzag-specific names.
     (total-challenges (== total-zigzag-challenges)))
  :schema 'zigzag-security-schema)

(deftransformation compute-zigzag-layers ((zigzag-delta zigzag-epsilon) -> (zigzag-layers))
  (+ (log (/ 1
	     (* 3 (- zigzag-epsilon (* 2 zigzag-delta))))
	  2)
     4))

(deftransformation compute-zigzag-tapered-layers ((zigzag-basic-layer-challenge-factor zigzag-lambda layers zigzag-taper)
						  -> (zigzag-layer-challenges total-zigzag-challenges))
  (let* ((reduction (- 1 zigzag-taper))
	 (layer-challenges (loop for i from 0 below layers
			      collect (* zigzag-lambda (max 20 (floor (* zigzag-basic-layer-challenge-factor (expt reduction i))))))))
    (values (apply #'vector layer-challenges) (reduce #'+ layer-challenges))))

(defparameter *default-zigzag-security*
  (tuple
   (zigzag-lambda 8)
   (zigzag-taper (/ 1 3))
   (zigzag-epsilon 0.007)
   (zigzag-delta 0.003)))

(defun zigzag-security-system (&key isolated)
  (make-instance 'system
		 :components (list (component ('compute-zigzag-layers))
				   (component ('compute-zigzag-tapered-layers)))
		 :subsystems (list (find-system 'zigzag-security-constraint-system))
		 :data (if isolated
			   (list (tuple (layers 10)) *default-zigzag-security*)
			   (list *default-zigzag-security*))))

(defun zigzag-system ()
  (make-instance 'system
		 :components (list (component ('select-merkle-hash-function))
				   (component ('select-alpha-merkle-hash-function))
				   (component ('select-beta-merkle-hash-function))
				   (component ('extract-merkle-hash-function-components))
				   (component ('select-kdf-hash-function))
				   (component ('extract-kdf-hash-function-components)))
		 :subsystems (list (find-system 'zigzag-constraint-system)
				   (zigzag-security-system))
		 :data (list *defaults*
			     *zigzag-defaults*
			     (tuple (hash-functions *hash-functions*)))))
