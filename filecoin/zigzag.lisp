(in-package :filecoin)
(in-suite filecoin-suite)

;; TODO:
;; + Add calc for wall-clock seal time.
;;  + Assume 14 cores.

;; Move these into system/schema.
(defparameter *spedersen* 1)
(defparameter *gpu-speedup* 1)
(defparameter *blake2s-speedup* 1)

(defun hash-functions (&key
			 (spedersen *spedersen*)
			 (gpu-speedup *gpu-speedup*)
			 (blake2s-speedup *blake2s-speedup*))
  ;; TODO: use blake2s-speedup.
  (relation (hash-function-name hash-function-time hash-function-constraints hash-function-size circuit-time)
	    (:pedersen (/ 2.6156e-5 spedersen)
		       1324
		       32
		       (/ (* 2 0.076994) gpu-speedup))
	    (:blake2s (/ 9.1216e-8 blake2s-speedup)
		      10324
		      32
		      (/ (* 2 0.65253) gpu-speedup))
	    (:blake2s-kdf (/ 9.1216e-8 2 blake2s-speedup)
			  (/ 10324 2)
			  32
			  (/ 0.65253 gpu-speedup))))

(defparameter *zigzag-defaults* (tuple
				 (alpha-merkle-hash-function-name :pedersen)
				 (merkle-hash-function-name :pedersen)
				 (alpha-hash-function-name :pedersen)
				 (beta-hash-function-name :blake2s)
				 (kdf-hash-function-name :blake2s-kdf)
				 ;;(partition-challenges 400)
				 (partitions 1)
				 (single-circuit-proof-size 192) ;; Groth16, BLS12-381 -- eventually should allow selection of proving systems/curves.
				 ;(single-circuit-proof-size 520) ;; Groth16, SW6 -- eventually should allow selection of proving systems/curves.
				 (sloth-iter 0)
				 (layers 10)
				 (base-degree 5)
				 (expansion-degree 8)

				 ;; TODO: account for other constraint sources.
				 (total-zigzag-other-constraints 0)

				 ;; Need from benchmarks
				 (single-sloth-iteration-time 123) ;; BOGUS
				 (single-sloth-iteration-constraints 321) ;; BOGUS

				 ;; On my proving machine (for comparable timing to hash benchmarks)
				 ;; ./target/release/examples/zigzag --size 1024 --groth
				 ;;(bench-circuit-proving-time (* 4 30.0))
				 ;;(bench-circuit-constraints 4652824)
				 (bench-circuit-proving-time 0.14464)
				 (bench-circuit-constraints 10324)

				 ;;(max-beta-merkle-height 0)
				 (max-beta-merkle-height 30)

				 ;(wall-clock-seal-time (* 60 60))
				 (seal-parallelism 14)
				 ))

(defun zigzag-bench-data () (tuple (hash-functions (hash-functions))))

(defparameter *zigzag-hypotheticals* nil)

;; -- is not assigned internally, so this constraint will not produce a 'return value'.
;; TODO: Consider a more explicit way to do this -- at least a strong convention, if not a syntax
;; allowing to explicitly forego a return value.
(define-system-constraint merkle-tree
    (-- (merkle-tree sector-size node-bytes))
  ;; Calculate LEAVES outside and take that as input.
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

(define-system-constraint hybrid-merkle-tree
    (-- (hybrid-merkle-tree sector-size node-bytes beta-height))
  ((leaves (/ sector-size node-bytes)
	   :description "Number of leaves in the merkle tree.")
   (height-raw (log leaves 2)
	       :description "Height of the merkle tree. Unit: float which MUST be integer-valued")
   (height (integer height-raw)
	   :description "Height of the merkle tree, including leaves and root.")
   (alpha-height (- height beta-height))
   (total-inclusion-proof-hash-length (== height)
				:description "Number of hashes required for a merkle inclusion proof.")
   (total-hash-count (- leaves 1)
		     :description "Total number of hashes required to construct the merkle tree (leaves are not hashed).")
   (alpha-leaves (expt 2 alpha-height))

   
   (alpha-hash-count (- alpha-leaves 1))
   (alpha-inclusion-proof-hash-length (== alpha-height))

   (beta-hash-count (- total-hash-count alpha-hash-count))
   (beta-inclusion-proof-hash-length (== beta-height))))

(test hybrid-merkle-tree-system-constraint
  (let* ((cs (constraint-system ((mt (hybrid-merkle-tree sector-size node-bytes 0)))))
	 (expected (rel (tuple (MT.HEIGHT 5)
                               (MT.LEAVES 32)
                               (NODE-BYTES 32)
                               (SECTOR-SIZE 1024)			       
			       (MT.ALPHA-HEIGHT 5)
			       (MT.BETA-HEIGHT 0)
			       (MT.ALPHA-LEAVES 32)
                               (MT.TOTAL-HASH-COUNT 31)
                               (MT.ALPHA-HASH-COUNT 31)
			       (MT.BETA-HASH-COUNT 0)
                               (MT.HEIGHT-RAW 5.0)
                               (MT.NODE-BYTES 32)
                               (MT.SECTOR-SIZE 1024)
                               (MT.TOTAL-INCLUSION-PROOF-HASH-LENGTH 5)
                               (MT.ALPHA-INCLUSION-PROOF-HASH-LENGTH 5)
                               (MT.BETA-INCLUSION-PROOF-HASH-LENGTH 0)
			       ))))
    (is (same expected
	      (solve-for cs '() (tuple (sector-size 1024) (node-bytes 32)))))))

;; TODO: It would be nice to be able to define a simple constraint within the system constraint which uses it
;; if used only there.
(define-simple-constraint select-hash-function-tuple (hash-function (name hash-functions))
    (extract (join (tuple (hash-function-name name)) hash-functions)))

(test select-hash-function-tuple
  (let* ((data (tuple (hf-name :pedersen) (hash-functions (hash-functions))))
	 (expected (with
		    (with data 'constraints 1324)
		    'hf (extract (join (tuple (hash-function-name :pedersen)) (hash-functions)))))
	 (system (constraint-system
		  ((hf (select-hash-function-tuple hf-name hash-functions))
		   (constraints (tref 'hash-function-constraints hf))))))
    (is (same expected
	      (solve-for system '() data)))))

;; TODO: With the right abstraction, this would be much simpler. Think about it.
(define-system-constraint select-hash-function
    (-- (select-hash-function name% hash-functions%))
  ((hash-function% (select-hash-function-tuple name% hash-functions%)
		  :description "Tuple containing the selected hash function's characteristics.")
   (constraints (tref 'hash-function-constraints hash-function%)
		:description "Number of constraints required to prove *.HASH-FUNCTION in circuit.")
   (circuit-time (tref 'circuit-time hash-function%)
		 :description "Time for one *.HASH-FUNCTION in circuit.")
   (size (tref 'hash-function-size hash-function%)
	 :description "Size of digest (output) generated by *.HASH-FUNCTION.")
   (time (tref 'hash-function-time hash-function%))))

(test select-hash-function
  (let* ((data (tuple (hf-name :pedersen) (hash-functions (hash-functions))))
	 (pedersen-hash-function (extract (join (tuple (hash-function-name :pedersen)) (hash-functions))))
	 (expected (TUPLE (HF-NAME :PEDERSEN) (HF.SIZE 32) (HF.TIME 2.6156e-5)
			  (HF.NAME% :PEDERSEN)
			  (HASH-FUNCTIONS
			   (RELATION
			    (CIRCUIT-TIME HASH-FUNCTION-NAME HASH-FUNCTION-SIZE HASH-FUNCTION-TIME
					  HASH-FUNCTION-CONSTRAINTS)
			    (0.153988 :PEDERSEN 32 2.6156e-5 1324)
			    (0.65253 :BLAKE2S-KDF 32 4.5608e-8 5162)
			    (1.30506 :BLAKE2S 32 9.1216e-8 10324)))
			  (HF.CONSTRAINTS 1324) (HF.CIRCUIT-TIME 0.153988)
			  (HF.HASH-FUNCTION%
			   (TUPLE (CIRCUIT-TIME 0.153988) (HASH-FUNCTION-NAME :PEDERSEN)
				  (HASH-FUNCTION-SIZE 32) (HASH-FUNCTION-TIME 2.6156e-5)
				  (HASH-FUNCTION-CONSTRAINTS 1324)))
			  (HF.HASH-FUNCTIONS%
			   (RELATION
			    (CIRCUIT-TIME HASH-FUNCTION-NAME HASH-FUNCTION-SIZE HASH-FUNCTION-TIME
					  HASH-FUNCTION-CONSTRAINTS)
			    (0.153988 :PEDERSEN 32 2.6156e-5 1324)
			    (0.65253 :BLAKE2S-KDF 32 4.5608e-8 5162)
			    (1.30506 :BLAKE2S 32 9.1216e-8 10324)))))
	 (system (constraint-system
		  ((hf (select-hash-function hf-name hash-functions))))))
    (is (same expected
	      (solve-for system '() data)))))

;; TODO: Add support for (schema) description.
(define-simple-constraint compute-zigzag-layers
    (zigzag-layers (zigzag-epsilon zigzag-delta))
    (floor (+ (log (/ 1
		      (* 3 (- zigzag-epsilon (* 2 zigzag-delta))))
		   2)
	      4)))

(deftransformation compute-zigzag-tapered-layers
    ((zigzag-basic-layer-challenge-factor zigzag-lambda layers zigzag-taper)
     => (layer-index zigzag-layer-challenges))
  (let* ((reduction (- 1 zigzag-taper))
	 (layer-challenges (loop for i from 0 below layers
			      collect (* zigzag-lambda
					 (ceiling (max 20
						       (* zigzag-basic-layer-challenge-factor
							  (expt reduction i))))))))
    (cons (list 0 0) ;; First layer is for CommD, with 0 challenges.
	  (loop for lc in layer-challenges
	     for layer-index downfrom layers
	     collect (list layer-index lc)))))

(deftransformation compute-total-zigzag-challenges
    ((layer-index zigzag-layer-challenges
				    &group-by sector-size
				    &acc (total-zigzag-challenges 0))
     -> (total-zigzag-challenges))
  (values (+ total-zigzag-challenges zigzag-layer-challenges)))

(define-constraint beta-merkle-heights
    (beta-merkle-height (beta-merkle-heights max-beta-merkle-height))
  "Provide BETA-MERKLE-HEIGHT values from 0 to MAX-BETA-MERKLE-HEIGHT."
  ((transformation* ((max-beta-merkle-height) => (beta-merkle-height)) ==
		    (loop for i from 0 to max-beta-merkle-height collect (list i)))))

(test beta-merkle-heights
  (let ((system (constraint-system
		 ((bmh (beta-merkle-heights x))))))
    (is (same (relation (bmh) (0) (1) (2) (3) (4))
	      (ask system '(bmh) (tuple (x 4))))))

  
  ;; FIXME: Make this test work. In general, we should be able to pass constaints to defined constraints. (This is a general constraints issue.)
  #+(or)
  (let ((system (constraint-system
		 ((bmh (beta-merkle-heights 4))))))
    (is (same (relation (bmh) (0) (1) (2) (3) (4))
	      (ask system '(bmh) (tuple))))))

(define-simple-constraint layer-challenge-inclusion-proofs (num-proofs (layer-index total-layers single-challenge-proofs))
    (let ((shifted-proofs 0))
      (cond ((zerop layer-index) shifted-proofs)
	    ((= layer-index total-layers) (- single-challenge-proofs shifted-proofs))
	    (t single-challenge-proofs))))

(test layer-challenge-inclusion-proofs
  (let ((system (constraint-system
		 ((layer-proofs (layer-challenge-inclusion-proofs layer-index total-layers single-challenge-proofs))))))
    (setf (system-data system) (list (relation (layer-index) (0) (1) (2) (10))
				      (tuple (total-layers 10) (single-challenge-proofs 123))))
    (is (same (relation (layer-index layer-proofs)
			(0 0)
			(1 123)
			(2 123)
			(10 123))
	      (ask system '(layer-index layer-proofs) nil)))))

(define-system-constraint zigzag-layer-performance
    (-- (zigzag-layer-performance layers layer-index layer-challenges
				  sector-size node-bytes
				  max-beta-merkle-height
				  circuit-proving-time-per-constraint
				  single-challenge-inclusion-proofs
				  single-challenge-kdf-hashes
				  alpha-hash-function
				  beta-hash-function
				  kdf-hash-function))
  ((beta-merkle-height (beta-merkle-heights max-beta-merkle-height))
   (merkle-tree (hybrid-merkle-tree sector-size node-bytes beta-merkle-height))
   (alpha-hashing-time (* merkle-tree.alpha-hash-count alpha-hash-function.time))
   (beta-hashing-time (* merkle-tree.beta-hash-count beta-hash-function.time))
   
   (alpha-inclusion-proof-constraints (* merkle-tree.alpha-inclusion-proof-hash-length alpha-hash-function.constraints))
   (beta-inclusion-proof-constraints (* merkle-tree.beta-inclusion-proof-hash-length beta-hash-function.constraints))
   (alpha-inclusion-proof-circuit-time (* merkle-tree.alpha-inclusion-proof-hash-length alpha-hash-function.circuit-time))
   (beta-inclusion-proof-circuit-time (* merkle-tree.beta-inclusion-proof-hash-length beta-hash-function.circuit-time))
   (inclusion-proof-constraints (+ alpha-inclusion-proof-constraints beta-inclusion-proof-constraints))
   (inclusion-proof-circuit-time (+ alpha-inclusion-proof-circuit-time beta-inclusion-proof-circuit-time))
   (layer-challenge-proofs (layer-challenge-inclusion-proofs layer-index layers single-challenge-inclusion-proofs))
   (challenge-kdf-constraints (* single-challenge-kdf-hashes kdf-hash-function.constraints))
   (challenge-kdf-circuit-time (* single-challenge-kdf-hashes kdf-hash-function.circuit-time))
   (challenge-inclusion-constraints (* inclusion-proof-constraints layer-challenge-proofs))
   (challenge-inclusion-circuit-time (* inclusion-proof-circuit-time layer-challenge-proofs))
   (challenge-constraints (+ challenge-kdf-constraints challenge-inclusion-constraints))
   (challenge-circuit-time (+ challenge-kdf-circuit-time challenge-inclusion-circuit-time))
   (constraints (* challenge-constraints layer-challenges))
;;    (circuit-time (* constraints circuit-proving-time-per-constraint))
   (circuit-time (* challenge-circuit-time layer-challenges))
   (hashing-time (+ alpha-hashing-time beta-hashing-time))
   (proving-time (+ circuit-time hashing-time))
   ))

(defschema zigzag-layer-performance-schema "Single ZigZag Layer Performance")

(deftransformation compute-total-zigzag-performance
    ;; FIXME: The aggregation removes CIRCUIT-PROVING-TIME-PER-CONSTRAINT from the results.
    ((lowest-time
      optimal-hashing-time
      optimal-circuit-time
      optimal-constraints
      optimal-beta-merkle-height
      layer-index
      &group-by sector-size
      &acc
      (total-proving-time 0)
      (total-hashing-time 0)
      (total-zigzag-constraints 0)
      (optimal-heights (make-relation nil))
      (optimal-constraints 0)
      )
     -> (total-proving-time
	 total-hashing-time
	 total-zigzag-constraints
	 optimal-heights
	 ))
  ;; TODO: Capture layer data as relation-valued attribute along with the aggregates.
  (values (+ total-proving-time lowest-time)
	  (+ total-hashing-time optimal-hashing-time)	  
	  (+ total-zigzag-constraints optimal-constraints)
	  (make-relation
	   (with (tuples optimal-heights)
	  	 (tuple (layer-index layer-index)
	  		(lowest-time lowest-time)
			(hashing-time optimal-hashing-time)
			(circuit-time optimal-circuit-time)
			(constraints optimal-constraints)
	  		(optimal-beta-merkle-height optimal-beta-merkle-height))))
	  ))

;; TODO: Can we use this instead of COMPUTE-ZIGZAG-TAPERED-LAYERS?
;; Problem: the latter 'returns' two values. General solution may be to support exactly that (multiple return values in 'operator'-like call-by-order constraints.
;; (define-simple-constraint zigzag-tapered-layers
;;     (zigzag-tapered-layers (zigzag-basic-layer-challenge-factor zigzag-lambda layers zigzag-taper))
;;     (let* ((reduction (- 1 zigzag-taper))
;; 	   (layer-challenges (loop for i from 0 below layers
;; 				collect (* zigzag-lambda (max 20 (floor (* zigzag-basic-layer-challenge-factor (expt reduction i))))))))
;;       (values (apply #'vector layer-challenges) (reduce #'+ layer-challenges))))

(defschema zigzag-schema
    "ZigZag"
  (node-bytes "The number of bytes in a node -- must also be the hash digest size.")
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
  (non-circuit-proving-time "Time (including replication) to generate a non-circuit proof of replication. Unit: seconds")
  (vector-commitment-time "Time to generate the vector commitments used in a non-circuit proof of replication. Unit: seconds")
  (circuit-proving-time-per-constraint "Groth16 circuit proving time (from benchmarks) per constraint. Unit: seconds")
  (zigzag-total-proving-time "Total time to generate a proof of replication (circuit and non-circuit). Unit: seconds")
  (seal-time "Total time to seal (replication + proving) one sector. Unit: seconds")
  (wall-clock-seal-time "Wall clock time sealing time using SEAL-PARALLELISM cores. Unit: seconds")
  (seal-parallelism "Number of cores utilized when computing wall-clock-seal-time.")
  (sector-GiB "Number of GiB in one sector. Unit: GiB")
  (GiB-seal-time "Total time to seal (replication + proving) one GiB. Unit: seconds")
  (total-parents "Number of parents (or padding) each node uses when performing key derivation.")
  (hash-functions "Alternate hash functions for use in merkle-tree generation, KDF, or other commitments. All values here assume 64 bytes of raw input, but benchmarks include any extra, 'personalization', bits/bytes included as input during merkle-tree construction.")
  (single-kdf-hashes "Number of hashes performed as part of a single application of the key-derivation function (KDF). This is equal to the number of parents and includes the replica ID. Since we use Merkle-Damgard construction, the number of compression hashes is the number of elements - 1.")
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
  (total-zigzag-kdf-hashing-constraints "Total number of kdf hashing constraints in a ZigZag circuit.")
  (total-zigzag-non-hashing-constraints "Total number of hashes which must be verified in a ZigZag circuit.")
  (total-zigzag-circuit-kdf-hashes "Total number of KDF hashes which must be verified in a ZigZag circuit.")

  (total-zigzag-sloth-constraints "Total number of constraints due to sloth verification.")
  (total-zigzag-constraints "Total number of constraints which must be verified in a ZigZag circuit.")
  (layer-index "Index of layer. Unit: integer")
  (layer-replication-time "Time to replicate one layer. Unit: seconds")

  (storage-to-proof-size-ratio "Ratio of sealed sector size to on-chain PoRep size.")
  (storage-to-proof-size-float "Ratio of sealed sector size to on-chain PoRep size -- expressed as a float.")

  ;; (replication-cycles "")
  ;; (sealing-cycles "")
  ;; (zigzag-vanilla-proving-cycles "")
  ;; (zigzag-groth-proving-cycles "")
  ;; (zigzag-total-proving-cycles "")
  ;; (total-seal-cycles "")  

  (single-circuit-proof-size "Size of a single Groth16 Proof. Unit: bytes")
  (total-circuit-proof-size "Total size of a single circuit proof. Unit: bytes")

  (total-challenges "")
  (partition-challenges "")
  )

(define-constraint group-layer-performance
    (lowest-time (group-layer-performance layer-index beta-merkle-height proving-time constraints hashing-time))
  ((transformation* ((layer-index beta-merkle-height proving-time constraints hashing-time
				  &group-by layer-index

				  ;; TODO: Document this pattern (or unsupport it) before deleting this dead code.
				  ;; &group-by 'layer-performance.layer-index
				  ;&group 'layer-performance.*

				  &acc
				  (alternate-layers (make-relation nil))
				  (lowest-time -1)
				  (optimal-beta-merkle-height 0)
				  (optimal-constraints -1)
				  (optimal-hashing-time -1)
				  (optimal-circuit-time -1))
		     -> (alternate-layers lowest-time optimal-beta-merkle-height optimal-constraints optimal-hashing-time optimal-circuit-time))
		    ==
		    (let ((alts
			   ;; TODO: convenience-function for adding tuple to relation.
			   (make-relation		      
			    (with (tuples alternate-layers)
				  (tuple (beta-merkle-height beta-merkle-height)
					 (proving-time proving-time)))))
			  (circuit-time (- proving-time hashing-time)))
		     (cond
		       ((and (= lowest-time -1) (= optimal-constraints -1) (= optimal-hashing-time -1)
			     (= optimal-circuit-time -1))
			 ;; Take first PROVING-TIME seen when LOWEST-TIME still has initial value of -1.
			(values alts proving-time beta-merkle-height constraints hashing-time circuit-time))
		       (t
			(let ((better? (< proving-time lowest-time)))
			  ;; If proving-time is less than lowest-time, this is the best combination.
			  (if better?
			      (values alts proving-time beta-merkle-height constraints hashing-time circuit-time)
			      (values alts lowest-time optimal-beta-merkle-height optimal-constraints optimal-hashing-time optimal-circuit-time)))))))))

(test group-layer-performance
  (let ((system (constraint-system
		 ((grouped (group-layer-performance
			    layer-performance.layer-index
			    layer-performance.beta-merkle-height
			    layer-performance.proving-time
			    layer-performance.constraints
			    layer-performance.hashing-time
			    ))))))
    (is (same
	 (RELATION
	  (OTHER LOWEST-TIME ALTERNATE-LAYERS OPTIMAL-CONSTRAINTS OPTIMAL-CIRCUIT-TIME
		 OPTIMAL-HASHING-TIME OPTIMAL-BETA-MERKLE-HEIGHT
		 LAYER-PERFORMANCE.LAYER-INDEX)
	  (3 4 (RELATION (PROVING-TIME BETA-MERKLE-HEIGHT) (4 0) (5 1) (6 2)) 6 -3 7 0
	     1)
	  (5 1 (RELATION (PROVING-TIME BETA-MERKLE-HEIGHT) (1 0) (2 1) (3 2)) 9 -2 3 0
	     0))
	 (solve-for system '()
		    (relation (layer-performance.layer-index
			       layer-performance.beta-merkle-height
			       layer-performance.proving-time
			       layer-performance.constraints
			       layer-performance.hashing-time
			       other)
			      (0 0 1 9 3 5)
			      (0 1 2 8 2 5)
			      (0 2 3 7 4 5)
			      (1 0 4 6 7 3)
			      (1 1 5 5 5 3)
			      (1 2 6 4 6 3)))))))

(defconstraint-system zigzag-constraint-system
    ;; TODO: Make variadic version of + and ==.
    ((sector-size (* sector-GiB #.GiB))
     (nodes (/ sector-size node-bytes))
     (comm-d-size (== merkle-hash-function.size))
     (comm-r-size (== merkle-hash-function.size))
     (comm-r-star-size (== merkle-hash-function.size))     
     (comm-rs-size (+ comm-r-size comm-r-star-size))
     (commitments-size (+ comm-rs-size comm-d-size))
     (total-circuit-proof-size (* single-circuit-proof-size partitions))
     (on-chain-porep-size (+ commitments-size total-circuit-proof-size))
     (total-challenges (* partitions partition-challenges))

     ;; TODO: There should be a syntax allowing keyword/call-by-name semantics for use cases like this.

     (degree (+ base-degree expansion-degree))
     (total-parents (== degree))

     (kdf-hash-function (select-hash-function kdf-hash-function-name hash-functions))
     (merkle-hash-function (select-hash-function merkle-hash-function-name hash-functions))
     (alpha-hash-function (select-hash-function alpha-hash-function-name hash-functions))
     (beta-hash-function (select-hash-function beta-hash-function-name hash-functions))

     (xxx (alpha-hash-function.constraints))

     (single-kdf-hashes (+ total-parents 1))
     (single-kdf-time (* single-kdf-hashes kdf-hash-function.time))
     (total-nodes-to-encode (* nodes layers))
     (single-node-sloth-time (* sloth-iter single-sloth-iteration-time))
     (single-node-encoding-time (+ single-kdf-time single-node-sloth-time)) ;; Excludes parent loading time.
     
     (single-challenge-inclusion-proofs (+ total-parents 2))
     (single-challenge-kdf-hashes (* single-kdf-hashes 1))
     (single-challenge-sloth-verifications (== sloth-iter))
     
     (total-zigzag-circuit-kdf-hashes (* single-challenge-kdf-hashes total-challenges))
     
     (layer-replication-time (* single-node-encoding-time nodes))
     (replication-time (* layers layer-replication-time))
     (replication-time-per-byte (/ replication-time sector-size))
     (replication-time-per-GiB (* replication-time-per-byte #.(* 1024 1024 1024)))

     (single-layer-merkle-hashing-time (* merkle-tree.hash-count merkle-hash-function.time))
          
     (non-circuit-proving-time (+ replication-time total-merkle-hashing-time))
     
     (circuit-proving-time-per-constraint (/ bench-circuit-proving-time bench-circuit-constraints))

     (total-zigzag-kdf-hashing-constraints (* total-zigzag-circuit-kdf-hashes kdf-hash-function.constraints))
     (total-zigzag-sloth-constraints (* total-challenges single-sloth-iteration-constraints))
     
     (total-zigzag-non-hashing-constraints (+ total-zigzag-sloth-constraints total-zigzag-other-constraints))
     
     (total-zigzag-constraints-z (+ total-zigzag-hashing-constraints total-zigzag-non-hashing-constraints))

     (seal-time (+ replication-time total-proving-time))
     (wall-clock-seal-time (/ total-proving-time seal-parallelism))
     (wall-clock-seal-time-per-byte (/ wall-clock-seal-time sector-size))
     (wall-clock-seal-time-per-gib (* wall-clock-seal-time-per-byte #.(* 1024 1024 1024 )))
     (total-circuit-time (- total-proving-time total-hashing-time))
     (sector-GiB (/ sector-size #.GiB))
     (GiB-seal-time (/ seal-time sector-GiB))
     (storage-to-proof-size-ratio (/ sector-size on-chain-porep-size))
     (storage-to-proof-size-float (* 1.0 storage-to-proof-size-ratio))

     (layer-performance (zigzag-layer-performance layers
						  layer-index
						  zigzag-layer-challenges
						  sector-size node-bytes
						  max-beta-merkle-height
						  circuit-proving-time-per-constraint
						  single-challenge-inclusion-proofs
						  single-challenge-kdf-hashes
						  alpha-hash-function
						  beta-hash-function
						  kdf-hash-function))
     (grouped (group-layer-performance layer-performance.layer-index layer-performance.beta-merkle-height layer-performance.proving-time
				       layer-performance.constraints layer-performance.hashing-time)))
  :schema 'zigzag-schema)

(test zigzag-layer-performance
  (let* ((system (zigzag-system :no-aggregate t))
	 (result (ask system
		      '(layer-performance.layer-index		     
			layer-performance.layer-challenges
			layer-performance.beta-merkle-height
			layer-performance.merkle-tree.alpha-inclusion-proof-hash-length
			layer-performance.merkle-tree.alpha-hash-count		     
			layer-performance.alpha-hash-function.time		     
			layer-performance.alpha-hash-function.constraints
			layer-performance.alpha-inclusion-proof-constraints
			layer-performance.merkle-tree.beta-inclusion-proof-hash-length
			
			layer-performance.beta-hash-function.time		     
			layer-performance.beta-hash-function.constraints
			layer-performance.beta-inclusion-proof-constraints
			
			layer-performance.challenge-constraints
			layer-performance.hashing-time
			layer-performance.circuit-time
			layer-performance.proving-time))))
    (is (same (RELATION
	       (LAYER-PERFORMANCE.LAYER-INDEX LAYER-PERFORMANCE.CIRCUIT-TIME
					      LAYER-PERFORMANCE.LAYER-CHALLENGES LAYER-PERFORMANCE.CHALLENGE-CONSTRAINTS
					      LAYER-PERFORMANCE.BETA-HASH-FUNCTION.TIME
					      LAYER-PERFORMANCE.ALPHA-HASH-FUNCTION.TIME
					      LAYER-PERFORMANCE.MERKLE-TREE.ALPHA-HASH-COUNT
					      LAYER-PERFORMANCE.BETA-HASH-FUNCTION.CONSTRAINTS
					      LAYER-PERFORMANCE.ALPHA-HASH-FUNCTION.CONSTRAINTS
					      LAYER-PERFORMANCE.BETA-INCLUSION-PROOF-CONSTRAINTS
					      LAYER-PERFORMANCE.ALPHA-INCLUSION-PROOF-CONSTRAINTS
					      LAYER-PERFORMANCE.MERKLE-TREE.BETA-INCLUSION-PROOF-HASH-LENGTH
					      LAYER-PERFORMANCE.MERKLE-TREE.ALPHA-INCLUSION-PROOF-HASH-LENGTH)
	       (0 0.0 0 72268 9.1216e-8 2.6156e-5 1073741823 10324 1324 0 39720 0 30)
	       (1 95426.0 160 4718068 9.1216e-8 2.6156e-5 0 10324 1324 309720 0 30 0)
	       (2 95426.0 160 4718068 9.1216e-8 2.6156e-5 0 10324 1324 309720 0 30 0)
	       (3 95426.0 160 4718068 9.1216e-8 2.6156e-5 0 10324 1324 309720 0 30 0)
	       (4 143139.0 240 4718068 9.1216e-8 2.6156e-5 0 10324 1324 309720 0 30 0)
	       (5 209937.19 352 4718068 9.1216e-8 2.6156e-5 0 10324 1324 309720 0 30 0)
	       (6 314905.78 528 4718068 9.1216e-8 2.6156e-5 0 10324 1324 309720 0 30 0)
	       (7 472358.7 792 4718068 9.1216e-8 2.6156e-5 0 10324 1324 309720 0 30 0)
	       (8 710923.7 1192 4718068 9.1216e-8 2.6156e-5 0 10324 1324 309720 0 30 0)
	       (9 1063999.9 1784 4718068 9.1216e-8 2.6156e-5 0 10324 1324 309720 0 30 0)
	       (10 1593614.1 2672 4718068 9.1216e-8 2.6156e-5 0 10324 1324 309720 0 30 0))
	      result))
    (is (= (cardinality result) 11))))

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

(defun zigzag-security-system (&key isolated no-aggregate)
  (make-instance 'system
		 :components `(,(component ('compute-zigzag-tapered-layers))
				,@(when (not no-aggregate)
				    (list (component ('compute-zigzag-tapered-layers))
					  (component ('compute-total-zigzag-challenges)))))
		 :subsystems (list (find-system 'zigzag-security-constraint-system))
		 :data (if isolated
			   (list (tuple (layers 10)) *default-zigzag-security*)
			   (list *default-zigzag-security*))))

(defun zigzag-system (&key no-aggregate)
  (make-instance 'system
		 :components (when (not no-aggregate)
			       (list
				(component ('compute-total-zigzag-performance))))
		 :subsystems (list (find-system 'zigzag-constraint-system)
				   ;; FIXME: If these subsystems are provided in the opposite order,
				   ;; something breaks.
				   (zigzag-security-system :no-aggregate no-aggregate))
		 :data (list* *defaults*
			      *zigzag-defaults*
			      (zigzag-bench-data)
			      *zigzag-hypotheticals*)))

(test optimal-heights
  (is (same
       (RELATION (OPTIMAL-HEIGHTS)
          ((RELATION
            (CONSTRAINTS LAYER-INDEX LOWEST-TIME CIRCUIT-TIME HASHING-TIME
             OPTIMAL-BETA-MERKLE-HEIGHT)
            (0 0 97.94244 0.0 97.94244 30)
            (171690880 1 24432.822 20836.523 3596.2986 3)
            (171690880 2 24432.822 20836.523 3596.2986 3)
            (171690880 3 24432.822 20836.523 3596.2986 3)
            (225136320 4 34205.582 27110.928 7094.655 2)
            (330199936 5 46857.348 39762.69 7094.655 2)
            (424019904 6 64618.906 50527.54 14091.367 1)
            (636029856 7 89882.68 75791.31 14091.367 1)
            (796337056 8 121573.375 93488.586 28084.791 0)
            (1191833312 9 168003.95 139919.16 28084.791 0)
            (1785077696 10 237649.81 209565.02 28084.791 0))))
       
       (ask (zigzag-system) '(optimal-heights)))))

#+(or) ;; FIXME
(test query-for-grouped-attributes
  ;; ZIGZAG-LAYER-CHALLENGES is not available in the final result because it has been grouped,
  ;; but it results in a valid plan because it is an intermediate product.
  ;; Probably PLAN should ensure that it's required outputs are indeed outputs of the generated plan
  ;; and return NIL if not.
  (is (same (fset:set 'sector-size 'zigzag-layer-challenges)
	    (attributes (ask (zigzag-system) '(sector-size zigzag-layer-challenges))))))
