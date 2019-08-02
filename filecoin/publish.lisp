(in-package :filecoin)
(in-suite filecoin-suite)

(defun publish-schema (system)
  (loop for schema in (all-system-schemas system)
     append (loop for parameter in (schema-parameters schema)
               collect (tuple (name (symbol-name (parameter-name parameter)))
			      (description (parameter-description parameter))))))

(defun publish (system)
  (let ((assignments (solve-for system '() nil)))
    (tuple (schema (publish-schema system))
	   (assignments assignments))))

(defun publish-filecoin ()
  (let ((published (publish (filecoin-system))))
    (with-attributes (assignments) published
      (setf assignments (extract assignments :error t)))
    published))

(defparameter *filecoin-json-path* "filecoin/json/filecoin.json")

(defun publish-filecoin-json (&optional (where *filecoin-json-path*))
  (typecase where
    ((or string pathname)
     (let* ((pathname (project-merge where))
	    (tmp-pathname (merge-pathnames (make-pathname :type "tmp") pathname)))
       (ensure-directories-exist pathname)
       (with-open-file (out tmp-pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
	 (json:encode-json (publish-filecoin) out))
       (uiop:run-program (format nil "cat ~a | jq > ~a" tmp-pathname pathname))
       (delete-file tmp-pathname)
       pathname))
    (null (with-output-to-string (out) (json:encode-json (publish-filecoin) out)))
    (t (json:encode-json (publish-filecoin) where))))

(eval-when (:load-toplevel :execute)
  (publish-filecoin-json))

(test published-representation
  (is
   (same
    '(TUPLE
      (SCHEMA
       ((TUPLE (NAME "AWS-GLACIER-PRICE")
	       (DESCRIPTION
		"Cost of one GiB storage from AWS glacier for one month. Unit: dollars"))
	(TUPLE (NAME "ANNUAL-INCOME")
	       (DESCRIPTION
		"Annual income from selling storage on the storage market. Unit: dollars"))
	(TUPLE (NAME "MONTHLY-INCOME")
	       (DESCRIPTION
		"Monthly income from selling storage on the storage market. Unit: dollars"))
	(TUPLE (NAME "COMPARABLE-MONTHLY-COST")
	       (DESCRIPTION
		"Expected cost of purchasing monthly storage from commodity provider. Unit: dollars"))
	(TUPLE (NAME "GIB-HOUR-SEAL-INVESTMENT")
	       (DESCRIPTION
		"Cost of investment required to seal one GiB in one hour at scale. Unit: dollars"))
	(TUPLE (NAME "GIB-SEAL-COST")
	       (DESCRIPTION "Cost of sealing one GiB. Unit: dollars"))
	(TUPLE (NAME "COMMODITY-STORAGE-DISCOUNT")
	       (DESCRIPTION
		"Fraction of commodity storage pricing expected as income from storage market. Unit: decimal fraction"))
	(TUPLE (NAME "MINER-MONTHS-TO-CAPACITY")
	       (DESCRIPTION
		"Months it should take a miner to reach full storage capacity. Unit: months"))
	(TUPLE (NAME "GIB-CAPACITY")
	       (DESCRIPTION "GiB of storage at full capacity. Unit: GiB"))
	(TUPLE (NAME "TIB-CAPACITY")
	       (DESCRIPTION "TiB of storage at full capacity. Unit: TiB"))
	(TUPLE (NAME "ANNUAL-TIB")
	       (DESCRIPTION
		"TiB of storage which must be brought online per year. Unit: TiB"))
	(TUPLE (NAME "MONTHLY-TIB")
	       (DESCRIPTION
		"TiB of storage which must be brought online per month. Unit: TiB"))
	(TUPLE (NAME "DAILY-TIB")
	       (DESCRIPTION
		"TiB of storage which must be brought online per day. Unit: TiB"))
	(TUPLE (NAME "HOURLY-TIB")
	       (DESCRIPTION
		"TiB of storage which must be brought online per hour. Unit: TiB"))
	(TUPLE (NAME "HOURLY-GIB")
	       (DESCRIPTION
		"GiB of storage which must be brought online per hour. Unit: GiB"))
	(TUPLE (NAME "SEAL-CYCLES-PER-HOUR")
	       (DESCRIPTION
		"CPU required to seal at required rate for one hour. Unit: cycles"))
	(TUPLE (NAME "SEAL-CYCLES-PER-MINUTE")
	       (DESCRIPTION
		"CPU required to seal at required rate for one minute. Unit: cycles"))
	(TUPLE (NAME "SEAL-CYCLES-PER-SECOND")
	       (DESCRIPTION
		"CPU required to seal at required rate for one second. Unit: cycles"))
	(TUPLE (NAME "GIB-SEAL-CYCLES")
	       (DESCRIPTION
		"Total CPU cycles required to seal 1 GiB. Unit: cycles"))
	(TUPLE (NAME "NEEDED-GHZ")
	       (DESCRIPTION
		"Total GHz capacity needed to seal at the required rate."))
	(TUPLE (NAME "UP-FRONT-DRIVE-COST")
	       (DESCRIPTION
		"Up-front investment in hard drives required to store sufficient sealed data. Unit: dollars."))
	(TUPLE (NAME "UP-FRONT-MEMORY-COST")
	       (DESCRIPTION
		"Up-front investment in memory (RAM) required to seal at necessary rate. Unit: dollars"))
	(TUPLE (NAME "UP-FRONT-COMPUTE-COST")
	       (DESCRIPTION
		"Up-front investement in compute hardware required to seal at necessary rate. Unit: dollars"))
	(TUPLE (NAME "UP-FRONT-SEALING-COST")
	       (DESCRIPTION
		"Up-front investment in total hardware require to seal at necessary rate. Unit: dollars"))
	(TUPLE (NAME "TOTAL-UP-FRONT-COST")
	       (DESCRIPTION
		"Total up-front investment required to generate MONTHLY-INCOME. Unit: dollars"))
	(TUPLE (NAME "AVERAGE-MONTHLY-INCOME-DURING-RAMP-UP")
	       (DESCRIPTION
		"Average monthly income before miner reaches capacity (assuming linear growth). Unit: dollars"))
	(TUPLE (NAME "INCOME-DURING-RAMP-UP")
	       (DESCRIPTION
		"Total income during ramp-up period (before reaching capacity). Unit: dollars"))
	(TUPLE (NAME "INCOME-TO-FGR-AT-CAPACITY")
	       (DESCRIPTION
		"Income still required to reach filecoin growth rate equilibrium after reaching capacity. Unit: dollars"))
	(TUPLE (NAME "FGR-MONTHS-AT-CAPACITY")
	       (DESCRIPTION
		"Months needed after reaching capacity before filecoin growth rate equilibrium."))
	(TUPLE (NAME "FGR-MONTHS")
	       (DESCRIPTION
		"Months after which a miner should reach filecoin growth rate equilibrium."))
	(TUPLE (NAME "ONE-YEAR-PROFIT-MONTHS")
	       (DESCRIPTION "Months from FGR to one year of profit. Unit: months"))
	(TUPLE (NAME "ONE-YEAR-PROFIT")
	       (DESCRIPTION "Profit after one year of operation: Unit: dollars"))
	(TUPLE (NAME "ONE-YEAR-FGR")
	       (DESCRIPTION "FGR after one year of operation: Unit: fraction"))
	(TUPLE (NAME "TWO-YEAR-PROFIT-MONTHS")
	       (DESCRIPTION "Months from FGR to two years of profit. Unit: months"))
	(TUPLE (NAME "TWO-YEAR-PROFIT")
	       (DESCRIPTION "Profit after two years of operation: Unit: dollars"))
	(TUPLE (NAME "TWO-YEAR-FGR")
	       (DESCRIPTION "FGR after two years of operation: Unit: fraction"))
	(TUPLE (NAME "THREE-YEAR-PROFIT-MONTHS")
	       (DESCRIPTION
		"Months from FGR to three years of profit. Unit: months"))
	(TUPLE (NAME "THREE-YEAR-PROFIT")
	       (DESCRIPTION "Profit after three years of operation: Unit: dollars"))
	(TUPLE (NAME "THREE-YEAR-FGR")
	       (DESCRIPTION "FGR after three years of operation: Unit: fraction"))
	(TUPLE (NAME "SEAL-HZ")
	       (DESCRIPTION
		"Cycles per second at which the sealing machine operates. Unit: Hz"))
	(TUPLE (NAME "SEAL-GHZ")
	       (DESCRIPTION
		"Cycles per second at which the sealing machine operates. Unit: GHz"))
	(TUPLE (NAME "NODE-BYTES")
	       (DESCRIPTION
		"The number of bytes in a node -- must also be the hash digest size."))
	(TUPLE (NAME "SECTOR-GIB") (DESCRIPTION "Size of one sector. Unit: GiB"))
	(TUPLE (NAME "SECTOR-SIZE") (DESCRIPTION "Size of one sector. Unit: bytes"))
	(TUPLE (NAME "COMM-D-SIZE")
	       (DESCRIPTION "Size of the data commitment (CommD). Unit: bytes"))
	(TUPLE (NAME "COMM-R-SIZE")
	       (DESCRIPTION "Size of the replica commitment (CommR). Unit: bytes"))
	(TUPLE (NAME "COMM-R-STAR-SIZE")
	       (DESCRIPTION
		"Size of the aggregated commitment to each layer's replica (CommR*). Unit: bytes"))
	(TUPLE (NAME "COMM-RS-SIZE")
	       (DESCRIPTION "Size of all replica commitments. Unit: bytes"))
	(TUPLE (NAME "COMMITMENTS-SIZE")
	       (DESCRIPTION
		"Size of all commitments returned by Seal. Unit: bytes"))
	(TUPLE (NAME "ON-CHAIN-POREP-SIZE")
	       (DESCRIPTION
		"On-chain size of one Seal proof plus commitments. Unit: bytes"))
	(TUPLE (NAME "DEGREE") (DESCRIPTION "Total in-degree of the ZigZag graph."))
	(TUPLE (NAME "BASE-DEGREE")
	       (DESCRIPTION "In-degree of the base depth-robust graph (DRG)."))
	(TUPLE (NAME "EXPANSION-DEGREE")
	       (DESCRIPTION
		"Maximum in-degree of the bipartite expander graph component of a ZigZag graph."))
	(TUPLE (NAME "SLOTH-ITER")
	       (DESCRIPTION
		"Number of iterations of sloth verifiable delay encoding (VDE) to perform."))
	(TUPLE (NAME "PARTITIONS")
	       (DESCRIPTION
		"Number of circuit partitions into which a proof is divided."))
	(TUPLE (NAME "REPLICATION-TIME")
	       (DESCRIPTION "Time to replicate one sector. Unit: seconds"))
	(TUPLE (NAME "REPLICATION-TIME-PER-BYTE")
	       (DESCRIPTION "Time to replicate one byte. Unit: seconds / byte"))
	(TUPLE (NAME "REPLICATION-TIME-PER-GIB")
	       (DESCRIPTION "Time to replicate one GiB. Unit: seconds / GiB"))
	(TUPLE (NAME "SEALING-TIME")
	       (DESCRIPTION
		"Total CPU time to seal (replicate + generate proof of replication) one sector. Unit: seconds"))
	(TUPLE (NAME "NON-CIRCUIT-PROVING-TIME")
	       (DESCRIPTION
		"Time (including replication) to generate a non-circuit proof of replication. Unit: seconds"))
	(TUPLE (NAME "VECTOR-COMMITMENT-TIME")
	       (DESCRIPTION
		"Time to generate the vector commitments used in a non-circuit proof of replication. Unit: seconds"))
	(TUPLE (NAME "CIRCUIT-PROVING-TIME-PER-CONSTRAINT")
	       (DESCRIPTION
		"Groth16 circuit proving time (from benchmarks) per constraint. Unit: seconds"))
	(TUPLE (NAME "ZIGZAG-TOTAL-PROVING-TIME")
	       (DESCRIPTION
		"Total time to generate a proof of replication (circuit and non-circuit). Unit: seconds"))
	(TUPLE (NAME "SEAL-TIME")
	       (DESCRIPTION
		"Total time to seal (replication + proving) one sector. Unit: seconds"))
	(TUPLE (NAME "WALL-CLOCK-SEAL-TIME")
	       (DESCRIPTION
		"Wall clock time sealing time using SEAL-PARALLELISM cores. Unit: seconds"))
	(TUPLE (NAME "SEAL-PARALLELISM")
	       (DESCRIPTION
		"Number of cores utilized when computing wall-clock-seal-time."))
	(TUPLE (NAME "SECTOR-GIB")
	       (DESCRIPTION "Number of GiB in one sector. Unit: GiB"))
	(TUPLE (NAME "GIB-SEAL-TIME")
	       (DESCRIPTION
		"Total time to seal (replication + proving) one GiB. Unit: seconds"))
	(TUPLE (NAME "TOTAL-PARENTS")
	       (DESCRIPTION
		"Number of parents (or padding) each node uses when performing key derivation."))
	(TUPLE (NAME "HASH-FUNCTIONS")
	       (DESCRIPTION
		"Alternate hash functions for use in merkle-tree generation, KDF, or other commitments. All values here assume 64 bytes of raw input, but benchmarks include any extra, 'personalization', bits/bytes included as input during merkle-tree construction."))
	(TUPLE (NAME "SINGLE-KDF-HASHES")
	       (DESCRIPTION
		"Number of hashes performed as part of a single application of the key-derivation function (KDF). This is equal to the number of parents and includes the replica ID. Since we use Merkle-Damgard construction, the number of compression hashes is the number of elements - 1."))
	(TUPLE (NAME "KDF-HASHES")
	       (DESCRIPTION
		"Number of hashes performed as part of the key-derivation function (KDF)."))
	(TUPLE (NAME "SINGLE-KDF-TIME")
	       (DESCRIPTION "Hashing time to perform a single KDF. Unit: seconds"))
	(TUPLE (NAME "SINGLE-LAYER-MERKLE-HASHING-TIME")
	       (DESCRIPTION
		"Merkle hashing time for a single layer. Unit: seconds"))
	(TUPLE (NAME "TOTAL-MERKLE-TREES")
	       (DESCRIPTION "Total merkle trees which must be generated."))
	(TUPLE (NAME "TOTAL-MERKLE-HASHING-TIME")
	       (DESCRIPTION
		"Total time to generate all merkle trees. Unit: seconds"))
	(TUPLE (NAME "TOTAL-NODES-TO-ENCODE")
	       (DESCRIPTION "Total nodes to encode across all layers."))
	(TUPLE (NAME "SINGLE-NODE-SLOTH-TIME")
	       (DESCRIPTION
		"Time to perform sloth (VDE) for a single node. Unit: seconds"))
	(TUPLE (NAME "SINGLE-NODE-ENCODING-TIME")
	       (DESCRIPTION "Time to encode a single node. Unit: seconds"))
	(TUPLE (NAME "SINGLE-CHALLENGE-INCLUSION-PROOFS")
	       (DESCRIPTION
		"Number of inclusion proofs which must be verified for a single challenge."))
	(TUPLE (NAME "SINGLE-CHALLENGE-MERKLE-HASES")
	       (DESCRIPTION
		"Number of merkle hashes which must be verified for a single challenge."))
	(TUPLE (NAME "SINGLE-CHALLENGE-KDF-HASHES")
	       (DESCRIPTION
		"Number of KDF hashes which must be verified for a single challenge."))
	(TUPLE (NAME "SINGLE-CHALLENGE-SLOTH-VERIFICATIONS")
	       (DESCRIPTION
		"Number of sloth iterations which must be verified for a single challenge."))
	(TUPLE (NAME "TOTAL-KDF-HASHES")
	       (DESCRIPTION
		"Total number of KDF (key-derivation function) required during replication."))
	(TUPLE (NAME "TOTAL-ZIGZAG-KDF-HASHING-CONSTRAINTS")
	       (DESCRIPTION
		"Total number of kdf hashing constraints in a ZigZag circuit."))
	(TUPLE (NAME "TOTAL-ZIGZAG-NON-HASHING-CONSTRAINTS")
	       (DESCRIPTION
		"Total number of hashes which must be verified in a ZigZag circuit."))
	(TUPLE (NAME "TOTAL-ZIGZAG-CIRCUIT-KDF-HASHES")
	       (DESCRIPTION
		"Total number of KDF hashes which must be verified in a ZigZag circuit."))
	(TUPLE (NAME "TOTAL-ZIGZAG-SLOTH-CONSTRAINTS")
	       (DESCRIPTION
		"Total number of constraints due to sloth verification."))
	(TUPLE (NAME "TOTAL-ZIGZAG-CONSTRAINTS")
	       (DESCRIPTION
		"Total number of constraints which must be verified in a ZigZag circuit."))
	(TUPLE (NAME "LAYER-INDEX") (DESCRIPTION "Index of layer. Unit: integer"))
	(TUPLE (NAME "LAYER-REPLICATION-TIME")
	       (DESCRIPTION "Time to replicate one layer. Unit: seconds"))
	(TUPLE (NAME "STORAGE-TO-PROOF-SIZE-RATIO")
	       (DESCRIPTION "Ratio of sealed sector size to on-chain PoRep size."))
	(TUPLE (NAME "STORAGE-TO-PROOF-SIZE-FLOAT")
	       (DESCRIPTION
		"Ratio of sealed sector size to on-chain PoRep size -- expressed as a float."))
	(TUPLE (NAME "SINGLE-CIRCUIT-PROOF-SIZE")
	       (DESCRIPTION "Size of a single Groth16 Proof. Unit: bytes"))
	(TUPLE (NAME "TOTAL-CIRCUIT-PROOF-SIZE")
	       (DESCRIPTION "Total size of a single circuit proof. Unit: bytes"))
	(TUPLE (NAME "TOTAL-CHALLENGES") (DESCRIPTION ""))
	(TUPLE (NAME "PARTITION-CHALLENGES") (DESCRIPTION ""))
	(TUPLE (NAME "KDF-HASH-FUNCTION.SIZE")
	       (DESCRIPTION
		"Size of digest (output) generated by *.HASH-FUNCTION."))
	(TUPLE (NAME "KDF-HASH-FUNCTION.CIRCUIT-TIME")
	       (DESCRIPTION "Time for one *.HASH-FUNCTION in circuit."))
	(TUPLE (NAME "KDF-HASH-FUNCTION.CONSTRAINTS")
	       (DESCRIPTION
		"Number of constraints required to prove *.HASH-FUNCTION in circuit."))
	(TUPLE (NAME "KDF-HASH-FUNCTION.HASH-FUNCTION%")
	       (DESCRIPTION
		"Tuple containing the selected hash function's characteristics."))
	(TUPLE (NAME "MERKLE-HASH-FUNCTION.SIZE")
	       (DESCRIPTION
		"Size of digest (output) generated by *.HASH-FUNCTION."))
	(TUPLE (NAME "MERKLE-HASH-FUNCTION.CIRCUIT-TIME")
	       (DESCRIPTION "Time for one *.HASH-FUNCTION in circuit."))
	(TUPLE (NAME "MERKLE-HASH-FUNCTION.CONSTRAINTS")
	       (DESCRIPTION
		"Number of constraints required to prove *.HASH-FUNCTION in circuit."))
	(TUPLE (NAME "MERKLE-HASH-FUNCTION.HASH-FUNCTION%")
	       (DESCRIPTION
		"Tuple containing the selected hash function's characteristics."))
	(TUPLE (NAME "ALPHA-HASH-FUNCTION.SIZE")
	       (DESCRIPTION
		"Size of digest (output) generated by *.HASH-FUNCTION."))
	(TUPLE (NAME "ALPHA-HASH-FUNCTION.CIRCUIT-TIME")
	       (DESCRIPTION "Time for one *.HASH-FUNCTION in circuit."))
	(TUPLE (NAME "ALPHA-HASH-FUNCTION.CONSTRAINTS")
	       (DESCRIPTION
		"Number of constraints required to prove *.HASH-FUNCTION in circuit."))
	(TUPLE (NAME "ALPHA-HASH-FUNCTION.HASH-FUNCTION%")
	       (DESCRIPTION
		"Tuple containing the selected hash function's characteristics."))
	(TUPLE (NAME "BETA-HASH-FUNCTION.SIZE")
	       (DESCRIPTION
		"Size of digest (output) generated by *.HASH-FUNCTION."))
	(TUPLE (NAME "BETA-HASH-FUNCTION.CIRCUIT-TIME")
	       (DESCRIPTION "Time for one *.HASH-FUNCTION in circuit."))
	(TUPLE (NAME "BETA-HASH-FUNCTION.CONSTRAINTS")
	       (DESCRIPTION
		"Number of constraints required to prove *.HASH-FUNCTION in circuit."))
	(TUPLE (NAME "BETA-HASH-FUNCTION.HASH-FUNCTION%")
	       (DESCRIPTION
		"Tuple containing the selected hash function's characteristics."))
	(TUPLE (NAME "ZIGZAG-SOUNDNESS")
	       (DESCRIPTION "ZigZag soundness: Unit fraction"))
	(TUPLE (NAME "ZIGZAG-LAMBDA") (DESCRIPTION "ZigZag soundness: Unit bits"))
	(TUPLE (NAME "ZIGZAG-EPSILON")
	       (DESCRIPTION
		"Maximum allowable deletion (space tightness): Unit: fraction"))
	(TUPLE (NAME "ZIGZAG-DELTA")
	       (DESCRIPTION
		"Maximum allowable cheating on labels (block corruption)"))
	(TUPLE (NAME "ZIGZAG-BASIC-LAYER-CHALLENGES")
	       (DESCRIPTION
		"Multiple of lambda challenges per layer, without tapering optimization."))
	(TUPLE (NAME "ZIGZAG-BASIC-LAYER-CHALLENGE-FACTOR")
	       (DESCRIPTION
		"Number of challenges which, when multiplied by lambda, yields the number of challenges per layer without tapering optimization."))
	(TUPLE (NAME "ZIGZAG-SPACE-GAP")
	       (DESCRIPTION
		"Maximum allowable gap between actual and claimed storage. Unit: fraction"))
	(TUPLE (NAME "ZIGZAG-LAYER-CHALLENGES")
	       (DESCRIPTION
		"Number of challenges in this (indexed) layer of ZigZag PoRep. Unit: integer"))
	(TUPLE (NAME "LAYERS")
	       (DESCRIPTION
		"Number of layers specified for this construction (not necessarily same as calculated from security parameters)."))
	(TUPLE (NAME "MUST-HAVE-FILECOIN")
	       (DESCRIPTION "WE MUST HAVE FILECOIN -- SWEET, SWEET FILECOIN."))
	(TUPLE (NAME "SPACE-GAP-SATISFIED")
	       (DESCRIPTION
		"Is the actual space gap less than or equal to the maximum allowable space gap?"))
	(TUPLE (NAME "FILECOIN-REQUIREMENTS-SATISFIED")
	       (DESCRIPTION "Are the Filecoin requirements all satisfied?"))
	(TUPLE (NAME "MINIMUM-ONE-YEAR-FGR")
	       (DESCRIPTION "Minimum allowable FGR after one year."))
	(TUPLE (NAME "MINIMUM-TWO-YEAR-FGR")
	       (DESCRIPTION "Minimum allowable FGR after two years."))
	(TUPLE (NAME "MINIMUM-THREE-YEAR-FGR")
	       (DESCRIPTION "Minimum allowable FGR after three years."))
	(TUPLE (NAME "MINIMUM-STORAGE-TO-PROOF-SIZE-RATIO")
	       (DESCRIPTION "Minimum necessary storage to on-chain proof ratio."))
	(TUPLE (NAME "PROFIT") (DESCRIPTION "… Profit."))))
      (ASSIGNMENTS
       (TUPLE
	(OPTIMAL-HEIGHTS
	 (RELATION
	  (CIRCUIT-TIME HASHING-TIME LAYER-INDEX CONSTRAINTS LOWEST-TIME
			OPTIMAL-BETA-MERKLE-HEIGHT)
	  (0.0 97.94244 0 0 97.94244 30)
	  (20836.523 3596.2986 1 171690880 24432.822 3)
	  (20836.523 3596.2986 2 171690880 24432.822 3)
	  (20836.523 3596.2986 3 171690880 24432.822 3)
	  (27110.928 7094.655 4 225136320 34205.582 2)
	  (39762.69 7094.655 5 330199936 46857.348 2)
	  (50527.54 14091.367 6 424019904 64618.906 1)
	  (75791.31 14091.367 7 636029856 89882.68 1)
	  (93488.586 28084.791 8 796337056 121573.375 0)
	  (139919.16 28084.791 9 1191833312 168003.95 0)
	  (209565.02 28084.791 10 1785077696 237649.81 0)))
	(NODES 1073741824) (DEGREE 13) (LAYERS 10) (PROFIT T) (SEAL-HZ 5.0e9)
	(SEAL-GHZ 5.0) (DAILY-TIB 1.1147972) (SEAL-TIME 843044.06)
	(FGR-MONTHS 6.409414) (HOURLY-GIB 47.564682) (HOURLY-TIB 0.046449885)
	(NEEDED-GHZ 1740.4132) (NODE-BYTES 32) (PARTITIONS 1) (SECTOR-GIB 32)
	(SLOTH-ITER 0) (BASE-DEGREE 5) (COMM-D-SIZE 32) (COMM-R-SIZE 32)
	(MONTHLY-TIB 33.908417) (SECTOR-SIZE 34359738368) (COMM-RS-SIZE 64)
	(CPU-GHZ-COST 10.0) (GIB-CAPACITY 104166.66) (ONE-YEAR-FGR 1.1387482)
	(TIB-CAPACITY 101.72525) (TWO-YEAR-FGR 3.5830317) (ZIGZAG-DELTA 0.003)
	(ZIGZAG-TAPER 1/3) (ANNUAL-INCOME 50000.0) (GIB-SEAL-COST 0.16707969)
	(GIB-SEAL-TIME 26345.127) (TOTAL-PARENTS 13) (ZIGZAG-LAMBDA 8)
	(ZIGZAG-LAYERS 12)
	(HASH-FUNCTIONS
	 (RELATION
	  (HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-NAME
			      CIRCUIT-TIME HASH-FUNCTION-SIZE)
	  (2.6156e-5 1324 :PEDERSEN 0.153988 32)
	  (4.5608e-8 5162 :BLAKE2S-KDF 0.65253 32)
	  (9.1216e-8 10324 :BLAKE2S 1.30506 32)))
	(MONTHLY-INCOME 4166.6665) (THREE-YEAR-FGR 6.027315) (TIB-DRIVE-COST 30.0)
	(ZIGZAG-EPSILON 0.007) (GIB-SEAL-CYCLES 1.3172563e14)
	(ONE-YEAR-PROFIT 23294.107) (SINGLE-KDF-TIME 6.38512e-7)
	(TWO-YEAR-PROFIT 73294.1)
	(ALTERNATE-LAYERS
	 (RELATION (PROVING-TIME BETA-MERKLE-HEIGHT) (237649.81 0) (269791.34 1)
		   (308929.63 2) (351566.25 3) (395952.03 4) (441212.38 5)
		   (486910.06 6) (532826.4 7) (578852.0 8) (624932.3 9)
		   (671039.94 10) (717161.25 11) (763289.4 12) (809420.9 13)
		   (855554.1 14) (901688.4 15) (947822.8 16) (993957.5 17)
		   (1040092.5 18) (1086227.4 19) (1132362.4 20) (1178497.4 21)
		   (1224632.3 22) (1270767.1 23) (1316902.3 24) (1363037.1 25)
		   (1409172.3 26) (1455307.1 27) (1501442.0 28) (1547577.1 29)
		   (1593712.1 30)))
	(COMM-R-STAR-SIZE 32) (COMMITMENTS-SIZE 96) (EXPANSION-DEGREE 8)
	(REPLICATION-TIME 6855.9707) (SEAL-PARALLELISM 14) (TOTAL-CHALLENGES 2672)
	(ZIGZAG-SOUNDNESS 0.00390625) (ZIGZAG-SPACE-GAP 0.01)
	(AWS-GLACIER-PRICE 0.004) (SINGLE-KDF-HASHES 14)
	(THREE-YEAR-PROFIT 123294.1) (MUST-HAVE-FILECOIN T)
	(TOTAL-CIRCUIT-TIME 698674.8) (TOTAL-HASHING-TIME 137513.25)
	(TOTAL-PROVING-TIME 836188.06) (ON-CHAIN-POREP-SIZE 288)
	(SPACE-GAP-SATISFIED T) (TOTAL-UP-FRONT-COST 20455.89)
	(UP-FRONT-DRIVE-COST 3051.7576) (MINIMUM-ONE-YEAR-FGR -2)
	(MINIMUM-TWO-YEAR-FGR 0.25) (PARTITION-CHALLENGES 2672)
	(SEAL-CYCLES-PER-HOUR 6.2654876e15) (UP-FRONT-MEMORY-COST 0.0)
	(WALL-CLOCK-SEAL-TIME 59727.72) (INCOME-DURING-RAMP-UP 6250.0)
	(TOTAL-NODES-TO-ENCODE 10737418240) (UP-FRONT-COMPUTE-COST 17404.133)
	(UP-FRONT-SEALING-COST 17404.133) (FGR-MONTHS-AT-CAPACITY 3.4094138)
	(KDF-HASH-FUNCTION-NAME :BLAKE2S-KDF) (KDF-HASH-FUNCTION.SIZE 32)
	(KDF-HASH-FUNCTION.TIME 4.5608e-8) (LAYER-REPLICATION-TIME 685.59705)
	(MAX-BETA-MERKLE-HEIGHT 30) (MINIMUM-THREE-YEAR-FGR 1.0)
	(ONE-YEAR-FGR-SATISFIED T) (ONE-YEAR-PROFIT-MONTHS 5.590586)
	(SEAL-CYCLES-PER-MINUTE 1.0442479e14) (SEAL-CYCLES-PER-SECOND 1.7404132e12)
	(SINGLE-NODE-SLOTH-TIME 0) (TWO-YEAR-FGR-SATISFIED T)
	(TWO-YEAR-PROFIT-MONTHS 17.590586) (BETA-HASH-FUNCTION-NAME :BLAKE2S)
	(BETA-HASH-FUNCTION.SIZE 32) (BETA-HASH-FUNCTION.TIME 9.1216e-8)
	(COMPARABLE-MONTHLY-COST 416.66666) (KDF-HASH-FUNCTION.NAME% :BLAKE2S-KDF)
	(TOTAL-ZIGZAG-CHALLENGES 2672) (ALPHA-HASH-FUNCTION-NAME :PEDERSEN)
	(ALPHA-HASH-FUNCTION.SIZE 32) (ALPHA-HASH-FUNCTION.TIME 2.6156e-5)
	(BETA-HASH-FUNCTION.NAME% :BLAKE2S) (FILECOIN-FGR-REQUIREMENT T)
	(GIB-HOUR-SEAL-INVESTMENT 365.90454) (LAYER-PERFORMANCE.LAYERS 10)
	(MINER-MONTHS-TO-CAPACITY 3) (REPLICATION-TIME-PER-GIB 214.24908)
	(THREE-YEAR-FGR-SATISFIED T) (THREE-YEAR-PROFIT-MONTHS 29.590586)
	(TOTAL-CIRCUIT-PROOF-SIZE 192) (TOTAL-ZIGZAG-CONSTRAINTS 5903706720)
	(ALPHA-HASH-FUNCTION.NAME% :PEDERSEN) (BENCH-CIRCUIT-CONSTRAINTS 10324)
	(INCOME-TO-FGR-AT-CAPACITY 14205.891) (MERKLE-HASH-FUNCTION-NAME :PEDERSEN)
	(MERKLE-HASH-FUNCTION.SIZE 32) (MERKLE-HASH-FUNCTION.TIME 2.6156e-5)
	(REPLICATION-TIME-PER-BYTE 1.9953501e-7) (SINGLE-CIRCUIT-PROOF-SIZE 192)
	(SINGLE-NODE-ENCODING-TIME 6.38512e-7) (BENCH-CIRCUIT-PROVING-TIME 0.14464)
	(COMMODITY-STORAGE-DISCOUNT 10) (FILECOIN-FGR-REQUIREMENT-1 T)
	(MERKLE-HASH-FUNCTION.NAME% :PEDERSEN) (TOTAL-UNTAPERED-CHALLENGES 32000.0)
	(MAXIMUM-ALLOWABLE-SPACE-GAP 0.02) (SINGLE-CHALLENGE-KDF-HASHES 14)
	(SINGLE-SLOTH-ITERATION-TIME 123) (STORAGE-TO-PROOF-SIZE-FLOAT 1.1930465e8)
	(STORAGE-TO-PROOF-SIZE-RATIO 1073741824/9) (LAYER-PERFORMANCE.NODE-BYTES 32)
	(WALL-CLOCK-SEAL-TIME-PER-GIB 1866.4912)
	(KDF-HASH-FUNCTION.CONSTRAINTS 5162) (LAYER-PERFORMANCE.LAYER-INDEX 10)
	(LAYER-PERFORMANCE.SECTOR-SIZE 34359738368)
	(WALL-CLOCK-SEAL-TIME-PER-BYTE 1.7383054e-6)
	(ZIGZAG-BASIC-LAYER-CHALLENGES 2666.6667)
	(BETA-HASH-FUNCTION.CONSTRAINTS 10324)
	(KDF-HASH-FUNCTION.CIRCUIT-TIME 0.65253)
	(LAYER-PERFORMANCE.CIRCUIT-TIME 1593614.1)
	(TOTAL-ZIGZAG-OTHER-CONSTRAINTS 0) (TOTAL-ZIGZAG-SLOTH-CONSTRAINTS 857712)
	(ALPHA-HASH-FUNCTION.CONSTRAINTS 1324)
	(ALPHA-MERKLE-HASH-FUNCTION-NAME :PEDERSEN)
	(BETA-HASH-FUNCTION.CIRCUIT-TIME 1.30506)
	(FILECOIN-REQUIREMENTS-SATISFIED T) (TOTAL-ZIGZAG-CIRCUIT-KDF-HASHES 37408)
	(ALPHA-HASH-FUNCTION.CIRCUIT-TIME 0.153988)
	(FILECOIN-STORAGE-RATIO-SATISFIED T)
	(KDF-HASH-FUNCTION.HASH-FUNCTION%
	 (TUPLE (CIRCUIT-TIME 0.65253) (HASH-FUNCTION-NAME :BLAKE2S-KDF)
		(HASH-FUNCTION-SIZE 32) (HASH-FUNCTION-TIME 4.5608e-8)
		(HASH-FUNCTION-CONSTRAINTS 5162)))
	(MERKLE-HASH-FUNCTION.CONSTRAINTS 1324)
	(BETA-HASH-FUNCTION.HASH-FUNCTION%
	 (TUPLE (CIRCUIT-TIME 1.30506) (HASH-FUNCTION-NAME :BLAKE2S)
		(HASH-FUNCTION-SIZE 32) (HASH-FUNCTION-TIME 9.1216e-8)
		(HASH-FUNCTION-CONSTRAINTS 10324)))
	(KDF-HASH-FUNCTION.HASH-FUNCTIONS%
	 (RELATION
	  (HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-NAME
			      CIRCUIT-TIME HASH-FUNCTION-SIZE)
	  (2.6156e-5 1324 :PEDERSEN 0.153988 32)
	  (4.5608e-8 5162 :BLAKE2S-KDF 0.65253 32)
	  (9.1216e-8 10324 :BLAKE2S 1.30506 32)))
	(MERKLE-HASH-FUNCTION.CIRCUIT-TIME 0.153988)
	(SINGLE-CHALLENGE-INCLUSION-PROOFS 15)
	(ALPHA-HASH-FUNCTION.HASH-FUNCTION%
	 (TUPLE (CIRCUIT-TIME 0.153988) (HASH-FUNCTION-NAME :PEDERSEN)
		(HASH-FUNCTION-SIZE 32) (HASH-FUNCTION-TIME 2.6156e-5)
		(HASH-FUNCTION-CONSTRAINTS 1324)))
	(BETA-HASH-FUNCTION.HASH-FUNCTIONS%
	 (RELATION
	  (HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-NAME
			      CIRCUIT-TIME HASH-FUNCTION-SIZE)
	  (2.6156e-5 1324 :PEDERSEN 0.153988 32)
	  (4.5608e-8 5162 :BLAKE2S-KDF 0.65253 32)
	  (9.1216e-8 10324 :BLAKE2S 1.30506 32)))
	(LAYER-PERFORMANCE.LAYER-CHALLENGES 2672)
	(SINGLE-SLOTH-ITERATION-CONSTRAINTS 321)
	(ALPHA-HASH-FUNCTION.HASH-FUNCTIONS%
	 (RELATION
	  (HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-NAME
			      CIRCUIT-TIME HASH-FUNCTION-SIZE)
	  (2.6156e-5 1324 :PEDERSEN 0.153988 32)
	  (4.5608e-8 5162 :BLAKE2S-KDF 0.65253 32)
	  (9.1216e-8 10324 :BLAKE2S 1.30506 32)))
	(CIRCUIT-PROVING-TIME-PER-CONSTRAINT 1.4010074e-5)
	(LAYER-PERFORMANCE.BETA-HASHING-TIME 97.94244)
	(MERKLE-HASH-FUNCTION.HASH-FUNCTION%
	 (TUPLE (CIRCUIT-TIME 0.153988) (HASH-FUNCTION-NAME :PEDERSEN)
		(HASH-FUNCTION-SIZE 32) (HASH-FUNCTION-TIME 2.6156e-5)
		(HASH-FUNCTION-CONSTRAINTS 1324)))
	(MINIMUM-STORAGE-TO-PROOF-SIZE-RATIO 1073741824/25)
	(ZIGZAG-BASIC-LAYER-CHALLENGE-FACTOR 333.33334)
	(LAYER-PERFORMANCE.ALPHA-HASHING-TIME 0.0)
	(LAYER-PERFORMANCE.MERKLE-TREE.HEIGHT 30)
	(LAYER-PERFORMANCE.MERKLE-TREE.LEAVES 1073741824)
	(MERKLE-HASH-FUNCTION.HASH-FUNCTIONS%
	 (RELATION
	  (HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-NAME
			      CIRCUIT-TIME HASH-FUNCTION-SIZE)
	  (2.6156e-5 1324 :PEDERSEN 0.153988 32)
	  (4.5608e-8 5162 :BLAKE2S-KDF 0.65253 32)
	  (9.1216e-8 10324 :BLAKE2S 1.30506 32)))
	(SINGLE-CHALLENGE-SLOTH-VERIFICATIONS 0)
	(TOTAL-ZIGZAG-KDF-HASHING-CONSTRAINTS 193100096)
	(TOTAL-ZIGZAG-NON-HASHING-CONSTRAINTS 857712)
	(AVERAGE-MONTHLY-INCOME-DURING-RAMP-UP 2083.3333)
	(FILECOIN-POREP-REQUIREMENTS-SATISFIED T)
	(LAYER-PERFORMANCE.CHALLENGE-CONSTRAINTS 4718068)
	(LAYER-PERFORMANCE.CHALLENGE-CIRCUIT-TIME 596.4125)
	(LAYER-PERFORMANCE.LAYER-CHALLENGE-PROOFS 15)
	(LAYER-PERFORMANCE.MAX-BETA-MERKLE-HEIGHT 30)
	(LAYER-PERFORMANCE.MERKLE-TREE.HEIGHT-RAW 30.0)
	(LAYER-PERFORMANCE.MERKLE-TREE.NODE-BYTES 32)
	(LAYER-PERFORMANCE.BETA-HASH-FUNCTION.TIME 9.1216e-8)
	(LAYER-PERFORMANCE.MERKLE-TREE.BETA-HEIGHT 30)
	(LAYER-PERFORMANCE.MERKLE-TREE.SECTOR-SIZE 34359738368)
	(LAYER-PERFORMANCE.ALPHA-HASH-FUNCTION.TIME 2.6156e-5)
	(LAYER-PERFORMANCE.MERKLE-TREE.ALPHA-HEIGHT 0)
	(LAYER-PERFORMANCE.MERKLE-TREE.ALPHA-LEAVES 1)
	(LAYER-PERFORMANCE.CHALLENGE-KDF-CONSTRAINTS 72268)
	(LAYER-PERFORMANCE.CHALLENGE-KDF-CIRCUIT-TIME 9.13542)
	(LAYER-PERFORMANCE.INCLUSION-PROOF-CONSTRAINTS 309720)
	(LAYER-PERFORMANCE.MERKLE-TREE.BETA-HASH-COUNT 1073741823)
	(LAYER-PERFORMANCE.SINGLE-CHALLENGE-KDF-HASHES 14)
	(FILECOIN-POREP-SECURITY-REQUIREMENTS-SATISFIED T)
	(LAYER-PERFORMANCE.INCLUSION-PROOF-CIRCUIT-TIME 39.151802)
	(LAYER-PERFORMANCE.MERKLE-TREE.ALPHA-HASH-COUNT 0)
	(LAYER-PERFORMANCE.MERKLE-TREE.TOTAL-HASH-COUNT 1073741823)
	(LAYER-PERFORMANCE.KDF-HASH-FUNCTION.CONSTRAINTS 5162)
	(LAYER-PERFORMANCE.BETA-HASH-FUNCTION.CONSTRAINTS 10324)
	(LAYER-PERFORMANCE.KDF-HASH-FUNCTION.CIRCUIT-TIME 0.65253)
	(LAYER-PERFORMANCE.ALPHA-HASH-FUNCTION.CONSTRAINTS 1324)
	(LAYER-PERFORMANCE.BETA-HASH-FUNCTION.CIRCUIT-TIME 1.30506)
	(LAYER-PERFORMANCE.CHALLENGE-INCLUSION-CONSTRAINTS 4645800)
	(LAYER-PERFORMANCE.ALPHA-HASH-FUNCTION.CIRCUIT-TIME 0.153988)
	(LAYER-PERFORMANCE.BETA-INCLUSION-PROOF-CONSTRAINTS 309720)
	(LAYER-PERFORMANCE.CHALLENGE-INCLUSION-CIRCUIT-TIME 587.27704)
	(LAYER-PERFORMANCE.ALPHA-INCLUSION-PROOF-CONSTRAINTS 0)
	(LAYER-PERFORMANCE.BETA-INCLUSION-PROOF-CIRCUIT-TIME 39.151802)
	(LAYER-PERFORMANCE.SINGLE-CHALLENGE-INCLUSION-PROOFS 15)
	(LAYER-PERFORMANCE.ALPHA-INCLUSION-PROOF-CIRCUIT-TIME 0.0)
	(LAYER-PERFORMANCE.CIRCUIT-PROVING-TIME-PER-CONSTRAINT 1.4010074e-5)
	(LAYER-PERFORMANCE.MERKLE-TREE.BETA-INCLUSION-PROOF-HASH-LENGTH 30)
	(LAYER-PERFORMANCE.MERKLE-TREE.ALPHA-INCLUSION-PROOF-HASH-LENGTH 0)
	(LAYER-PERFORMANCE.MERKLE-TREE.TOTAL-INCLUSION-PROOF-HASH-LENGTH 30))))
    (representation (publish-filecoin)))))
