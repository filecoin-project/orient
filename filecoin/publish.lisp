(in-package :filecoin)
(in-suite filecoin-suite)

(defun publish-schema (system)
  (sort (loop for schema in (all-system-schemas system)
	   append (loop for parameter in (schema-parameters schema)
		     collect (tuple (name (string-downcase (symbol-name (parameter-name parameter))))
				    (description (parameter-description parameter)))))
	#'string< :key (lambda (tuple) (tref 'name tuple))))

(defun publish (system)
  (let ((assignments (solve-for system '() nil)))
    (tuple (schema (publish-schema system))
	   (assignments assignments))))

(defun publish-filecoin ()
  (let ((published (publish (filecoin-system))))
    (with-attributes (assignments) published
      (setf assignments (extract assignments :error t)))
    published))

(defun publish-filecoin-json (&optional (where (filecoin-json-path)))
  (typecase where
    ((or string pathname)
     (let* ((pathname (project-merge where))
	    (tmp-pathname (merge-pathnames (make-pathname :type "tmp") pathname)))
       (ensure-directories-exist pathname)
       ;; TODO: create or use  WITH-TMP macro.
       (unwind-protect
	    (progn
	      (with-open-file (out tmp-pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
		(let ((json:*lisp-identifier-name-to-json* #'string-downcase))
		  (json:encode-json (publish-filecoin) out)))
	      (uiop:run-program (format nil "cat ~a | jq > ~a" tmp-pathname pathname)))
	 (delete-file tmp-pathname))
       pathname))
    (null (with-output-to-string (out) (json:encode-json (publish-filecoin) out)))
    (t (json:encode-json (publish-filecoin) where))))

;; TODO: This produces an error when testing in Docker on CircleCI.
;; But we don't want to just swallow errors in general, because we need to know this has run successfully when run natively.
;; The goal is to ensure tests are published at every commit, but a pre-commit hook would be heavy.
(test force-publish
  (when (not (member :docker *features*))
    (eval-when (:load-toplevel :execute)
      (publish-filecoin-json))))

(test published-representation
  (let ((*use-parallel-apply-transformation* nil))
    (is
     (same
      '(TUPLE
        (ASSIGNMENTS
         (TUPLE (ALPHA-HASH-FUNCTION-NAME :PEDERSEN)
          (ALPHA-HASH-FUNCTION.CIRCUIT-TIME 0.093016036)
          (ALPHA-HASH-FUNCTION.CONSTRAINTS 1376)
          (ALPHA-HASH-FUNCTION.HASH-FUNCTION%
           (TUPLE (CIRCUIT-TIME 0.093016036) (HASH-FUNCTION-CONSTRAINTS 1376)
                  (HASH-FUNCTION-NAME :PEDERSEN) (HASH-FUNCTION-SIZE 32)
                  (HASH-FUNCTION-TIME 1.173068e-5)))
          (ALPHA-HASH-FUNCTION.HASH-FUNCTIONS%
           (RELATION
            (HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-NAME
                                CIRCUIT-TIME HASH-FUNCTION-SIZE)
            (1.173068e-5 1376 :PEDERSEN 0.093016036 32)
            (5.2520004e-8 10759 :BLAKE2S-KDF 0.16443467 32)
            (5.2468e-8 21518 :BLAKE2S 0.32886934 32)))
          (ALPHA-HASH-FUNCTION.NAME% :PEDERSEN) (ALPHA-HASH-FUNCTION.SIZE 32)
          (ALPHA-HASH-FUNCTION.TIME 1.173068e-5)
          (ALPHA-MERKLE-HASH-FUNCTION-NAME :PEDERSEN)
          (ALTERNATE-LAYERS
           (RELATION (PROVING-TIME BETA-MERKLE-HEIGHT) (130589.375 0)
                     (133772.69 1) (140090.84 2) (147976.42 3) (156645.7 4)
                     (165706.84 5) (174963.9 6) (184318.95 7) (193722.97 8)
                     (203151.48 9) (212592.22 10) (222039.13 11) (231489.08 12)
                     (240940.53 13) (250392.77 14) (259845.38 15) (269298.22 16)
                     (278751.1 17) (288204.03 18) (297657.03 19) (307110.03 20)
                     (316563.0 21) (326016.0 22) (335469.0 23) (344922.0 24)
                     (354374.97 25) (363828.0 26) (373281.0 27) (382734.0 28)
                     (392186.97 29) (401640.0 30)))
          (ANNUAL-INCOME 50000.0)
          (AVERAGE-MONTHLY-INCOME-DURING-RAMP-UP 2083.3333)
          (AWS-GLACIER-PRICE 0.004) (BASE-DEGREE 5)
          (BETA-HASH-FUNCTION-NAME :BLAKE2S)
          (BETA-HASH-FUNCTION.CIRCUIT-TIME 0.32886934)
          (BETA-HASH-FUNCTION.CONSTRAINTS 21518)
          (BETA-HASH-FUNCTION.HASH-FUNCTION%
           (TUPLE (CIRCUIT-TIME 0.32886934) (HASH-FUNCTION-CONSTRAINTS 21518)
                  (HASH-FUNCTION-NAME :BLAKE2S) (HASH-FUNCTION-SIZE 32)
                  (HASH-FUNCTION-TIME 5.2468e-8)))
          (BETA-HASH-FUNCTION.HASH-FUNCTIONS%
           (RELATION
            (HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-NAME
                                CIRCUIT-TIME HASH-FUNCTION-SIZE)
            (1.173068e-5 1376 :PEDERSEN 0.093016036 32)
            (5.2520004e-8 10759 :BLAKE2S-KDF 0.16443467 32)
            (5.2468e-8 21518 :BLAKE2S 0.32886934 32)))
          (BETA-HASH-FUNCTION.NAME% :BLAKE2S) (BETA-HASH-FUNCTION.SIZE 32)
          (BETA-HASH-FUNCTION.TIME 5.2468e-8) (COMM-D-SIZE 32) (COMM-R-SIZE 32)
          (COMM-R-STAR-SIZE 32) (COMM-RS-SIZE 64) (COMMITMENTS-SIZE 96)
          (COMMODITY-STORAGE-DISCOUNT 10) (COMPARABLE-MONTHLY-COST 416.66666)
          (CPU-GHZ-COST 10.0) (DAILY-TIB 1.1147972) (DEGREE 13)
          (EXPANSION-DEGREE 8) (FGR-MONTHS 4.3792353)
          (FGR-MONTHS-AT-CAPACITY 1.379235) (FILECOIN-FGR-REQUIREMENT T)
          (FILECOIN-FGR-REQUIREMENT-1 T)
          (FILECOIN-POREP-REQUIREMENTS-SATISFIED T)
          (FILECOIN-POREP-SECURITY-REQUIREMENTS-SATISFIED T)
          (FILECOIN-REQUIREMENTS-SATISFIED T)
          (FILECOIN-STORAGE-RATIO-SATISFIED T) (GIB-CAPACITY 104166.66)
          (GIB-HOUR-SEAL-INVESTMENT 188.06085) (GIB-SEAL-COST 0.08587253)
          (GIB-SEAL-CYCLES 6.7701905e13) (GIB-SEAL-TIME 13540.381)
          (HASH-FUNCTIONS
           (RELATION
            (HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-NAME
                                CIRCUIT-TIME HASH-FUNCTION-SIZE)
            (1.173068e-5 1376 :PEDERSEN 0.093016036 32)
            (5.2520004e-8 10759 :BLAKE2S-KDF 0.16443467 32)
            (5.2468e-8 21518 :BLAKE2S 0.32886934 32)))
          (HOURLY-GIB 47.564682) (HOURLY-TIB 0.046449885)
          (INCOME-DURING-RAMP-UP 6250.0) (INCOME-TO-FGR-AT-CAPACITY 5746.8125)
          (KDF-HASH-FUNCTION-NAME :BLAKE2S-KDF)
          (KDF-HASH-FUNCTION.CIRCUIT-TIME 0.16443467)
          (KDF-HASH-FUNCTION.CONSTRAINTS 10759)
          (KDF-HASH-FUNCTION.HASH-FUNCTION%
           (TUPLE (CIRCUIT-TIME 0.16443467) (HASH-FUNCTION-CONSTRAINTS 10759)
                  (HASH-FUNCTION-NAME :BLAKE2S-KDF) (HASH-FUNCTION-SIZE 32)
                  (HASH-FUNCTION-TIME 5.2520004e-8)))
          (KDF-HASH-FUNCTION.HASH-FUNCTIONS%
           (RELATION
            (HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-NAME
                                CIRCUIT-TIME HASH-FUNCTION-SIZE)
            (1.173068e-5 1376 :PEDERSEN 0.093016036 32)
            (5.2520004e-8 10759 :BLAKE2S-KDF 0.16443467 32)
            (5.2468e-8 21518 :BLAKE2S 0.32886934 32)))
          (KDF-HASH-FUNCTION.NAME% :BLAKE2S-KDF) (KDF-HASH-FUNCTION.SIZE 32)
          (KDF-HASH-FUNCTION.TIME 5.2520004e-8)
          (LAYER-PERFORMANCE.ALPHA-HASH-FUNCTION.CIRCUIT-TIME 0.093016036)
          (LAYER-PERFORMANCE.ALPHA-HASH-FUNCTION.CONSTRAINTS 1376)
          (LAYER-PERFORMANCE.ALPHA-HASH-FUNCTION.TIME 1.173068e-5)
          (LAYER-PERFORMANCE.ALPHA-HASHING-TIME 0.0)
          (LAYER-PERFORMANCE.ALPHA-INCLUSION-PROOF-CIRCUIT-TIME 0.0)
          (LAYER-PERFORMANCE.ALPHA-INCLUSION-PROOF-CONSTRAINTS 0)
          (LAYER-PERFORMANCE.BETA-HASH-FUNCTION.CIRCUIT-TIME 0.32886934)
          (LAYER-PERFORMANCE.BETA-HASH-FUNCTION.CONSTRAINTS 21518)
          (LAYER-PERFORMANCE.BETA-HASH-FUNCTION.TIME 5.2468e-8)
          (LAYER-PERFORMANCE.BETA-HASHING-TIME 56.337086)
          (LAYER-PERFORMANCE.BETA-INCLUSION-PROOF-CIRCUIT-TIME 9.86608)
          (LAYER-PERFORMANCE.BETA-INCLUSION-PROOF-CONSTRAINTS 645540)
          (LAYER-PERFORMANCE.CHALLENGE-CIRCUIT-TIME 150.29329)
          (LAYER-PERFORMANCE.CHALLENGE-CONSTRAINTS 9833726)
          (LAYER-PERFORMANCE.CHALLENGE-INCLUSION-CIRCUIT-TIME 147.99121)
          (LAYER-PERFORMANCE.CHALLENGE-INCLUSION-CONSTRAINTS 9683100)
          (LAYER-PERFORMANCE.CHALLENGE-KDF-CIRCUIT-TIME 2.3020854)
          (LAYER-PERFORMANCE.CHALLENGE-KDF-CONSTRAINTS 150626)
          (LAYER-PERFORMANCE.CIRCUIT-TIME 401583.66)
          (LAYER-PERFORMANCE.INCLUSION-PROOF-CIRCUIT-TIME 9.86608)
          (LAYER-PERFORMANCE.INCLUSION-PROOF-CONSTRAINTS 645540)
          (LAYER-PERFORMANCE.KDF-HASH-FUNCTION.CIRCUIT-TIME 0.16443467)
          (LAYER-PERFORMANCE.KDF-HASH-FUNCTION.CONSTRAINTS 10759)
          (LAYER-PERFORMANCE.LAYER-CHALLENGE-PROOFS 15)
          (LAYER-PERFORMANCE.LAYER-CHALLENGES 2672)
          (LAYER-PERFORMANCE.LAYER-INDEX 10) (LAYER-PERFORMANCE.LAYERS 10)
          (LAYER-PERFORMANCE.MAX-BETA-MERKLE-HEIGHT 30)
          (LAYER-PERFORMANCE.MERKLE-TREE.ALPHA-HASH-COUNT 0)
          (LAYER-PERFORMANCE.MERKLE-TREE.ALPHA-HEIGHT 0)
          (LAYER-PERFORMANCE.MERKLE-TREE.ALPHA-INCLUSION-PROOF-HASH-LENGTH 0)
          (LAYER-PERFORMANCE.MERKLE-TREE.ALPHA-LEAVES 1)
          (LAYER-PERFORMANCE.MERKLE-TREE.BETA-HASH-COUNT 1073741823)
          (LAYER-PERFORMANCE.MERKLE-TREE.BETA-HEIGHT 30)
          (LAYER-PERFORMANCE.MERKLE-TREE.BETA-INCLUSION-PROOF-HASH-LENGTH 30)
          (LAYER-PERFORMANCE.MERKLE-TREE.HEIGHT 30)
          (LAYER-PERFORMANCE.MERKLE-TREE.HEIGHT-RAW 30.0)
          (LAYER-PERFORMANCE.MERKLE-TREE.LEAVES 1073741824)
          (LAYER-PERFORMANCE.MERKLE-TREE.NODE-BYTES 32)
          (LAYER-PERFORMANCE.MERKLE-TREE.SECTOR-SIZE 34359738368)
          (LAYER-PERFORMANCE.MERKLE-TREE.TOTAL-HASH-COUNT 1073741823)
          (LAYER-PERFORMANCE.MERKLE-TREE.TOTAL-INCLUSION-PROOF-HASH-LENGTH 30)
          (LAYER-PERFORMANCE.NODE-BYTES 32)
          (LAYER-PERFORMANCE.SECTOR-SIZE 34359738368)
          (LAYER-PERFORMANCE.SINGLE-CHALLENGE-INCLUSION-PROOFS 15)
          (LAYER-PERFORMANCE.SINGLE-CHALLENGE-KDF-HASHES 14)
          (LAYER-REPLICATION-TIME 789.501) (LAYERS 10)
          (MAX-BETA-MERKLE-HEIGHT 30) (MAXIMUM-ALLOWABLE-SPACE-GAP 0.02)
          (MERKLE-HASH-FUNCTION-NAME :PEDERSEN)
          (MERKLE-HASH-FUNCTION.CIRCUIT-TIME 0.093016036)
          (MERKLE-HASH-FUNCTION.CONSTRAINTS 1376)
          (MERKLE-HASH-FUNCTION.HASH-FUNCTION%
           (TUPLE (CIRCUIT-TIME 0.093016036) (HASH-FUNCTION-CONSTRAINTS 1376)
                  (HASH-FUNCTION-NAME :PEDERSEN) (HASH-FUNCTION-SIZE 32)
                  (HASH-FUNCTION-TIME 1.173068e-5)))
          (MERKLE-HASH-FUNCTION.HASH-FUNCTIONS%
           (RELATION
            (HASH-FUNCTION-TIME HASH-FUNCTION-CONSTRAINTS HASH-FUNCTION-NAME
                                CIRCUIT-TIME HASH-FUNCTION-SIZE)
            (1.173068e-5 1376 :PEDERSEN 0.093016036 32)
            (5.2520004e-8 10759 :BLAKE2S-KDF 0.16443467 32)
            (5.2468e-8 21518 :BLAKE2S 0.32886934 32)))
          (MERKLE-HASH-FUNCTION.NAME% :PEDERSEN) (MERKLE-HASH-FUNCTION.SIZE 32)
          (MERKLE-HASH-FUNCTION.TIME 1.173068e-5) (MINER-MONTHS-TO-CAPACITY 3)
          (MINIMUM-ONE-YEAR-FGR -2)
          (MINIMUM-STORAGE-TO-PROOF-SIZE-RATIO 1073741824/25)
          (MINIMUM-THREE-YEAR-FGR 1.0) (MINIMUM-TWO-YEAR-FGR 0.25)
          (MONTHLY-INCOME 4166.6665) (MONTHLY-TIB 33.908417)
          (MUST-HAVE-FILECOIN T) (NEEDED-GHZ 894.50543) (NODE-BYTES 32)
          (NODES 1073741824) (ON-CHAIN-POREP-SIZE 288) (ONE-YEAR-FGR 2.646802)
          (ONE-YEAR-FGR-SATISFIED T) (ONE-YEAR-PROFIT 31753.186)
          (ONE-YEAR-PROFIT-MONTHS 7.6207647)
          (OPTIMAL-HEIGHTS
           (RELATION
            (CIRCUIT-TIME HASHING-TIME LAYER-INDEX CONSTRAINTS LOWEST-TIME
                          OPTIMAL-BETA-MERKLE-HEIGHT)
            (0.0 56.337086 0 0 56.337086 30)
            (9329.68 840.04865 1 316535360 10169.729 4)
            (9329.68 840.04865 2 316535360 10169.729 4)
            (9329.68 840.04865 3 316535360 10169.729 4)
            (13145.448 1623.7601 4 402291840 14769.208 3)
            (19279.99 1623.7601 5 590028032 20903.75 3)
            (27052.027 3191.1833 6 725517408 30243.21 2)
            (40578.043 3191.1833 7 1088276112 43769.227 2)
            (56854.945 6326.0293 8 1277771552 63180.977 1)
            (78780.195 12595.722 9 1373369584 91375.914 0)
            (117993.66 12595.722 10 2056975072 130589.375 0)))
          (PARTITION-CHALLENGES 2672) (PARTITIONS 1) (PROFIT T)
          (REPLICATION-TIME 7895.01) (REPLICATION-TIME-PER-BYTE 2.2977503e-7)
          (REPLICATION-TIME-PER-GIB 246.71906)
          (SEAL-CYCLES-PER-HOUR 3.2202196e15)
          (SEAL-CYCLES-PER-MINUTE 5.3670327e13)
          (SEAL-CYCLES-PER-SECOND 8.9450545e11) (SEAL-GHZ 5.0) (SEAL-HZ 5.0e9)
          (SEAL-PARALLELISM 14) (SEAL-TIME 433292.2) (SECTOR-GIB 32)
          (SECTOR-SIZE 34359738368) (SINGLE-CHALLENGE-INCLUSION-PROOFS 15)
          (SINGLE-CHALLENGE-KDF-HASHES 14) (SINGLE-CIRCUIT-PROOF-SIZE 192)
          (SINGLE-KDF-HASHES 14) (SINGLE-KDF-TIME 7.352801e-7)
          (SINGLE-NODE-ADD-ENCODING-TIME 0)
          (SINGLE-NODE-ENCODING-TIME 7.352801e-7) (SPACE-GAP-SATISFIED T)
          (STORAGE-TO-PROOF-SIZE-FLOAT 1.1930465e8)
          (STORAGE-TO-PROOF-SIZE-RATIO 1073741824/9) (THREE-YEAR-FGR 10.982349)
          (THREE-YEAR-FGR-SATISFIED T) (THREE-YEAR-PROFIT 131753.19)
          (THREE-YEAR-PROFIT-MONTHS 31.620766) (TIB-CAPACITY 101.72525)
          (TIB-DRIVE-COST 30.0) (TOTAL-CHALLENGES 2672)
          (TOTAL-CIRCUIT-PROOF-SIZE 192) (TOTAL-CIRCUIT-TIME 381673.34)
          (TOTAL-HASHING-TIME 43723.844) (TOTAL-NODES-TO-ENCODE 10737418240)
          (TOTAL-PARENTS 13) (TOTAL-PROVING-TIME 425397.2)
          (TOTAL-UNTAPERED-CHALLENGES 32000.0) (TOTAL-UP-FRONT-COST 11996.8125)
          (TOTAL-ZIGZAG-CHALLENGES 2672) (TOTAL-ZIGZAG-CIRCUIT-KDF-HASHES 37408)
          (TOTAL-ZIGZAG-CONSTRAINTS 8463835680)
          (TOTAL-ZIGZAG-KDF-HASHING-CONSTRAINTS 402472672)
          (TOTAL-ZIGZAG-NON-HASHING-CONSTRAINTS 0)
          (TOTAL-ZIGZAG-OTHER-CONSTRAINTS 0) (TWO-YEAR-FGR 6.8145757)
          (TWO-YEAR-FGR-SATISFIED T) (TWO-YEAR-PROFIT 81753.19)
          (TWO-YEAR-PROFIT-MONTHS 19.620766) (UP-FRONT-COMPUTE-COST 8945.055)
          (UP-FRONT-DRIVE-COST 3051.7576) (UP-FRONT-MEMORY-COST 0.0)
          (UP-FRONT-SEALING-COST 8945.055) (WALL-CLOCK-SEAL-TIME 30385.514)
          (WALL-CLOCK-SEAL-TIME-PER-BYTE 8.8433484e-7)
          (WALL-CLOCK-SEAL-TIME-PER-GIB 949.5473)
          (ZIGZAG-BASIC-LAYER-CHALLENGE-FACTOR 333.33334)
          (ZIGZAG-BASIC-LAYER-CHALLENGES 2666.6667) (ZIGZAG-DELTA 0.003)
          (ZIGZAG-EPSILON 0.007) (ZIGZAG-LAMBDA 8) (ZIGZAG-LAYERS 12)
          (ZIGZAG-SOUNDNESS 0.00390625) (ZIGZAG-SPACE-GAP 0.01)
          (ZIGZAG-TAPER 1/3)))
        (SCHEMA
         ((TUPLE (DESCRIPTION "Time for one *.HASH-FUNCTION in circuit.")
                 (NAME "alpha-hash-function.circuit-time"))
          (TUPLE
           (DESCRIPTION
            "Number of constraints required to prove *.HASH-FUNCTION in circuit.")
           (NAME "alpha-hash-function.constraints"))
          (TUPLE
           (DESCRIPTION
            "Tuple containing the selected hash function's characteristics.")
           (NAME "alpha-hash-function.hash-function%"))
          (TUPLE (DESCRIPTION "Size of digest (output) generated by *.HASH-FUNCTION.")
                 (NAME "alpha-hash-function.size"))
          (TUPLE
           (DESCRIPTION
            "Annual income from selling storage on the storage market. Unit: dollars")
           (NAME "annual-income"))
          (TUPLE
           (DESCRIPTION
            "TiB of storage which must be brought online per year. Unit: TiB")
           (NAME "annual-tib"))
          (TUPLE
           (DESCRIPTION
            "Average monthly income before miner reaches capacity (assuming linear growth). Unit: dollars")
           (NAME "average-monthly-income-during-ramp-up"))
          (TUPLE
           (DESCRIPTION
            "Cost of one GiB storage from AWS glacier for one month. Unit: dollars")
           (NAME "aws-glacier-price"))
          (TUPLE (DESCRIPTION "In-degree of the base depth-robust graph (DRG).")
                 (NAME "base-degree"))
          (TUPLE (DESCRIPTION "Time for one *.HASH-FUNCTION in circuit.")
                 (NAME "beta-hash-function.circuit-time"))
          (TUPLE
           (DESCRIPTION
            "Number of constraints required to prove *.HASH-FUNCTION in circuit.")
           (NAME "beta-hash-function.constraints"))
          (TUPLE
           (DESCRIPTION
            "Tuple containing the selected hash function's characteristics.")
           (NAME "beta-hash-function.hash-function%"))
          (TUPLE (DESCRIPTION "Size of digest (output) generated by *.HASH-FUNCTION.")
                 (NAME "beta-hash-function.size"))
          (TUPLE (DESCRIPTION "Size of the data commitment (CommD). Unit: bytes")
                 (NAME "comm-d-size"))
          (TUPLE (DESCRIPTION "Size of the replica commitment (CommR). Unit: bytes")
                 (NAME "comm-r-size"))
          (TUPLE
           (DESCRIPTION
            "Size of the aggregated commitment to each layer's replica (CommR*). Unit: bytes")
           (NAME "comm-r-star-size"))
          (TUPLE (DESCRIPTION "Size of all replica commitments. Unit: bytes")
                 (NAME "comm-rs-size"))
          (TUPLE (DESCRIPTION "Size of all commitments returned by Seal. Unit: bytes")
                 (NAME "commitments-size"))
          (TUPLE
           (DESCRIPTION
            "Fraction of commodity storage pricing expected as income from storage market. Unit: decimal fraction")
           (NAME "commodity-storage-discount"))
          (TUPLE
           (DESCRIPTION
            "Expected cost of purchasing monthly storage from commodity provider. Unit: dollars")
           (NAME "comparable-monthly-cost"))
          (TUPLE
           (DESCRIPTION
            "TiB of storage which must be brought online per day. Unit: TiB")
           (NAME "daily-tib"))
          (TUPLE (DESCRIPTION "Total in-degree of the ZigZag graph.") (NAME "degree"))
          (TUPLE
           (DESCRIPTION
            "Maximum in-degree of the bipartite expander graph component of a ZigZag graph.")
           (NAME "expansion-degree"))
          (TUPLE
           (DESCRIPTION
            "Months after which a miner should reach filecoin growth rate equilibrium.")
           (NAME "fgr-months"))
          (TUPLE
           (DESCRIPTION
            "Months needed after reaching capacity before filecoin growth rate equilibrium.")
           (NAME "fgr-months-at-capacity"))
          (TUPLE (DESCRIPTION "Are the Filecoin requirements all satisfied?")
                 (NAME "filecoin-requirements-satisfied"))
          (TUPLE (DESCRIPTION "GiB of storage at full capacity. Unit: GiB")
                 (NAME "gib-capacity"))
          (TUPLE
           (DESCRIPTION
            "Cost of investment required to seal one GiB in one hour at scale. Unit: dollars")
           (NAME "gib-hour-seal-investment"))
          (TUPLE (DESCRIPTION "Cost of sealing one GiB. Unit: dollars")
                 (NAME "gib-seal-cost"))
          (TUPLE (DESCRIPTION "Total CPU cycles required to seal 1 GiB. Unit: cycles")
                 (NAME "gib-seal-cycles"))
          (TUPLE
           (DESCRIPTION
            "Total time to seal (replication + proving) one GiB. Unit: seconds")
           (NAME "gib-seal-time"))
          (TUPLE
           (DESCRIPTION
            "Alternate hash functions for use in merkle-tree generation, KDF, or other commitments. All values here assume 64 bytes of raw input, but benchmarks include any extra, 'personalization', bits/bytes included as input during merkle-tree construction.")
           (NAME "hash-functions"))
          (TUPLE
           (DESCRIPTION
            "GiB of storage which must be brought online per hour. Unit: GiB")
           (NAME "hourly-gib"))
          (TUPLE
           (DESCRIPTION
            "TiB of storage which must be brought online per hour. Unit: TiB")
           (NAME "hourly-tib"))
          (TUPLE
           (DESCRIPTION
            "Total income during ramp-up period (before reaching capacity). Unit: dollars")
           (NAME "income-during-ramp-up"))
          (TUPLE
           (DESCRIPTION
            "Income still required to reach filecoin growth rate equilibrium after reaching capacity. Unit: dollars")
           (NAME "income-to-fgr-at-capacity"))
          (TUPLE (DESCRIPTION "Time for one *.HASH-FUNCTION in circuit.")
                 (NAME "kdf-hash-function.circuit-time"))
          (TUPLE
           (DESCRIPTION
            "Number of constraints required to prove *.HASH-FUNCTION in circuit.")
           (NAME "kdf-hash-function.constraints"))
          (TUPLE
           (DESCRIPTION
            "Tuple containing the selected hash function's characteristics.")
           (NAME "kdf-hash-function.hash-function%"))
          (TUPLE (DESCRIPTION "Size of digest (output) generated by *.HASH-FUNCTION.")
                 (NAME "kdf-hash-function.size"))
          (TUPLE
           (DESCRIPTION
            "Number of hashes performed as part of the key-derivation function (KDF).")
           (NAME "kdf-hashes"))
          (TUPLE (DESCRIPTION "Index of layer. Unit: integer") (NAME "layer-index"))
          (TUPLE (DESCRIPTION "Time to replicate one layer. Unit: seconds")
                 (NAME "layer-replication-time"))
          (TUPLE
           (DESCRIPTION
            "Number of layers specified for this construction (not necessarily same as calculated from security parameters).")
           (NAME "layers"))
          (TUPLE (DESCRIPTION "Time for one *.HASH-FUNCTION in circuit.")
                 (NAME "merkle-hash-function.circuit-time"))
          (TUPLE
           (DESCRIPTION
            "Number of constraints required to prove *.HASH-FUNCTION in circuit.")
           (NAME "merkle-hash-function.constraints"))
          (TUPLE
           (DESCRIPTION
            "Tuple containing the selected hash function's characteristics.")
           (NAME "merkle-hash-function.hash-function%"))
          (TUPLE (DESCRIPTION "Size of digest (output) generated by *.HASH-FUNCTION.")
                 (NAME "merkle-hash-function.size"))
          (TUPLE
           (DESCRIPTION
            "Months it should take a miner to reach full storage capacity. Unit: months")
           (NAME "miner-months-to-capacity"))
          (TUPLE (DESCRIPTION "Minimum allowable FGR after one year.")
                 (NAME "minimum-one-year-fgr"))
          (TUPLE (DESCRIPTION "Minimum necessary storage to on-chain proof ratio.")
                 (NAME "minimum-storage-to-proof-size-ratio"))
          (TUPLE (DESCRIPTION "Minimum allowable FGR after three years.")
                 (NAME "minimum-three-year-fgr"))
          (TUPLE (DESCRIPTION "Minimum allowable FGR after two years.")
                 (NAME "minimum-two-year-fgr"))
          (TUPLE
           (DESCRIPTION
            "Monthly income from selling storage on the storage market. Unit: dollars")
           (NAME "monthly-income"))
          (TUPLE
           (DESCRIPTION
            "TiB of storage which must be brought online per month. Unit: TiB")
           (NAME "monthly-tib"))
          (TUPLE (DESCRIPTION "WE MUST HAVE FILECOIN -- SWEET, SWEET FILECOIN.")
                 (NAME "must-have-filecoin"))
          (TUPLE
           (DESCRIPTION "Total GHz capacity needed to seal at the required rate.")
           (NAME "needed-ghz"))
          (TUPLE
           (DESCRIPTION
            "The number of bytes in a node -- must also be the hash digest size.")
           (NAME "node-bytes"))
          (TUPLE
           (DESCRIPTION
            "Time (including replication) to generate a non-circuit proof of replication. Unit: seconds")
           (NAME "non-circuit-proving-time"))
          (TUPLE
           (DESCRIPTION
            "On-chain size of one Seal proof plus commitments. Unit: bytes")
           (NAME "on-chain-porep-size"))
          (TUPLE (DESCRIPTION "FGR after one year of operation: Unit: fraction")
                 (NAME "one-year-fgr"))
          (TUPLE (DESCRIPTION "Profit after one year of operation: Unit: dollars")
                 (NAME "one-year-profit"))
          (TUPLE (DESCRIPTION "Months from FGR to one year of profit. Unit: months")
                 (NAME "one-year-profit-months"))
          (TUPLE (DESCRIPTION "") (NAME "partition-challenges"))
          (TUPLE
           (DESCRIPTION "Number of circuit partitions into which a proof is divided.")
           (NAME "partitions"))
          (TUPLE (DESCRIPTION "… Profit.") (NAME "profit"))
          (TUPLE (DESCRIPTION "Time to replicate one sector. Unit: seconds")
                 (NAME "replication-time"))
          (TUPLE (DESCRIPTION "Time to replicate one byte. Unit: seconds / byte")
                 (NAME "replication-time-per-byte"))
          (TUPLE (DESCRIPTION "Time to replicate one GiB. Unit: seconds / GiB")
                 (NAME "replication-time-per-gib"))
          (TUPLE
           (DESCRIPTION
            "CPU required to seal at required rate for one hour. Unit: cycles")
           (NAME "seal-cycles-per-hour"))
          (TUPLE
           (DESCRIPTION
            "CPU required to seal at required rate for one minute. Unit: cycles")
           (NAME "seal-cycles-per-minute"))
          (TUPLE
           (DESCRIPTION
            "CPU required to seal at required rate for one second. Unit: cycles")
           (NAME "seal-cycles-per-second"))
          (TUPLE
           (DESCRIPTION
            "Cycles per second at which the sealing machine operates. Unit: GHz")
           (NAME "seal-ghz"))
          (TUPLE
           (DESCRIPTION
            "Cycles per second at which the sealing machine operates. Unit: Hz")
           (NAME "seal-hz"))
          (TUPLE
           (DESCRIPTION
            "Number of cores utilized when computing wall-clock-seal-time.")
           (NAME "seal-parallelism"))
          (TUPLE
           (DESCRIPTION
            "Total time to seal (replication + proving) one sector. Unit: seconds")
           (NAME "seal-time"))
          (TUPLE
           (DESCRIPTION
            "Total CPU time to seal (replicate + generate proof of replication) one sector. Unit: seconds")
           (NAME "sealing-time"))
          (TUPLE (DESCRIPTION "Size of one sector. Unit: GiB") (NAME "sector-gib"))
          (TUPLE (DESCRIPTION "Number of GiB in one sector. Unit: GiB")
                 (NAME "sector-gib"))
          (TUPLE (DESCRIPTION "Size of one sector. Unit: bytes") (NAME "sector-size"))
          (TUPLE
           (DESCRIPTION
            "Number of inclusion proofs which must be verified for a single challenge.")
           (NAME "single-challenge-inclusion-proofs"))
          (TUPLE
           (DESCRIPTION
            "Number of KDF hashes which must be verified for a single challenge.")
           (NAME "single-challenge-kdf-hashes"))
          (TUPLE
           (DESCRIPTION
            "Number of merkle hashes which must be verified for a single challenge.")
           (NAME "single-challenge-merkle-hases"))
          (TUPLE (DESCRIPTION "Size of a single Groth16 Proof. Unit: bytes")
                 (NAME "single-circuit-proof-size"))
          (TUPLE
           (DESCRIPTION
            "Number of hashes performed as part of a single application of the key-derivation function (KDF). This is equal to the number of parents and includes the replica ID. Since we use Merkle-Damgard construction, the number of compression hashes is the number of elements - 1.")
           (NAME "single-kdf-hashes"))
          (TUPLE (DESCRIPTION "Hashing time to perform a single KDF. Unit: seconds")
                 (NAME "single-kdf-time"))
          (TUPLE (DESCRIPTION "Merkle hashing time for a single layer. Unit: seconds")
                 (NAME "single-layer-merkle-hashing-time"))
          (TUPLE (DESCRIPTION "Time to add-encode a single node. Unit: seconds")
                 (NAME "single-node-add-encoding-time"))
          (TUPLE (DESCRIPTION "Time to encode a single node. Unit: seconds")
                 (NAME "single-node-encoding-time"))
          (TUPLE
           (DESCRIPTION
            "Is the actual space gap less than or equal to the maximum allowable space gap?")
           (NAME "space-gap-satisfied"))
          (TUPLE
           (DESCRIPTION
            "Ratio of sealed sector size to on-chain PoRep size -- expressed as a float.")
           (NAME "storage-to-proof-size-float"))
          (TUPLE (DESCRIPTION "Ratio of sealed sector size to on-chain PoRep size.")
                 (NAME "storage-to-proof-size-ratio"))
          (TUPLE (DESCRIPTION "FGR after three years of operation: Unit: fraction")
                 (NAME "three-year-fgr"))
          (TUPLE (DESCRIPTION "Profit after three years of operation: Unit: dollars")
                 (NAME "three-year-profit"))
          (TUPLE
           (DESCRIPTION "Months from FGR to three years of profit. Unit: months")
           (NAME "three-year-profit-months"))
          (TUPLE (DESCRIPTION "TiB of storage at full capacity. Unit: TiB")
                 (NAME "tib-capacity"))
          (TUPLE (DESCRIPTION "") (NAME "total-challenges"))
          (TUPLE (DESCRIPTION "Total size of a single circuit proof. Unit: bytes")
                 (NAME "total-circuit-proof-size"))
          (TUPLE
           (DESCRIPTION
            "Total number of KDF (key-derivation function) required during replication.")
           (NAME "total-kdf-hashes"))
          (TUPLE
           (DESCRIPTION "Total time to generate all merkle trees. Unit: seconds")
           (NAME "total-merkle-hashing-time"))
          (TUPLE (DESCRIPTION "Total merkle trees which must be generated.")
                 (NAME "total-merkle-trees"))
          (TUPLE (DESCRIPTION "Total nodes to encode across all layers.")
                 (NAME "total-nodes-to-encode"))
          (TUPLE
           (DESCRIPTION
            "Number of parents (or padding) each node uses when performing key derivation.")
           (NAME "total-parents"))
          (TUPLE
           (DESCRIPTION
            "Total up-front investment required to generate MONTHLY-INCOME. Unit: dollars")
           (NAME "total-up-front-cost"))
          (TUPLE
           (DESCRIPTION
            "Total number of KDF hashes which must be verified in a ZigZag circuit.")
           (NAME "total-zigzag-circuit-kdf-hashes"))
          (TUPLE
           (DESCRIPTION
            "Total number of constraints which must be verified in a ZigZag circuit.")
           (NAME "total-zigzag-constraints"))
          (TUPLE
           (DESCRIPTION
            "Total number of kdf hashing constraints in a ZigZag circuit.")
           (NAME "total-zigzag-kdf-hashing-constraints"))
          (TUPLE
           (DESCRIPTION
            "Total number of hashes which must be verified in a ZigZag circuit.")
           (NAME "total-zigzag-non-hashing-constraints"))
          (TUPLE (DESCRIPTION "FGR after two years of operation: Unit: fraction")
                 (NAME "two-year-fgr"))
          (TUPLE (DESCRIPTION "Profit after two years of operation: Unit: dollars")
                 (NAME "two-year-profit"))
          (TUPLE (DESCRIPTION "Months from FGR to two years of profit. Unit: months")
                 (NAME "two-year-profit-months"))
          (TUPLE
           (DESCRIPTION
            "Up-front investement in compute hardware required to seal at necessary rate. Unit: dollars")
           (NAME "up-front-compute-cost"))
          (TUPLE
           (DESCRIPTION
            "Up-front investment in hard drives required to store sufficient sealed data. Unit: dollars.")
           (NAME "up-front-drive-cost"))
          (TUPLE
           (DESCRIPTION
            "Up-front investment in memory (RAM) required to seal at necessary rate. Unit: dollars")
           (NAME "up-front-memory-cost"))
          (TUPLE
           (DESCRIPTION
            "Up-front investment in total hardware require to seal at necessary rate. Unit: dollars")
           (NAME "up-front-sealing-cost"))
          (TUPLE
           (DESCRIPTION
            "Time to generate the vector commitments used in a non-circuit proof of replication. Unit: seconds")
           (NAME "vector-commitment-time"))
          (TUPLE
           (DESCRIPTION
            "Wall clock time sealing time using SEAL-PARALLELISM cores. Unit: seconds")
           (NAME "wall-clock-seal-time"))
          (TUPLE
           (DESCRIPTION
            "Number of challenges which, when multiplied by lambda, yields the number of challenges per layer without tapering optimization.")
           (NAME "zigzag-basic-layer-challenge-factor"))
          (TUPLE
           (DESCRIPTION
            "Multiple of lambda challenges per layer, without tapering optimization.")
           (NAME "zigzag-basic-layer-challenges"))
          (TUPLE
           (DESCRIPTION "Maximum allowable cheating on labels (block corruption)")
           (NAME "zigzag-delta"))
          (TUPLE
           (DESCRIPTION
            "Maximum allowable deletion (space tightness): Unit: fraction")
           (NAME "zigzag-epsilon"))
          (TUPLE (DESCRIPTION "ZigZag soundness: Unit bits") (NAME "zigzag-lambda"))
          (TUPLE
           (DESCRIPTION
            "Number of challenges in this (indexed) layer of ZigZag PoRep. Unit: integer")
           (NAME "zigzag-layer-challenges"))
          (TUPLE (DESCRIPTION "ZigZag soundness: Unit fraction")
                 (NAME "zigzag-soundness"))
          (TUPLE
           (DESCRIPTION
            "Maximum allowable gap between actual and claimed storage. Unit: fraction")
           (NAME "zigzag-space-gap"))
          (TUPLE
           (DESCRIPTION
            "Total time to generate a proof of replication (circuit and non-circuit). Unit: seconds")
           (NAME "zigzag-total-proving-time")))))    
      (representation (publish-filecoin))))))
