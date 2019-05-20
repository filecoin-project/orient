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
   (investment 100000)
   (comparable-monthly-income 50000)
   (seal-cost 10)

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
	 (expected; (make-relation (list
				   (tuple (INVESTMENT 100000)
					       (COMPARABLE-MONTHLY-INCOME 50000)
					       (SEAL-COST 10)
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
    (setq asdf result)
    (setq fdsa expected)
    (is (same expected result))))

#|
(run! 'filecoin-suite)
|#
