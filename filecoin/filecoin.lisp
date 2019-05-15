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
  )

(defparameter *defaults*
  (data-map
   (investment 100000)
   (comparable-monthly-income 50000)
   (seal-cost 10)

   (pedersen-hash-seconds 0.000017993)
   (pedersen-hash-constraints 1152)
   
   (blake-hash-seconds 1.6055e-7)
   (blake-hash-constraints 10324)

   (sector-size (* 1 GiB))
   )
  )

;; TODO: Add a function to check data against schema -- which will make more sense once schema is typed.

(deftransformation sector-merkle-trees
    ((sector-size) -> (sector-merkle-tree-leaves
		       sector-merkle-tree-height		       
		       ))
  (let* ((leaves (/ sector-size 32)) ;; FIXME: check power of two or round up.
	 (height (ceiling (+ (log leaves 2)) 1)))
    (values leaves
	    height
	  )))

(defparameter *system* (sys ((component (sector-merkle-trees)))))

(def-suite filecoin-suite)
(in-suite filecoin-suite)
(test defaults-test
  "Test and assert results of solving with defaults."
  (let ((result (solve-for *system* '(sector-merkle-tree-leaves sector-merkle-tree-height) *defaults*)))
    (is (same result (data-map (INVESTMENT 100000)
			       (COMPARABLE-MONTHLY-INCOME 50000)
			       (SEAL-COST 10)
			       (PEDERSEN-HASH-SECONDS 1.7993e-5)
			       (PEDERSEN-HASH-CONSTRAINTS 1152)
			       (BLAKE-HASH-SECONDS 1.6055e-7)
			       (BLAKE-HASH-CONSTRAINTS 10324)
			       (SECTOR-SIZE 1073741824)
			       (SECTOR-MERKLE-TREE-LEAVES 33554432)
			       (SECTOR-MERKLE-TREE-HEIGHT 25))))))
