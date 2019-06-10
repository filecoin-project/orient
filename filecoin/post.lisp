(in-package :filecoin)
(in-suite filecoin-suite)

;; PoSt -- WIP

(defschema post-schema
    "PoSt Schema"
  (replication-attacker-discount "The fraction of the calculated replication cost which an attacker can realize.")

  (post-polling-time)
  (post-proving-period)
  (post-epochs)
  (post-hash-function)

  (attacker-replication-cost "Cost to replicate one sector. Unit: dollars")
  (minimum-replication-time)
  (expected-value-of-space-reuse "")

  (cost-of-retaining-data "How much does it cost to retain unsealed data.")
  (post-attack-rationality)
  (cost-to-delete-all-data-and-pass "Cost to delete all data and pass PoSt.")
  ;;; S - C 
  )

(defparameter *post-defaults*
  (tuple
   (post-proving-period (* 60 60 24 2)) ;; TODO: derive this.
   (replication-time 2000)))

;; (defconstraint-system post-constraint-system
;;     (post-polling-time (xxx replication-time replication-amax))
;;   (attacker-replication-cost (xxx replication-attacker-discount replication-cost))
  
  
;;   )

;; (defun post-system (&key isolated)
;;   (make-instance 'system
;; 		 :subsystems (list (find-system 'post-constraint-system))
;; 		 :data (list *post-defaults*)))

