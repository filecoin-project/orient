(defpackage :orient.base.util
  (:use :common-lisp)
  (:nicknames :util)
  (:export :comma-list :string-split :project-commit :project-sha1))

(defpackage :orient
  (:use :common-lisp :it.bese.FiveAm :fset :gmap :orient.base.util)
  (:shadow :join :restrict :relation :tuple)
  ;; Use same shadowing imports as FSET-USER does.
  (:shadowing-import-from :fset
			  ;; Shadowed type/constructor names
			  #:set #:map
			  ;; Shadowed set operations
			  #:union #:intersection #:set-difference #:complement
			  ;; Shadowed sequence operations
			  #:first #:last #:subseq #:reverse #:sort #:stable-sort
			  #:reduce
			  #:find #:find-if #:find-if-not
			  #:count #:count-if #:count-if-not
			  #:position #:position-if #:position-if-not
			  #:remove #:remove-if #:remove-if-not
			  #:substitute #:substitute-if #:substitute-if-not
			  #:some #:every #:notany #:notevery)
  (:export

   :all-system-schemas
   :dbg :display :aif :apply-transformation :ask :attributes :awhen
   :dbreak :*dval*
   :cardinality
   :component :component-operation :component-args :component-target :component-transformations
   :constraint-system
   :defconstraint-system :define-constraint :define-simple-constraint :define-system-constraint
   :display :ensure-tuples :expand-references
   ::tuple :tuples :create-tuple-report-step
   :describe-transformation-calculation :defschema
   :deftransformation :deftransformation= :extract
   :find-component :find-constraint :find-schema :find-system :find-transformation :format-value
   :forget :generate-directed-graph :it
   :implementation :implementation-module :implementation-name
   :tref :join :lookup-description :make-relation
   :make-signature :make-tuple :operation
   :orient-tests :optimal-heights
   :org-present
   :parameter :parameter-name :parameter-description :parameter-type
   :plan :plan-for :pipeline-signature :private-attr-p
   :present-data :project :project-commit-link
   :rel :relation :remove-attributes :rename :report-data :report-solution-for :representation
   :same :schema :schema-parameters :schema-description :sig :signature :signature-input :signature-output :solve :solve-for
   :synthesize-report-steps :symbolconc :sys :system
   :system-components :system-data :system-name :system-schema :system-subsystems
   :tpl :transformation :transformation*
   :transformation-implementation :transformation-name :transformation-signature :tref :trem :try-with
   :use-construction :use-attribute
   :where :with-construction :write-dot-format
   :*current-construction* :*trace-plan* :-> :=> :~> :=== :== &acc &all &group &group-by &into :!>))
