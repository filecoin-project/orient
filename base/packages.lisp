(defpackage orient.base.util
  (:use :common-lisp)
  (:nicknames :util)
  (:export :comma-list
           :flatten :flatten1 :flatten2
	   :get-string
	   :resolve-json-input
	   :keywordize
	   :map-tree :mk-list
	   :partial
	   :project-commit :project-merge :project-sha1 :project-root :*project-root*
	   :string-split
	   :transform-tree :transform-tree-if
	   ))

(defpackage orient
  (:use :common-lisp :it.bese.FiveAm :fset :gmap :orient.base.util :named-readtables)
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
   :all-system-schemas :assignments
   :clean-tmps
   :dbg :display :aif :apply-transformation :ask :attributes :awhen
   :dbreak :*dval*
   :cardinality
   :component :component-operation :component-args :component-target :component-transformations
   :constraint-system
   :defconstraint-system :define-constraint :define-simple-constraint :define-system-constraint
   :defexternal-constraint-system :defexternal-system
   :defaulted-initial-data
   :display
   :dot-graph-from-plan :generate-directed-graph
   :ensure-tuples :expand-references
   :exp2
   :extern
   ::tuple :tuples :create-tuple-report-step
   :describe-transformation-calculation :defschema
   :deftransformation :deftransformation= :extract
   :find-component :find-constraint :find-schema :find-system :find-transformation :flags :flag-symbol :format-value
   :forget :generate-directed-graph :it
   :internal-implementation :external-implementation :implementation :implementation-module :implementation-name :isetq
   :link
   :log2 :logn
   :tref :trf :join :lookup-description :lookup-type :make-relation :make-relation+ :make-relation-list
   :disjoin
   :make-flag :make-signature :make-tuple :make-tuple* :make-tuple+ :operation
   :orient-tests :optimal-heights
   :org-present :org-present-tuple
   :parameter :parameter-name :parameter-description :parameter-type
   :plan :plan-for :pipeline-signature :private-attr-p :project-commit-link
   :present-data :project :publish :prune-system-for-flags
   :range :reduce-range
   :rel :relation :remove-attributes :rename :report-data :report-solution-for :representation :restrict
   :same :schema :schema-parameters :schema-description :sig :signature :signature-input :signature-output :solve :solve-for
   :separate-by-flag-combinations
   :synthesize-report-steps :symbolconc :sys :system
   :system-components :system-data :system-name :system-schema :system-subsystems
   :tpl :transformation :transformation*
   :transformation-implementation :transformation-name :transformation-signature :tref :trem :try-with
   :use-construction :use-attribute
   :where :with-attributes :with-construction :write-dot-format
   :*current-construction* :*trace-plan* :-> :=> :~> :~=> :=== :== &acc &all &group &group-by &into :!>))

(defpackage orient.interface
  (:use :common-lisp :orient :cl-json :it.bese.FiveAm :orient.base.util)
  (:import-from :fset :wb-map :convert)
  (:shadowing-import-from :fset :set)
  (:export :camel-case-to-lisp* :get-json :get-json-data :get-json-data-from-string :get-json-from-string :get-json-relation-list 
           :get-json-relation-list-from-string :dump-json :dump-json-to-string
	   :load-pipeline :load-transformation :load-tuple :load-json :<-json
           :make-relation-list
	   :test-roundtrip :with-json-encoding
	   :*alpha-sort-tuples* :*schema-package*)
  (:nicknames :interface))

(defpackage orient.cache
  (:use :common-lisp :orient :orient.interface :it.bese.FiveAm :orient.base.util)
  (:export :cache :disk-cache :mem-cache :disk-backed-mem-cache :call-with-cache)
  (:nicknames :cache))

(defpackage orient.lang
  (:use :common-lisp :orient :it.bese.FiveAm :orient.base.util :cl-json)
  (:import-from :fset :wb-map :convert)
  (:shadowing-import-from :fset :set)
  (:export :combine-systems :get-system :nested<-parsed :parse-string :source<-nested :assume)
  (:nicknames :lang))

(defpackage orient.scratch
  (:use :common-lisp :orient :orient.base.util :orient.interface :orient.lang :it.bese.FiveAm :fset :gmap)
  (:nicknames :scratch)
  (:shadowing-import-from :orient #:join #:restrict #:relation #:tuple)
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
			  #:some #:every #:notany #:notevery))
