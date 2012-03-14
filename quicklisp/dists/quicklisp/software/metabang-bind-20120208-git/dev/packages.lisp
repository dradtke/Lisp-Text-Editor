(in-package #:common-lisp-user)

(defpackage #:metabang.bind
    (:use #:common-lisp)
    (:nicknames #:bind #:metabang-bind)
    (:intern 
     #:bind-generate-bindings
     #:bind-filter-declarations
     #:bind-macro-helper
     #:bind-fix-nils)
    (:export 
     #:bind
     #:fluid-bind

     #:binding-forms
     #:binding-form-synonyms
     #:binding-form-groups
     #:binding-form-docstring
     #:binding-form			;for documentation

     #:*bind-all-declarations*
     #:*bind-non-var-declarations*
     #:*bind-lambda-list-markers*

     #:bind-error
     #:bind-keyword/optional-nil-with-default-error
     #:bind-missing-value-form-warning
     #:bind-too-many-value-forms-error

     #:*unused-declarations-behavior*
     #:bind-unused-declarations-condition
     #:bind-unused-declarations-warning
     #:bind-unused-declarations-error

     #:lambda-bind))

(defpackage #:metabang.bind.developer
    (:use #:common-lisp #:metabang-bind)
    (:import-from #:metabang-bind
		  #:bind-generate-bindings
		  #:bind-filter-declarations
		  #:bind-macro-helper
		  #:bind-fix-nils)
    (:export 
     #:bind-generate-bindings
     #:bind-filter-declarations
     #:bind-macro-helper
     #:bind-fix-nils
     #:defbinding-form))

