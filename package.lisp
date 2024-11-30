;;;; package.lisp
                
(defpackage #:ros-utils
  (:shadow :rationalize)
  (:use :cl :iterate)
  (:import-from :alexandria
     :with-gensyms)
  (:import-from :serapeum
     :batches)
  (:export
   :printout->string
   :broadcast-printout
   :dbind
   :mbind
   :rationalize))

(defpackage #:prime-utils
  (:use :cl)
  (:import-from :alexandria
		:emptyp)
  (:import-from :serapeum
		:last-elt)
  (:export :primep
	   :baillie-psw
	   :miller-rabin
	   :lucas-probable
	   :modular-exponentiation
	   :binary-expansion ))

(defpackage #:prime-test
  (:use :cl :prime-utils
	:fiveam))
                
(defpackage #:ros-01
  (:shadowing-import-from :ros-utils
			  :rationalize)
  (:use :cl :iterate :cl-ppcre :computable-reals)
  (:import-from :ros-utils
		:printout->string
		:broadcast-printout
		:dbind
		:mbind)
  (:import-from :alexandria
		:copy-array
		:hash-table-keys
		:hash-table-values
		:if-let
		:iota
		:map-permutations
		:random-elt
		:set-equal
		:when-let)
  (:import-from :serapeum
		:batches
		:deq
		:dict
		:enq
		:frequencies
		:href
		:maphash-new
		:parse-float
		:qappend
		:queue
		:queue-empty-p
		:toggle-pretty-print-hash-table
		:tokens
		:words)
  (:import-from :series
		:choose-if
		:scan-range
		:collect-nth)
  (:import-from :uiop
		:split-string)
  (:import-from :repl-utilities
		:trace-package)
  #+ecl(:import-from :ext
		     :long-float-positive-infinity))
  
(defpackage #:power-series
  (:shadow "+" "-" "*" "/" "EXPT")
  (:use :cl :series)
  (:import-from :serapeum
     :dict)
  (:import-from :alexandria
     :iota))

(in-package :ros-01)

(setq *suppress-series-warnings* t)

(toggle-pretty-print-hash-table t)

#+ecl(defconstant +infinity+
       long-float-positive-infinity)
#-ecl(defconstant +infinity+
       most-positive-long-float)

(in-package :power-series)

(setq *suppress-series-warnings* t)
