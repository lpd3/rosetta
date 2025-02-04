;;;; package.lisp

(defpackage #:ros-conditions
  (:use :cl)
  (:export :type-error*
           :domain-error
           :large-range-error))
                
(defpackage #:ros-utils
  #+ecl (:shadow :rationalize)
  (:use :cl :iterate :ros-conditions)
  (:import-from :alexandria
   :with-gensyms)
  (:import-from :serapeum
   :batches)
  (:export
   :printout->string
   :broadcast-printout
   :dbind
   :mbind
   :binary-search))

(defpackage #:prime-utils
  (:use :cl :ros-conditions)
  (:import-from :alexandria
		:emptyp
		:last-elt)
  (:export :primep
	   :baillie-psw
	   :miller-rabin
	   :lucas
	   :modular-exponentiation
	   :binary-expansion
           :square
           :jacobi
           :perfect-square-p
           :primes-below-x
           :estimate-prime-count
           :sieve-of-eratosthenes
           :real-val-subseq
           :next-prime
           :segmented-sieve
           :primes-in-range))

(defpackage #:ros-01
  #+ecl (:shadowing-import-from :ros-utils
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
                :positive-integer
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
  (:import-from :prime-utils
                :next-prime)
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
