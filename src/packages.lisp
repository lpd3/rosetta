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

(defpackage #:rosetta-main
  (:use :cl)
  (:export :main))
  
(defpackage #:hickerson
  (:use :cl)
  (:import-from :computable-reals
   :*print-prec*
                :*creal-tolerance*
   :print-r
                :*r
   :/r
                :+log2-r+
   :expt-r
                :truncate-r
   :make-real))

(defpackage #:pierpont-primes
  (:use :cl)
  (:import-from :prime-utils
   :primep
                :primes-in-range)
  (:import-from :serapeum
   :do-each
   :take
   :extrema)
  (:import-from :alexandria
   :last-elt))

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
