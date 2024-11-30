;;;; rosetta/prime-test.lisp

;;;; Unit tests for the prime-utils package

(in-package #:prime-test)
;;; Includes symbols from fiveam,
;;; A regression testing framework.

;;; Importing the private symbols from prime-utils
;;; Public symbols are already
;;; imported in a 'use' statement
;;; in package.lisp

(import
 '(prime-utils::*small-prime-limit*
   prime-utils::init-small-primes
   prime-utils::*baillie-psw-limit*
   prime-utils::*small-primes*
   prime-utils::*largest-small-prime*
   prime-utils::square
   prime-utils::num-twos
   prime-utils::find-d
   prime-utils::u-square
   prime-utils::v-square
   prime-utils::u-inc
   prime-utils::v-inc)
 )


   

#|
Dependency tree



primep
|
|--- *small-primes* (var)
|     |
|     |--- init-small-primes
|          |
|          |--- *small-primes-limit* (var)
|--- *largest-small-prime* (var)
|     |
|     |--- *small-primes* (var, see above)
|
|--- *baillie-psw-limit* (var)
|
|--- baillie-psw
-    |
     |--- miller-rabin
-    |    |
-    |    |--- num-twos
-    |    |
-    |    |--- modular-exponentiation
-    |
-    |--- lucas-probable
-         |--- find-d
-         |    |
-         |    |--- jacobi
-         |
-         |--- binary-expansion
-         |
-         |--- u-square
-         |
-         |--- v-square
-         |    |
-         |    |--- modular-exponentiation (see above)
-         |
-         |--- u-inc
-         |
-         |--- v-inc

|#
;;; fiveam vars
(setf *on-error* :backtrace
      ;; print backtrace on unexpected error
      *on-failure* nil
      ;; keep going on test failure
      *print-names* t
      ;; print the names of tests as they are being run
      *verbose-failures* t
      ;; display as much info about failures as possible.
      )
