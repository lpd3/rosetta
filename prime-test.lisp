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

(defun rand-int (&key (min 2) (max most-positive-fixnum))
  (random-in-range min max))

(def-suite prime-utils
    :description
  "Top level test suite for all tests on functions 
in rosetta/prime-utils.lisp")

(def-suite* modular-exponentiation
    :description
    "Test the modular-exponentiation function"
    :in
    prime-utils)

;; args to modular-exponentiation:
;; n: the base, a non-negative integer
;; e: the exponent, a non-negative integer
;; m: the modulus, a positive integer

(test me-works
  (is (= (modular-exponentiation 4 13 497) 445))
  (is (= (modular-exponentiation 37 82 52) 49))
  (is (= (modular-exponentiation 514 5367 711) 694))
  (is (= (modular-exponentiation 3 2019 99) 81))
  (is (= (modular-exponentiation 7 644 645) 436))
  (is (= (modular-exponentiation 123 1001 101) 22)))
  
(test me-outliers
    ;; a mod 1 = 0
    (is (= (modular-exponentiation
	    (rand-int)
	    (rand-int)
	    1)
	   0))
  ;; 0^b = 0, b ≠ 0
  (is (= (modular-exponentiation
	  0
	  (rand-int)
	  (rand-int))
	 0))
  ;; 1^b = 1
  (is (= (modular-exponentiation
	  1
	  (rand-int)
	  (rand-int))
	 1))
  ;; a^0 = 1, a ≠ 0
  (is (= (modular-exponentiation
	  (rand-int)
	  0
	  (rand-int))
	 1))
  ;; a^1 = a (we first make sure m > n)
  (let* ((n (rand-int :max 100000))
	 (m (+ n (rand-int :max n))))
    (is (= (modular-exponentiation
	    n
	    1
	    m)
	   n))))

(test me-errors
  ;; n must be an integer
  (signals error
    (modular-exponentiation
     (rand 123456.789)
     (rand-int)
     (rand-int)))
  (signals error
    (modular-exponentiation
     "forty-two"
     (rand-int)
     (rand-int)))
  ;; e must be a non-negative integer
  (signals error
    (modular-exponentiation
     (rand-int)
     (rand 123456.789)
     (rand-int)))
  (signals error
    (modular-exponentiation
     (rand-int)
     #\z
     (rand-int)))
  (signals error
    (modular-exponentiation
     (rand-int)
     (- (rand-int))
     (rand-int)))
  ;; m must be a positive integer
  (signals error
    (modular-exponentiation
     (rand-int)
     (rand-int)
     (rand 123456.789)))
  (signals error
    (modular-exponentiation
     (rand-int)
     (rand-int)
     '(a number)))
  (signals error
    (modular-exponentiation
     (rand-int)
     (rand-int)
     (- (rand-int))))
  (signals error
   (modular-exponentiation
    (rand-int)
    (rand-int)
    0))
  ;; 0^0 is undefined
  (signals error
    (modular-exponentiation
     0
     0
     (rand-int))))









   

#|
Dependency tree				; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
primep					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|--- *small-primes* (var)		; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|     |					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|     |--- init-small-primes		; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|          |				; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|          |--- *small-primes-limit* (var) ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|--- *largest-small-prime* (var)	; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|     |					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|     |--- *small-primes* (var, see above) ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|--- *baillie-psw-limit* (var)		; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|--- baillie-psw			; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-    |					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
|--- miller-rabin			; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-    |    |				; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-    |    |--- num-twos			; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-    |    |				; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-    |    |--- modular-exponentiation	; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-    |					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-    |--- lucas-probable		; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |--- find-d			; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |    |			; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |    |--- jacobi		; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |				; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |--- binary-expansion		; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |				; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |--- u-square			; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |				; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |--- v-square			; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |    |			; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |    |--- modular-exponentiation (see above) ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |				; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |--- u-inc			; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |				; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
-         |--- v-inc			; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
					; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
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
