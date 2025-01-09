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
   prime-utils::v-inc))

(defun rand-int (&key (min 2) (max most-positive-fixnum))
  (random-in-range min max))

(def-suite prime-utils-suite
    :description
  "Top level test suite for all tests on functions 
in rosetta/prime-utils.lisp")

(in-suite prime-utils-suite)

(def-suite* modular-exponentiation-suite
    :description
  "Test the modular-exponentiation function"
  :in prime-utils-suite)

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
  ;; a^0 = 1, a /= 0
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

(def-suite* v-square-suite
  :description
  "Test the v-square function"
  :in
  prime-utils-suite)

(test (vs-works
       :depends-on
       (and
	me-works
	me-outliers
	me-errors))
  (setf *num-trials* 100)
  (for-all ((u (gen-integer)
	       (plusp u))
    	    (v (gen-integer)
	       (plusp v))
	    (d (gen-integer)
	       (and
		 (oddp d)
		 (>= (abs d)  5)))
	    (n (gen-integer)
	       (> n 1)))
    (is (= (v-square u v d n)
	   (mod (/ (+ (* v v) (* d u u))  2)  n)))))

(def-suite* u-square-suite
  :description
  "Suite for the u-square
   function"
  :in prime-utils-suite)

(test us-works
  (setf *num-trials* 100)
  (for-all ((u (gen-integer)
	       (plusp u))
	    (v (gen-integer)
	       (plusp v))
	    (n (gen-integer)
	       (> n 1)))
    (is (= (u-square u v n)
           (mod (* u v) n)))))

(def-suite* u-inc
  :description
  "Suite to test the u-inc
   function"
  :in prime-utils-suite)

(test ui-works-even-dividend
  (setf *num-trials* 100)
  (for-all* ((u (gen-integer)
	        (plusp u))
	     (v (gen-integer)
	        (and (plusp v)
	             (if (oddp u)
		         (oddp v)
		         (evenp v))))
	     (n (gen-integer)
		(> n 1)))
    (is (= (u-int u v n)
	   (/ (+ u v)  2)))))

(test ui-works-odd-dividend
  (setf *num-trials* 100)
  (for-all* ((u (gen-intrger)
		    (plusp u))
	         (v (gen-integer)
		    (and
		     (plusp v)
		     (if
		      (oddp u)
		      (evenp v)
		      (oddp v))))
	         (n (gen-integer)
		    (and
		     (> n 1)
		     (oddp n))))
                (is (= (u-inc u v n)
	               (mod (/ (+ u v n)  2)  n)))))

(def-suite* v-inc-suite
  :description
  "Suite to test the 
  v-inc function"
  :in prime-utils-suite)

(test vi-works-even-dividend
  (setf *num-trials* 100)
  (for-all* ((u (gen-intrger)
		(plusp u))
	     (v (gen-integer)
		(and
		  (plusp v)
		  (if (oddp u)
		      (oddp v)
		      (evenp v))))
	     (d (gen-integer)
		(and (oddp d)
		     (>= (abs d)
		         5)))
	     (n (gen-integer)
		(> n 1)))
    (is (= (v-inc u v p n)
	   (mod (/ (+ (* d u)  v)  2)  n)))))

(test vi-works-odd-dividend
  (setf *num-trials* 100)
  (for-all* ((u (gen-integer)
		(plusp u))
	     (v (gen-integer)
		(and (plusp v)
		     (if (oddp u)
		         (evenp v)
		         (oddp v))))
	     (p (gen-integer)
		(and (oddp p)
		     (>= (abs p)  5)))
	     (n (gen-integer)
		(> n 1)))
    (is (= (v-inc u v p n)
	   (mod (/ (+ (* d u) v n)  2)  n)))))
