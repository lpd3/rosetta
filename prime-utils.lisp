;; rosetta/prime-utils.lisp
;;
;; Prime number utilities

(in-package #:prime-utils)

(defparameter *small-prime-limit*
  1000
  "The largest prime in the *small-primes* array will be 
  the largest prime number less than this integer.")

(defun init-small-primes (&optional (limit *small-prime-limit))
  "Initializes the *primes-below-1000* parameter."
  (let ((sieve (make-array (1+ limit) :element-type 'boolean :initial-element t)))
    (setf (aref sieve 0) nil
	  (aref sieve 1) nil)
    (do ((index 4 (+ index 2)))
	((> index limit))
      (setf (aref sieve index) nil))
    (do ((index 3 (+ index 2)))
	((> index limit))
      (when (aref sieve index)
	(do ((multiple (* index index) (+ multiple index)))
	    ((> multiple 1000))
	  (setf (aref sieve multiple) nil))))
    (let ((primes (make-array (1+ limit)
			      :element-type 'fixnum))
	  (prime-index 0))
      (dotimes (sieve-index (1+ limit))
	(when (aref sieve sieve-index)
	  (setf (aref primes prime-index) sieve-index)
	  (incf prime-index)))
      (adjust-array primes (fill-pointer primes))
      primes)))

(defparameter *baillie-psw-limit*
  (expt 2 64)
  "The Billie-PSW primality test is a probabilistic primality test. However, there 
  are no known composite integers that are known to return a false positive. 
  It is known with certainly that there are no false baillie-psw primes
  less than 2^80. Whether or not there are composite numbers that pass the test
  is an open question in mathematics.")

(defparameter *small-primes*
  (init-small-primes)
  "Vector that holds the prime numbers less than *small-prime-limit*")

(defparameter *largest-small-prime*
  (serapeum:last-elt *small-primes*)
  "The largest prime number in *small-primes*, also the
  largest prime number less than or equal to *small-prime-limit*")

(defun square (x)
  "The square of x"
  (* x x))

(defun primep (n)
  "Is N a prime number? Signals an error if n is not an integer >= 0."
  (when (or (not (integerp n))
	    (minusp n))
    (error "PRIMEP requires a non-negative integer, not ~A ~S."
	   (type-of n)
	   n))
  (cond
   ((< n 2)
    nil)
   ((< n 4)
    t)
   ((= (mod n 2) 0) t)
   ((< n 1000)
    (if (find n *small-primes*)
	t
      nil))
   ((<= n (square *largest-small-prime*))
    (loop with search-limit = (isqrt n)
	  for prime in *small-primes*
	  while (<= prime search-limit)
	    when (= (mod n prime) 0)
	    do
	    (return nil)
	  finally
	  (return t)))
   ((<= n *baillie-psw-limit*)
    (baillie-psw n))
   (t
    (error "Input to `primep` (~D) is too large. Numbers must be no greater than ~D" n *baillie-psw-limit*))))

(defun baillie-psw (n)
  "Performs a baillie-psw test, which is guaranteed to be accurate for numbers that are no larger than the *baillie-psw-limit* An open question in mathematics is if there is any composite number
that passes this test. While both subtests produce false positives, those have been different numbers. It has been postulated that
the false positives produced are disjoint sets of numbers."
  (and (miller-rabin n) (lucas n)))
	  
(defun miller-rabin (n &optional (number-of-runs 1) (bases: '(2)))
  "Miller-Rabin Probable Prime test. A probabilistic test, 
  it never returns a false negative. It produces false 
  positives in up to 1/4 of cases. When run alone, it is
  typical to run several times using different 'bases'. 
  When run as part of the baillie-psw test, it is run once,
  and only with base 2. Takes a positive integer n to test
  for primality. Returns a generalized boolean. Optional
  arguments are the number-of-runs (defaulting to 1) and
  bases, which should either be nil or a list of bases, 
  defaulting to '(2)'. One base is used for each run until
  the list is exhausted."
  (assert (and (integerp n)
	       (not (minusp n)))
	  ()
	  ("MILLER-RABIN: n must be a non-negative integer, not ~A ~S" (type-of n) n))
  (assert (and (integerp runs)
	       (plusp runs))
	  ()
	  ("MILLER-RABIN: 'runs' argument must be positive integer, not ~A ~S" (type-of n) n))
  (assert (and (listp bases)
	       (every #'(lambda (base)
			  (and (integerp base)
			       (> 1 base)
			       (< (1- n) base)))
		      bases))
	  ()
	  "MILLER-RABIN: Arg `bases` must be a list that is either empty, or contains only integers such that each integer is greater than 1 and less than n-1. Your entry: ~A ~S" (type-of bases) bases)
  (let* ((num-twos-n-minus-one (num-twos (1- n)))
	 (n-minus-one-no-twos (/ n (expt 2 num-twos-n-minus-one))))
    (dotimes (_ number-of-runs t)
      (let* ((base (if bases
		      (first bases)
		      (+ (random (- n 3)) 2)))
	     (x (modular-exponentiation
		 base n-minus-one-no-twos n)))
	(dotimes (_ num-twos-n-minus-one)
	  (let ((y (modular-exponentiation x 2 n)))
	    (when
		(and
		 (= y 1)
		 (/= x 1)
		 (/= x (- n 1)))
	      (return-from miller-rabin nil))
	    (setq x y)))
	(when (/= y 1)
	  (return-from miller-rabin nil))))))

(defun modular-exponentiation (n e m)
  "Given n, and exponent, and a modulus, 
  return n^e mod m"
  (assert (integerp n)
	  ()
	  "MODULAR-EXPONENTIATION: n must be integer, not ~A ~S" (type-of n) n)
  (assert (and
	   (integerp e)
	   (not (minusp e)))
	  ()
	  "MODULAR-EXPONENTIATION: exponent must be a non-negative integer, not ~A ~S" (type-of e) e)
  (assert (and
	   (integerp m)
	   (plusp m))
	  ()
	  "MODULAR-EXPONENTITATION: modulus must be a positive integer, not ~A ~S" (type-of m) m)
  (cond
   ((and (zerop n) (zerop e))
    (error "MODULAR-EXPONENTIATION: 0^0 is undefined"))
   ((= m 1) 0)
   ((zerop n) 0)
   ((= n 1) 1)
   ((zerop e) 1)
   ((= e 1) n)
   (t
    (do ((base (mod n m))
	 (result 1)
	 (exponent e))
	((zerop exponent) result)
      (when (= (mod exponenent 2) 1)
	(setf result (mod (* result base) m)))
      (setf exponent (ash exponent -1)
	    base (mod (* base base) m))))))

(defun lucas (n)
  (uiop:not-implemented-error)
  )     
 
     
