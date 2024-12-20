;; rosetta/prime-utils.lisp
;;
;; Prime number utilities

(in-package #:prime-utils)

(defparameter *small-prime-limit*
  1000
  "The largest prime in the *small-primes* array will be 
  the largest prime number less than this integer.")

(defun init-small-primes (&optional (limit *small-prime-limit*))
  "Initializes the *small-primes* parameter."
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
			      :element-type 'fixnum
			      :adjustable t
			      :fill-pointer 0))
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
   ((<= n (square (last-elt *small-primes*)))
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
  (and (miller-rabin n) (lucas-probable n)))
	  
(defun miller-rabin (n &optional (number-of-runs 1) (bases '(2)))
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
  the list is exhausted. Sssumes the test is being run
as part of a Baillie-PSW test. The n is not 
error-checked."
  (assert (and (integerp runs)
	       (plusp runs))
	  ()
	  "MILLER-RABIN: 'runs' argument must be positive integer, not ~A ~S" (type-of n) n)
  (assert (and (listp bases)
	       (every #'(lambda (base)
			  (and (integerp base)
			       (> 1 base)
			       (< (1- n) base)))
		      bases))
	  ()
	  "MILLER-RABIN: Arg `base` must be a list that is either empty, or contains only integers such that each integer is greater than 1 and less than n-1. Your entry: ~A ~S" (type-of bases) bases)
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

(defun num-twos (n)
  "Helper function for miller-rabin.
   Given integer n, return
   k such that 2^kd = n,
   where d is an odd 
   integer. Performs no 
   error checking."
  (if (zerop n)
      0
      (do ((k 0 (1+ k))
	   (d n (/ d 2)))
	  ((oddp d) k))))

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
       (when (= (mod exponent 2) 1)
	 (setf result (mod (* result base) m)))
       (setf exponent (ash exponent -1)
	     base (mod (* base base) m))))))

(defun lucas-probable (n)
  "Lucas pobable prime test, a probalistic prime test based
on a Lucas series.
It is guaranteed to never produce a 
false negative. On occasion,  it produces a false
positive. Assumes the test 
is being run as part of a Baillie-PSW test. Does no 
input error checking."
  ;; We already know n is
  ;; 1. odd
  ;; 2. greater than 2
  ;; 3. not divisible by
  ;; small primes.
  ;; As a final preliminary
  ;; test, we need to make
  ;; sure that n is not
  ;; a perfect square
  (let ((int-sqrt (isqrt n)))
    (when (= (* int-sqrt int-sqrt) n)
      (return-from lucas-probable nil)))
  ;; ok. The main test.
  ;; First, find suitable
  ;; D and Q parameters.
  ;; Set the P parameter to
  ;; 1. Get the binary
  ;; expansion of n+1 as
  ;; a big-endian adjustable
  ;; vector
  (let* ((d (find-d n))
	 (q (/ (- 1 d) 4))
	 (p 1)
	 (expansion (binary-expansion (+ n 1))))
    ;; Calculate U[k], V[k]
    ;; and Q^k as determined
    ;; by expansion
    (do* ((bit #1=(vector-pop expansion) #1#)
	  (k 1 (next-k k bit))
	  (u 1)
	  (v p))
	 ((emptyp expansion)
	  (/= (mod u n) 0))
      (setv u (u-square u v n))
      (setv v (v-square u v d n))
      (when (= bit 1)
	(setv u (u-inc u v p n))
	(setv v (v-inc u v d p n))))))

(defun find-d (n)
  "Finds and returns the first integer
D in the series 5, -7, 9, 
-11, 13, -15, ... for which
the Jacobi symbol D/n = -1."
  (loop for d = 5 then (* -1 (+ 2 m))
	when (= (jacobi d n) -1)
	do
	  (return-from find-d d)))

(defun jacobi (n k)
  "Given two integers n and k, return the 
jacobi symbol n/k."
  (assert (integerp n)
	  ()
	  "JACOBI: first arg must be an integer, not ~A ~S" (type-of n) n)
  (assert (and
	   (integerp k)
	   (oddp k)
	   (plusp k))
	  ()
	  "JACOBI: second arg must be a positive, odd integer, not ~A ~S" (type-of k) k)
  (do ((n (mod n k))
       (tee 1))
      ((= n 0) (if (= k 1)
		   tee
		   0))
    (do ()
	((= (mod n 2) 1))
      (setv n (/ n 2))
      (let ((r (mod k 8)))
	(when (member r '(3 5))
	 (setv tee (- tee)))
	(rotatef n k)
	(when (and
	       (= (mod n 4) 3)
	       (= (mod k 4) 3))
	  (setf tee (- tee)))
	(setf n (mod n k))))))

(defun binary-expansion (n)
  "Given a non-negative 
decimal integer n, return
an adjustable vector containing
the big-endian bits of the 
binary equivalent of n"
  (assert (and (integerp n)
	       (not (minusp n)))
	  ()
	  "BINARY-EXPANSION: arg must be a non-negative integer, not ~A ~S"
	   (type-of n) n)
  (let ((result
	 (make-array 100
		     :element-type fixnum
		     :adjustable t
		     :fill-pointer 0)))
    (cond
      ((zerop n)
       (push 0 result)
       result)
      (t
       (do ((remainder n (ash n -1)))
	   ((zerop remainder) result)
	 (if (oddp remainder)
	     (push 1 result)
	     (push 0 result)))))))

(defun u-square (u v n)
  "Helper function for lucas-probable. With args U[k], 
V[k] and n, returns 
U[2k] mod n."
  (mod (* u v) n))

(defun v-square (u v d n)
  "Helper function for lucas-probable. With args U[k],
V[k], D and n, returns 
V[2k] mod n."
  (mod
   (/
    (+
     (modular-exponentiation
      v 2 n)
     (*
      d
      (modular-exponentiation u 2 n)))
    2)
   n))

(defun u-inc (u v p n)
  "Helper function for lucas-probable.
Given U[2k], V[2k], P and n,
returns
U[2k+1] mod n"
  (let ((dividend
	 (mod
	  (+
	   (* p u)
	   v)
	  n)))
    (if (oddp dividend)
	(mod
	 (/
	  (+ dividend n)
	  2)
	 n)
	(/ dividend 2))))

(defun v-inc (u v d p n)
  "Helper function for lukas-probable.
Given U[2k], V[2k], D, P and n, returns
V[2k+1]"
  (let ((dividend
	 (mod
	  (+
	   (* d u)
	   (* p v))
	  n)))
    (if (oddp dividend)
	(mod
	 (/
          (+ dividend n)
	  2)
	 n)
	(/ dividend 2))))




 
     
