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

(defparameter *largest-small-prime*
  (aref *small-primes* (1- (length *small-primes*)))
  "The largest integer in the *small-primes* vector.")

(defun square (x)
  "The square of x"
  (* x x))

(defun primep (n)
  "Is N a prime number?
  Returns two values, both Booleans. The first is whether the number is
  prime (or probably probably prime). The second is true when the answer
  is not completely certain. The second value will always be t if the 
  the first value is nil or if n < *baillie-psw-limit*."
  (when (not (numberp n))
    (error "PRIMEP requires a number, not ~A ~S."
	   (type-of n)))
  (when (complexp n)
    ; Imaginary numbers and complex numbers that are not real integers are never prime
    (return-from primep (values nil t)))
  ; Eases dealing with float inputs
  (when (floatp n)
    (setq n (rationalize n)))
  ; Fractional numbers are never prime
  (when (not (integerp n))
    (return-from primep (values nil t)))
  
  
  (when (< n 2)
    ; n must be a negative integer, 0 or 1. None of these are prime.
    (return-from primep (values nil t)))
  (loop for prime across *small-primes*
        when (= n prime)
        ; n is one of the small primes
        do
        (return-from primep (values t t))
        when (zerop (mod n prime))
        ; n /= prime and prime|n. n is not prime.
        do
        (return-from primep (values nil t)))
  ; If we got this far, then n is larger than the largest small prime and is not divisible by any
  ; small prime. Perhaps n is a perfect square? This can be true only if n is larger than the
  ; square of the largest small prime
  (when (> n (expt *largest-small-prime* 2))
    (let ((integer-sqrt-n (isqrt n)))
      (when (= integer-sqrt-n (sqrt n))
        ; If the integer square root equals the square root, n is a perfect square, and not prime.
        (return-from primep (values nil t)))))
  ; Ok. We still don't know. Time for some tougher testing.
  (if (<= n *baillie-psw-limit*)
    ; It is known for certain that all composite numbers less than or equal to this limit will fail
    ; the Baillie-PSW Prime test. It is worth the effort to pursue this test.
    (values (baillie-psw n) t)
    ; Otherwise, we should run a miller-rabin test a sufficient number of times so that we can be
    ; so that we can provide an extremely (but not completely) reliable answer.
    ; If n fails, it must be composite. If it passes, there is an extraordinarily small
    ; chance that it is still composite. So the certainty is the opposite of the result.
    (let* ((runs (ceiling (log n 2)))
           (result (miller rabin n runs)))
      (values result (not result)))))

(defun baillie-psw (n)
  "Performs a baillie-psw test, which is guaranteed to be accurate for numbers that are no larger than the *baillie-psw-limit* An open question in mathematics is if there is any composite number
that passes this test. While both subtests produce false positives, those have been different numbers. It has been postulated that
the false positives produced are disjoint sets of numbers."
  (and (miller-rabin n) (lucas-probable n)))
	  
(defun miller-rabin (n &optional (runs 1 suppliedp))
  "Miller-Rabin Probable Prime test. Takes a mandatory arg N,
   which should be an integer larger than 4, but no type-checking
   is performed. Takes an
   optional RUNS argument, a positive integer specifying the number
   of runs to make. If RUNS is not supplied, then it is assumed
   that this test is being run as part of a Ballie-PSW test, in
   which case one run will be performed with a witness base of 2.
   Otherwise, the test will be run RUNS times, with random 
   appropriate integers being chosen as witnesses. Note that 
   a run may therefore be repeated with the same witness. 
   Returns a generalized Boolean, with t
   signifying that n is probably prime and nil signifying that n is
   is not prime with absolute certainty."
  (assert (and (integerp runs)
	       (plusp runs))
	  ()
	  "MILLER-RABIN: 'runs' argument must be positive integer, not ~A ~S" (type-of n) n)
  
  (assert (> n 3)
    ()
    "MILLER-RABIN: n must be > 3. Received ~D." n)

  (let* ((num-twos-n-minus-one (num-twos (1- n)))
	 (n-minus-one-no-twos (/ n (expt 2 num-twos-n-minus-one))))

    (dotimes (_ number-of-runs t)
      (let* ((base (if (suppliedp runs)
		       2
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
  ;; First, find suitable
  ;; D and Q parameters.
  ;; Set the P parameter to
  ;; 1. Get the binary
  ;; expansion of n+1 as
  ;; a big-endian adjustable
  ;; vector.
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
jacobi symbol n/k (this is not a quotient.)"
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
       (vector-push-extend 0 result)
       result)
      (t
       (do ((remainder n (ash n -1)))
	   ((zerop remainder) result)
	 (if (oddp remainder)
	     (vector-push-extend 1 result)
	     (vector-push-extend 0 result)))))))

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
