;; rosetta/prime-utils.lisp
;;
;; Prime number utilities

(in-package #:prime-utils)

(defparameter *small-prime-limit*
  1000
  "The largest prime in the *small-primes* array will be 
  the largest prime number less than this integer.")

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
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
        primes))))

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
    ; Ok. We still don't know. Time for some tougher testing.
  (if (<= n *baillie-psw-limit*)
    ; It is known for certain that all composite numbers less than or equal to this limit will fail
    ; the Baillie-PSW Prime test. It is worth the effort to pursue this test.
    (values (baillie-psw n) t)
    ; It has not been proven that the Baillie-PSW test is fool-proof against all odd composite
    ; numbers > the *baillie-psw-limit*. However no composite number has been found that foils
    ; the test. We should run the test. If the number passes the test, we will report also that
    ; the result is uncertain. If the number fails the test, it is certainly composite. Therefore,
    ; the certainty will be the opposite of the test result.
    (let* ((result (baillie-psw n))
           (certainty (not result)))
      (values result certainty))))


;; This implementation was cribbed from
;; https://github.com/armchaircaver/Baillie-PSW/blob/main/baillie_psw.py

(defun baillie-psw (n)
  "https://en.m.wikipedia.org/wiki/Baillie%E2%80%93PSW_primality_test 
Performs a baillie-psw test, which is guaranteed to be accurate for numbers that are no larger
than the *baillie-psw-limit*.  An open question in mathematics is if there is any composite number
that passes this test. While both subtests produce false positives, those have been different
numbers. It has been postulated that
the false positives produced are disjoint sets of numbers."
  (unless (miller-rabin n)
    (return-from baillie-psw nil))
  
  (multiple-value-bind
        (d n-is-still-probable-prime)
        (lucas-d-chooser n)
    (unless n-is-still-probable-prime
      (return-from baillie-psw nil))

    (lucas n d 1)))
	  
(defun miller-rabin (n &optional (base 2))
  "https://en.m.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
  Miller-Rabin probable-prime test. In addition to N, the number we 
  are checking (which should already be tested against small primes,
  and should be > 3), takes an optional BASE argument. This defaults
  to 2, which is the correct base when this test is run as part of
  a Baillie-PSW test. Otherwise, the test should be run several times,
  with different BASEs chosen for each run such that, for each BASE,
  1 < BASE < N-1. Returns a Boolean. A nil result means that N is
  provably composite. A t means N is probably prime, with the 
  probability of being composite decreasing exponentially in 
  proportion to the number of test runs."
  (assert (and (oddp n)
               (> n 3))
    ()
    "MILLER-RABIN: N must be odd and greater than 3. Received ~D." n)
  (assert (< 1 base (1- n))
    ()
    "MILLER-RABIN: BASE ~D out of range for N ~D"
    base
    n)
  ;; Find s and d such that 2^sd = n-1.
  (multiple-value-bind
        (d s)
        (find-miller-rabin-d-s n)
   ;; The Miller-Rabin algorithm
    (do* ((i 0 (1+ i))
          (x (modular-exponentiation base d n) y)
          (y #1=(modular-exponentiation x 2 n) #1#))
         ;; If the final y is 1, then n is probably prime. Otherwise, n is demonstrably composite.
         ((= i s) (= y 1)) 
      (when (and (= y 1) (/= x 1) (/= x (1- n)))
        ;; y is a non-trivial square root of 1 modulo n. n is composite.
        (return-from miller-rabin nil)))))

(defun find-miller-rabin-d-s (n)
  "Helper function for Miller-Rabin"
  (do ((d (1- n) (/ d 2))
       (s 0 (1+ s)))
      ((oddp d) (values d s))))

(defun modular-exponentiation (n e m)
  "Given N (the base: non-negative integer) M (modulus, positive integer, 
   return N^E mod M."
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

(defun lucas-m-d-chooser (n)
  "Helper function for Baillie-PSW. Given odd positive N, searches
   for an odd integer D such that the Jacobi symbol d/N /= 1. 
   If N is not a perfect square, the algorithm finds such a d in
   1.76 iterations on average. However, if N is a perfect square, 
   the algorithm would never terminate. After a few iterations, 
   N is checked to see if it is a square. This is done now instead
   of earlier because checking for a perfect square is slightly 
   more expensive then a few runs of this algorithm. Returns two
   values. If the Jacobi function successfully returns 0 or 1, 
   d is returned as the first value. The second boolean value
   indicates whether or not N is still a candidate for prime testing.
   If the Jacobi symbol is 0, then d and N are not coprime and 
   N is composite. In that case, the second value is nil. If
   N is a perfect square, 0 and nil are returned. If 
   the Jacobi symbol is -1 (the only other possibility), 
   then d and T are returned."
  (do* ((d 5 (if (plusp d)
                 (- (+ d 2))
                 (- (- d 2))))
        (j #1=(jacobi d n) #1#))
       ((j /= 1) (ecase j
                   (0 (values d nil))
                   (-1 (values d t))))
    (when (= d -15)
      (when (perfect-square-p n)
        (return-from lucas-d-chooser (values 0 nil))))))

(defun jacobi (m k)
  "Given M (integer) and K (positive odd integer),
  return the Jacobi symbol M/K. This will be one of
  -1, 0 or 1."
  (assert (integerp m)
    ()
    "JACOBI: First arg must be an integer, not ~A ~S" (type-of m) m)
  (assert (and (integerp k) (oddp k) (plusp k))
    ()
    "JACOBI: Second arg must be a positive, odd integer, not ~A ~D" (type-of k) k)
  (do ((next-m (mod m k) (mod next-m next-k))
       (next-k k)
       (possible-j 1))
      ((zerop next-m) (if (= next-k 1)
                          possible-j
                          0))
    (do ()
        ((oddp next-m))
      (setf next-m (/ next-m 2))
      (when (member (mod next-k 8) '(3 5))
        (setf possible-j (- possible-j))))
    (rotatef next-m next-k)
    (when (= 3 (mod next-m 4) (mod next-k 4))
      (setf possible-j (- possible-j)))))

(defun perfect-square-p (x)
  "Is X (a number) a perfect square?"
  (assert (numberp x)
    ()
    "PERFECT-SQUARE-P: x must be a number, not ~A ~S" (type-of x) x)
  (let ((coerced-x
          (if (floatp x)
              (rationalize x)
              x)))
    (cond
      ((not (integerp x))
       nil)
      ((minusp x)
       nil)
      ((zerop x)
       t)
      (t
       (= (sqrt x) (isqrt x))))))

(defun lucas (n d p)
  "Lucas Probable Prime Test. Takes 3 args: 
  N (the number we wish to check for primality, odd integer greater than 1),
  D (a math parameter: odd integer) and p (math parameter: integer).
  Returns T if n is probably prime and NIL if N is 
  demonstrably composite. Performs no value checking.
  Is assumed to be run as part of the Baillie-PSW test,
  otherwise, the test should be complimented by initial
  value checks, tests for divisibility by small primes, 
  and this test should be run multiple times with different
  parameters."

  (zerop (u-v-subscript n p d)))

(defun u-v-subscript (n p d)
  "Helper function for the Lucas test. Takes 3 integer args:
   N, P, D. N is the number we are checking for primality.
   The others are math parameters."
  (let ((digits (binary-expansion (1- n)))
        (u 1)
        (v p))
    (dotimes (i (1- (length digits)) u)
      (let* ((index (1+ i))
             (digit (aref digits index)))
        (setf u (mod (* u v) n)
              v (div2mod (+ (* v v) (* d u u)) n))
        (unless (zerop digit)
          (setf u (div2mod (+ (* p u) v) n)
                v (div2mod (+ (* d u) (* p v)) n)))))))

(defun div2mod (x n)
  "Helper function for u-v-subscript. Both args, X and N are
   integers."
  (if (oddp x)
      (mod (ash (+ x n) -1) n)
      (mod (ash x -1) n)))

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

(defun u-v-subscript (k n p d)
  "Helper function for the lucas test"
  (let ((u 1)
        (v p)
        (digits (binary-expansion n)))
    (do ((i 1 (1+ i))
         (u-next (mod (* u v) n) (mod (* u-next v-next) n))
         (v-next (div2mod (+ (* v v) (* d u u)) n)
                 (div2mod (+ (* v-next v-next) (* d u-next u-next)) n)))
        ((= i (length digits)) (values u-next v-next))
      (let ((digit (aref digits i)))
        (when (= digit 1)
          (setf u-next (div2mod (+ (* p u-next) v-next) n)
                v-next (div2mod (+ (* d u-next) (* p v-next)) n)))))))

