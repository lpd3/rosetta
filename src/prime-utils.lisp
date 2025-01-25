;; rosetta/prime-utils.lisp
;;
;; Prime number utilities

(in-package #:prime-utils)

;;; Primality testing of individual numbers: the Baillie-PSW test

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (progn
    (defparameter *small-primes-limit*
      1000
      "The largest prime in the *small-primes* array will be 
       the largest prime number less than this integer.")

    (defparameter *small-primes*
      nil
      "Vector that holds the prime numbers less than *small-primes-limit*")
    (defun init-small-primes (&optional (limit *small-primes-limit*))
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
	        ((> multiple *small-primes-limit*))
	      (setf (aref sieve multiple) nil))))
        (let ((primes (make-array (1+ limit)
			          :element-type 'integer
			          :adjustable t
			          :fill-pointer 0)))
          (dotimes (sieve-index (1+ limit))
	    (when (aref sieve sieve-index)
	      (vector-push sieve-index primes)))
          primes)))
    (setf *small-primes* (init-small-primes))))

(defparameter *largest-small-prime*
  (last-elt *small-primes*))


(defparameter *baillie-psw-limit*
  (expt 2 64)
  "The Billie-PSW primality test is a probabilistic primality test. However, there 
  are no known composite integers that are known to return a false positive. 
  It is known with certainly that there are no false baillie-psw primes
  less than 2^80. Whether or not there are composite numbers that pass the test
  is an open question in mathematics.")

(defun square (x)
  "The square of x"
  (unless (numberp x)
    (error 'type-error*
      :argument x
      :type (type-of x)
      :expected-type 'number
      :location 'square))
  (* x x))

(defun primep (n)
  "Is N a prime number?
  Returns two values, both Booleans. The first is whether the number is
  prime (or probably probably prime). The second is true when the answer
  is not completely certain. The second value will always be t if the 
  the first value is nil or if n < *baillie-psw-limit*."
  (when (not (numberp n))
    (error 'type-error*
      :argument n
      :type (type-of n)
      :location 'primep
      :expected-type 'number))
  (when (complexp n)
    ;; Complex numbers that are not real are never prime.
    ;; If the imaginary part were integer 0, Lisp would have
    ;; already silently coerced the number to a real. This
    ;; does not happen when the imaginary part is float 0.0
    ;; (to preserve the numeric type on underflow).
    ;; So we need this test.
    (if (zerop (imagpart n))
        (setq n (realpart n))
        (return-from primep (values nil t))))
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
  
  (let ((d (lucas-d-chooser n)))
    (if d
        (lucas n d 1)
        nil)))
	  
(defun miller-rabin (n &optional (base 2))
  "https://en.m.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
  Miller-Rabin probable-prime test. In addition to N, the positive odd integer we 
  are checking (which should already be tested against small primes,
  and should be > 3), takes an optional BASE argument. This defaults
  to 2, which is the correct base when this test is run as part of
  a Baillie-PSW test. Otherwise, the test should be run several times,
  with different BASEs chosen for each run such that, for each BASE,
  1 < BASE < N-1. Returns a Boolean. A nil result means that N is
  demonstrably composite. A t means N is probably prime, with the 
  probability of being composite decreasing exponentially in 
  proportion to the number of test runs."
  (unless (and (oddp n)
               (> n 3))
    (error 'domain-error
      :argument n
      :location "MILLER-RABIN: n"
      :domain "n > 3, n is odd"))
  (unless (< 1 base (1- n))
    (error 'domain-error
      :argument base
      :location "MILLER-RABIN: base"
      :domain (format nil "1 < base < n-1 (n = ~D)" n)))
  ;; Find s and d such that 2^sd = n-1.
  (multiple-value-bind
        (d s)
        (find-miller-rabin-d-s n)
   ;; The Miller-Rabin algorithm
    (do* ((i 0 (1+ i))
          (x (modular-exponentiation base d n)))
         ;; If the final y is 1, then n is probably prime. Otherwise, n is demonstrably composite.
         ((= i s) (= x 1))
      (let ((y (modular-exponentiation x 2 n)))
        (when (and (= y 1) (/= x 1) (/= x (1- n)))
          ;; y is a non-trivial square root of 1 modulo n. n is composite.
          (return-from miller-rabin nil))
        (setf x y)))))

(defun find-miller-rabin-d-s (n)
  "Helper function for Miller-Rabin"
  (do ((d (1- n) (/ d 2))
       (s 0 (1+ s)))
      ((oddp d) (values d s))))

(defun modular-exponentiation (n e m)
  "Given N (the base: non-negative integer) M (modulus, positive integer, 
   return N^E mod M."
  (unless (integerp n)
    (error 'type-error*
           :argument n
           :type (type-of n)
           :location "MODULAR-EXPONENTIATION: n"
           :expected-type 'integer))
  (when (minusp n)
    (error 'domain-error
           :argument n
           :location "MODULAR-EXPONENTIATION: n"
           :domain "n >= 0"))
  (unless (integerp e)
    (error 'type-error*
           :argument e
           :type (type-of e)
           :location "MODULAR-EXPONENTIARION: e"
           :expected-type 'integer))
  (when (minusp e)
    (error 'domain-error
           :argument e
           :location "MODULAR-EXPONENTIATION: e"
           :domain "e >= 0"))
  (unless (integerp m)
    (error 'type-error*
           :argument m
           :type (type-of m)
           :location "MODULAR-EXPONENTIATION: m"
           :expected-type 'integer))
  (unless (plusp m)
    (error 'domain-error
           :argument m
           :location "MODULAR-EXPONENTIATION: m"
           :domain "m > 1"))
  (cond
    ((and (zerop n) (zerop e))
     (error 'domain-error
            :argument '(0 0)
            :location "MODULAR-EXPONENTIATION: n & e"
            :domain "n and e may not both be 0."))
    ((= m 1) 0)
    ((zerop n) 0)
    ((= n 1) 1)
    ((zerop e) 1)
    ((= e 1) (mod n m))
    (t
     (do ((base (mod n m))
	  (result 1)
	  (exponent e))
	 ((zerop exponent) result)
       (when (= (mod exponent 2) 1)
	 (setf result (mod (* result base) m)))
       (setf exponent (ash exponent -1)
	     base (mod (* base base) m))))))

(defun lucas-d-chooser (n)
  "Helper function for Baillie-PSW. Given odd positive N, searches for
   an integer d in the series 5, -7, 9, -11, ... such that the Jacobi
   symbol d/N is not 1. If the symbol is 0, then we know that N is 
   composite and NIL is returned. If the symbol is -1, we first 
   check to make sure that (1 - d) / 4 shares no common factors with 
   N. If they share common factors, NIL is returned. If they do not, 
   then N is a candidate for further testing, and d is returned.

   If N is a perfect square, the procedure used here could easily
   lead to an infinite loop. Since a result in non-square N is 
   usually found quickly (average 1.7 iterations), the loop 
   pauses when d reaches -15. At that point, N is tested to see
   if it is a perfect square. If so, NIL is immediately returned.
   Otherwise, the search continues.

   Checking for squares is delayed because the square check is 
   more expensive then a few iterations of the search, and a Jacobi
   symbol of 0 will be revealed early on. This that n not be 5 or 11, since
   this chooser will return nil for both: in both cases, n is encountered in the
   series, producing a Jacobi symbol of 0. No other odd prime in *small-primes*
   exibits this behavior. Therefore, we return a pre-selected d in these cases."
  (case n
    (5 -7)
    (11 13)
    (otherwise
     (do* ((d 5 (if (plusp d)
                    (- (+ d 2))
                    (- (- d 2))))
           (j #1=(jacobi d n) #1#))
          ((/= j 1) (unless (zerop j)
                      d))
       (when (= d -15)
         (when (perfect-square-p n)
           (return-from lucas-d-chooser nil)))))))

(defun jacobi (m k)
  "Given M (integer) and K (positive odd integer),
  return the Jacobi symbol M/K. This will be one of
  -1, 0 or 1."
  (unless (integerp m)
    (error 'type-error*
           :argument m
           :type (type-of m)
           :location "JACOBI: m"
           :expected-type 'integer))
  (unless (integerp k)
    (error 'type-error*
           :argument k
           :type (type-of k)
           :location "JACOBI: k"
           :expected-type 'integer))
  (unless (and (oddp k) (plusp k))
    (error 'domain-error
           :argument k
           :location "JACOBI: k"
           :domain "k > 0, k mod 2 = 1"))
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
  (unless (numberp x)
    (error 'type-error*
      :argument x
      :type (type-of x)
      :location 'perfect-square-p
      :expected-type 'number))
  (let ((coerced-x
          (if (floatp x)
              (rationalize x)
              x)))
    (cond
      ((not (integerp coerced-x))
       nil)
      ((minusp coerced-x)
       nil)
      ((zerop coerced-x)
       t)
      (t
       (= (sqrt coerced-x) (isqrt coerced-x))))))

(defun lucas (n d p)
  "Lucas Probable Prime Test. Takes 3 args: 
  N (the number we wish to check for primality, odd integer greater than 101),
  D (a math parameter: odd integer) and p (math parameter: integer).
  Returns T if n is probably prime and NIL if N is 
  demonstrably composite. Performs no value checking.
  Is assumed to be run as part of the Baillie-PSW test,
  otherwise, the test should be complimented by initial
  value checks, tests for divisibility by small primes, 
  and this test should be run multiple times with different
  parameters.

  When this test is run as part of a Baillie-PSW test, then
  d will be an odd integer such that jacobi (d/n) = -1, and
  p always will be 1."

  (unless (integerp n)
    (error 'type-error*
           :argument n
           :type (type-of n)
           :expected-type 'integer
           :location "LUCAS: n"))
  
  (unless (and (> n 1) (oddp n))
    (error 'domain-error
           :argument n
           :domain "n > 1 and n is odd."
           :location "LUCAS: n"))
  (unless (integerp p)
    (error 'type-error*
           :argument p
           :type (type-of p)
           :expected-type 'integer
           :location "LUCAS: d"))
  (unless (plusp p)
    (error 'domain-error
           :argument p
           :domain "p: positive integer"
           :location "LUCAS: p"))
  (unless (integerp d)
    (error 'type-error*
           :argument d
           :type (type-of d)
           :expected-type 'integer
           :location "LUCAS: d"))
  (zerop (u-v-subscript n d p)))

(defun u-v-subscript (n d p)
  "Helper function for the Lucas test. Takes 3 integer args:
   N, P, D. N is the number we are checking for primality.
   The others are math parameters. When this test is run as
   part of the Baillie-PSW test, d will be an odd integer 
   such that jacobi (d/n) = -1. In a Baillie-PSW test, p
   is always 1."
   (let ((u 1)
         (v p)
         (digits (binary-expansion (1+ n))))
     (do ((i 1 (1+ i)))
         ((= i (length digits)) u)
       (psetq u (mod (* u v) n)
              v (div2mod (+ (* v v) (* d u u)) n))
       (let ((digit (aref digits i)))
         (when (= digit 1)
           (psetq u (div2mod (+ (* p u) v) n)
                  v (div2mod (+ (* d u) (* p v)) n)))))))


(defun div2mod (x n)
  "Helper function for u-v-subscript. Both args, X and N are
   integers."
  (if (oddp x)
      (mod (ash (+ x n) -1) n)
      (mod (ash x -1) n)))

(defun binary-expansion (n &optional little-endian-p)
  "Given a non-negative 
decimal integer N, return
an adjustable vector containing bits of the 
binary equivalent of n. Takes an optional Boolean arg
LITTLE-ENDIAN-P. If true, the result will be in little-endian
order. If nil (the default), the result will be in big-endian order."
  (unless (integerp n)
    (error 'type-error*
      :argument n
      :type (type-of n)
      :location 'binary-expansion
      :expected-type 'integer))
  (when (minusp n)
    (error 'domain-error
      :argument n
      :location 'binary-expansion
      :domain "n >= 0"))
  (if (< n 2)
      (make-array 1
        :element-type 'fixnum
        :initial-element n)
      (do ((remainder n (ash remainder -1))
           (work-list nil))
          ((zerop remainder)
           (coerce
            (if little-endian-p
                (nreverse work-list)
                work-list)
            '(vector fixnum *)))
        (if (oddp remainder)
            (push 1 work-list)
            (push 0 work-list)))))

;;; General Utilities for prime numbers

(defun primes-below-x (x)
  "Given x (a real number), return an array containing all the prime numbers less than x.
  Coerces x to an integer. If the integer is less than 2, an empty array is returned.
  Does not limit the size of the array created. You have been warned."
  (unless (realp x)
    (error 'type-error*
           :argument x
           :type (type-of x)
           :expected-type "REAL NUMBER"
           :location 'primes-below-x))
  (let ((n (floor x)))
    (cond
      ((< n 2)
       (make-array 1
         :element-type 'integer
         :adjustable t
         :fill-pointer 0))
      ((< n *largest-small-prime*)
       (let ((i (position-if #'(lambda (p) (>= p n)) *small-primes*)))
         (alexandria:copy-array
          (make-array i
            :element-type 'integer
            :displaced-to *small-primes*)
          :adjustable t)))
      ((< n *small-primes-limit*)
       (alexandria:copy-array *small-primes* :adjustable t))
      (t
       (let ((sieve (sieve-to% n))
             (primes (make-array 200
                       :element-type 'integer
                       :adjustable t
                       :fill-pointer 0)))
         (loop for i from 0
               for bool across sieve
               when bool
                 do
               (vector-push-extend i primes)
               finally
               (return primes)))))))

(defun sieve-to% (n)
  "Helper function for primes-below-x"
  (assert (typep n '(and integer (satisfies plusp) (not (member 0 1)))))
  (let ((sieve ; each number represented by its index. We start off assuming all are prime.
          (make-array n
                      :element-type 'bit
                      :initial-element 1)))
    ; 0 and 1 are not prime
    (setf (aref sieve 0) 0
          (aref sieve 1) 0)
    ; 2 is prime. Leave it alone. Sieve out multiples of 2
    (do ((2-multiple 4 (+ 2-multiple 2)))
        ((>= 2-multiple n))
      (setf (bit sieve 2-multiple) 0))
    ; All other primes are odd. Starting with 3, we consider each odd index. If the value is nil,
    ; we have already established that that index is composite. Move on.
    ; If the value is t, the index is prime. Sieve out its multiples.
    ; We need to look for primes only up to the floor of the sqrt of n.
    (do ((prime-max (isqrt n))
         (possible-prime 3 (+ possible-prime 2)))
        ((> possible-prime prime-max) sieve)
      (when (plusp (bit sieve probable-prime))
        ; Starting with the square of possible-prime (possible-prime is now definitely prime) mark all multiples nil.
        (do* ((prime possible-prime)
              (multiple (* prime prime) (+ multiple prime)))
             ((> multiple n))
          (setf (bit sieve multiple) 0)))
      (let ((primes (make-array (ceiling n (log n))
                                 :element-type 'integer
                                 :adjustable t)))
        (dotimes (i (length sieve) primes)
          (when (plusp (bit sieve i))
            (vector-push-extend i primes)))))))

(defparameter *sequential-search-low-ceiling*
  64
  "Calls to primes in range in which *small-primes-limit* <= STOP
   and the range (STOP - START) < *sequential-search-lower-max* 
   will employ sequential prime testing of individual integers.
   Default: 32")

(defparameter *plain-sieve-limit*
  (expt 2 22)
  "Calls to primes-in-range will use a simple
   sieve of Erastothenes when range (STOP - START) > *sequential-search-lower-max*
   and *small-primes-limit* <= STOP < *plain-sieve-limit*
   It defaults to 2^22: 4,194,304.")

(defparameter *segmented-sieve-limit*
  (expt 2 44)
  "Calls to primes-in-range with range (STOP - START) >= *sequential-sieve-lower-max*
   and with *plain-sieve-max* <= STOP < *segmented-sieve-max* 
   will employ a segmented sieve. When STOP >= *segmented-sieve-limit*, 
   regardless of range, sequential prime testing is used instead.
   Default is 2^44: 17,592,186,044,416.")

(defparameter *soft-range-max*
  (expt 2 22)
  "When a call to primes-in-range is made with a range (STOP - START) > *soft-range-max*,
   a correctable error will be signaled, permitting the user to proceed 
   anyway upon careful reflection
   Default: 2^22: 4,194,304")

(defun primes-in-range (start stop)
  "Given two non-negative integers, START and
   STOP, return an array of prime numbers in the
   from START below STOP. If STOP >= START, an 
   empty array is returned. When sensible arguments
   are provided, the algorithm chosen to accomplish this
   feat depends on the arguments:

   1. When a. the range is < 1 or b. the range is 1, START > 2 and START is even, return an empty vector.
   2. When STOP is less than *small-primes-limit*, return a slice of *small-primes*
   3. When STOP is less than *plain-sieve-limit*, truncate a standard sieve.
   4. When range is less than *sequential-search-low-ceiling*, test for primes individually.
   5. When STOP is less than *segmented-sieve-limit*, perform a segmented sieve.
   6. Otherwise, test for primes individually

   Signals a correctable error if range is greater than 5 million. 
   The user can choose to proceed anyway." 
  (unless (integerp start)
    (error 'type-error*
           :argument start
           :type (type-of start)
           :expected-type 'integer
           :location "PRIMES-IN-RANGE: start"))
  (when (minusp start)
    (error 'domain-error
           :argument start
           :domain "0 <= start"
           :location "PRIMES-IN-RANGE: start"))
  (unless (integerp end)
    (error 'type-error*
           :argument stop
           :type (type-of stop)
           :expected-type 'integer
           :location "PRIMES-IN-RANGE: stop"))
  (when (minusp stop)
    (error 'domain-error
           :argument stop
           :domain "0 <= stop"
           :location "PRIMES-IN-RANGE: stop"))
  (let ((range (- stop start)))
    (when (> range *soft-range-max*)
      (cerror "Proceed anyway with range ~D" range
              'large-range-error :range range))
    (cond
      ;; when STOP <= START or
      ;; STOP - START = 1 and START > 2 and START is even, there can be no primes. Return an empty
      ;; array.
      ((or (< range 1)
           (and (= range 1)
                (> start 2)
                (evenp start)))
       (make-array 1
                   :element-type 'integer
                   :adjustable t
                   :fill-pointer 0))
      ;; if stop <= *small-primes-limit*, return a slice of *small-primes*
      ((< stop *small-primes-limit*)
       (subseq *small-primes*
               (position-if #'(lambda (elt) (>= elt start)) *small-primes*)
               (1- (position-if #'(lambda (elt) (>= elt stop))))))
      ;; if stop < *plain-sieve-limit*,, perform a standard sieve and drop and initial elements that are < start
      ((< stop *plain-sieve-limit*)
       (subseq (sieve-to% stop)
               (position-if #'(lambda (elt) (>= elt start)))))
      ;; if range < *sequential-search-lower-ceiling*, search odd numbers one by one.
      ((< range *sequential-search-lower-ceiling)
       (sequential-test-range% start stop))
      ;; if start > sqrt(stop) and stop < *segmented-sieve-limit*,
      ;; perform segmented sieve
      ((and (> start (isqrt stop))
            (< stop *segmented-sieve-limit*))
       (modified-segmented-sieve% start stop))
      ;; otherwise, perform individual prime testing
      (t
       (sequential-test-range% start stop)))))


(defun sequential-test-range% (start stop)
  "Helper function for PRIMES-IN-RANGE. Given 
  integers START and STOP, returns an array of 
  the prime numbers p,  START <= p < STOP. 

  Assumes
  1. Input error and outlier checking has already been performed.
  2. STOP >= *plain-sieve-limit*
  3. Either
     a. The range (STOP - START) < *sequential-search-low-ceiling* 
     Or
     b. STOP >= *segmented-sieve-limit*

     When the range < *sequential-search-low-ceiling* or START >= *segmented-sieve-limit*, 
     the array is constructed by simply testing for 
     primes sequentially in the array. Otherwise,
     the odd numbers from *segmented-sieve-limit* to below STOP are checked
     sequentially for primality and the primes added to
     a sequential array. Then an array of primes to below *segmented-sieve-limit*
     is constructed using a segmented sieve. This is then
     concatenated to the sequential array and the concatenation
     is returned."
  (do* ((range (- stop start))
        (sequential-start (if (or (< range *sequential-range-low-ceiling*)
                                  (>= start *segmented-sieve-limit*))
                              start
                              *segmented-sieve-limit*))
        (sequential-array (make-array (estimate-prime-array-size% sequential-start stop)
                                      :element-type 'integer
                                      :adjustable t
                                      :fill-pointer 0))
        (cur-prime (next-prime (1- sequential-start) (next-prime cur-prime))))
       ((>= cur-prime stop) (if (or (< range *sequential-range-low-ceiling*)
                                    (>= start *segmented-sieve-max*))
                                sequential-array
                                (concatenate 'vector (modified-segmented-sieve% start (1+ sequential-start) sequential-array))))
    (vector-push-extend cur-prime sequential-array)))

(defun estimate-prime-array-size% (start stop)
  "Helper function. Given START and STOP, two integers, (very roughly) estimates the
   number of prime numbers p where START <= p < STOP
   Returns a positive integer. Performs minimal error checking."
  (assert (and (integerp start) (integerp stop) (<= start stop) (>= stop 3))
    ()
    "Inappropriate arguments to ESTIMATE-PRIME-ARRAY-SIZE. START: ~A ~S; STOP: ~A ~S."
    (type-of start) start (type-of stop) stop)
  (let ((adjusted-start-minus-one (if (< start 3)
                            2
                            (- start 1))))
    (let ((estimate (ceiling (- (/ stop (log stop))
                                (/ adjusted-start-minus-one (log adjusted-start-minus-one))))))
      (max estimate 1))))

(defun next-prime (x)
  "Given a real number x, return 
   the smallest prime number p, p > x.
   When x < *largest-small-prime*, searches the
   array for the prime and returns it.
    Otherwise, tests consecutive odd numbers 
   and returns the first prime found."
  (if (realp x)
      (let ((m (floor x)))
        (if (< m *largest-small-prime*)
            (find-if #'(lambda (p) (> p m)) *small-primes*)
            (do ((n (if (evenp m) (+ m 1) (+ m 2)) (+ n 2)))
                ((primep n) n))))
      (error 'type-error*
             :argument x
             :type (type-of x)
             :expected-type 'real
             :location 'next-prime)))

(defun modified-segmented-sieve% (start stop)
  (let ((segment-max (isqrt stop)))
    (let ((lower-segment
            (cond
              ((< segment-max *largest-small-prime*)
               (subseq *small-primes* (1+ (position-if #'(lambda (p)
                                                           (>= p segment-max)) *small-primes*))))
              ((= segment-max *largest-small-prime*)
               (alexandria:copy-array *small-primes*))
              (t
               (make-extended-lower-segment% segment-max))))
          (sieve (make-array (- stop start)
                               :element-type 'bit
                               :initial-element 1)))
      (serapeum:do-each (prime lower-segment)
        (do ((multiple (* (ceiling start prime) prime) (+ multiple prime)))
            ((>= multiple stop))
          (setf (bit lower-segment (- multiple start)) 0)))
      (let ((primes (make-array (estimate-prime-array-size start stop)
                      :element-type 'integer
                      :adjustable t
                      :fill-pointer 0)))
        (loop for i from start below stop
              for bit across sieve
              when (= bit 1)
              do
              (vector-push-extend i primes)
              finally
              (return primes))))))

(defun make-extended-lower-segment% (segment-max)
  "Given two integer SEGMENT-MAX, returns
   an array of the all prime numbers less than or equal
   to SEGMENT-MAX. This helper function is intended to be
   used only if SEGMENT-MAX > *largest-small-prime*"
  (let ((sieve (make-array (1+ segment-max)
          :element-type 'bit
          :initial-element 1))
        (segment-max-isqrt (isqrt segment-max)))
    (setf (bit sieve 0) 0
          (bit sieve 1) 0)
    (do ((i 4 (+ i 2)))
        ((> i segment-max))
      (setf (bit sieve i) 0))
    (do ((j 3 (+ j 2)))
        ((> j segment-max-isqrt))
      (do ((i (* j j) (+ i j)))
          ((> i segment-max))
        (setf (bit sieve i) 0)))
    (let ((segment (make-array (ceiling (/ segment-max (log segment-max)))
                     :element-type 'integer
                     :adjustable t
                     :fill-pointer 0)))
      (dotimes (i (1+ segment-max) segment)
        (when (= (bit sieve 1))
          (vector-push-extend i segment))))))
