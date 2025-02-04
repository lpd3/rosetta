;; rosetta/prime-utils.lisp
;;
;; Prime number utilities

(in-package #:prime-utils)

;;; Primality testing of individual numbers: the Baillie-PSW test

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (progn
    (defvar *small-primes-limit*
      1000
      "The largest prime in the *small-primes* array will be 
       the largest prime number less than this integer.")

    (defvar *small-primes*
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
    (unless *small-primes*
      (setf *small-primes* (init-small-primes)))))

(defparameter *largest-small-prime*
  (last-elt *small-primes*))

(defparameter *baillie-psw-limit*
  (expt 2 64)
  "The Billie-PSW primality test is a probabilistic primality test. However, there 
  are no known composite integers that are known to return a false positive. 
  It is known with certainly that there are no false baillie-psw primes
  less than 2^64 = 18,446,744,073,709,551,616 (~ 18 quintillion). Whether or not there are composite 
  numbers that pass the test is an open question in mathematics.")

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
  (when (= n e 0)
    (error 'domain-error
           :argument '(0 0)
           :location "MODULAR-EXPONENTIATION: n and e"
           :domain "when n = 0, e != 0; when e = 0, n != 0"))
  (cond
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
       (let ((isqrt (isqrt coerced-x)))
         (= coerced-x (square isqrt)))))))

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
  (let* ((xfloor (floor x))
         (n (if (= x xfloor)
                xfloor
                (1+ xfloor))))
    (primes-in-range 0 n)))

(defparameter *sequential-range-low-ceiling*
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
  "Given two real numbers, START and
   STOP, return an array of prime numbers in the
   from START below STOP. If STOP >= START, an 
   empty array is returned. When sensible arguments
   are provided, the algorithm chosen to accomplish this
   feat depends on the arguments:
   1. If range < 1 or if STOP < 2, or if range = 1 and START is even and START > 2: return an empty vector
   2. If STOP < *small-primes-limit*: return a subsequence of *small-primes*
   3. If START <= sqrt(STOP) and STOP < *plain-sieve-limit*, perform a sieve of Eratosthenes
   4. If range < *sequential-range-low-ceiling* and STOP >= *plain-sieve-limit*, test for primes sequentially
   5. If START < *segmented-sieve-limit* (we are assuming START > sqrt(STOP)), perform a segmented sieve.
   6. Otherwise: test for primes sequentially. This covers the following cases:
      6.1: STOP >= *segmented-sieve-max*
      6.2: STOP >= *plain-sieve-max* and START < sqrt(STOP).
           Note that the second will take a LONG time.
           It will hopefully be prevented by a range error as described
           next.

   Signals a correctable error if range is greater than *soft-range-limit*. 
   The user can choose to proceed anyway." 
  (unless (realp start)
    (error 'type-error*
           :argument start
           :type (type-of start)
           :expected-type 'real
           :location "PRIMES-IN-RANGE: start"))
  (unless (realp stop)
    (error 'type-error*
           :argument stop
           :type (type-of stop)
           :expected-type 'integer
           :location "PRIMES-IN-RANGE: stop"))
  (let* ((int-start (max (floor start) 0))
         (int-stop (ceiling stop))
         (range (- int-stop int-start)))
    (when (> range *soft-range-max*)
      (cerror "Proceed anyway with given range"
              'large-range-error :range range))
    (cond
      ((or (< range 1)
           ;; deals with 0 or negative ranges. The latter will occur if STOP < 0
           (< int-stop 2)
           (and (= range 1)
                (evenp int-start)
                (> int-start 2)))
       (make-prime-array%))
      ((< int-stop *small-primes-limit*)
       ;; don't re-invent the wheel. return a subsequence of *small-primes*
       (small-primes-range-subseq% int-start int-stop))
      ((and (< int-stop *plain-sieve-limit*) (<= int-start (isqrt int-stop)))
       ;; return a subsequence of an array produced by the sieve of Eratosthenes.
       (plain-sieve-subseq% int-start int-stop))
      ((and (< range *sequential-range-low-ceiling*)
            (>= int-stop *plain-sieve-limit*))
       ;; test for primes one-by-one
       (sequential-prime-range% int-start int-stop))
      ((< int-stop *segmented-sieve-limit*)
       ;; perform a segmented sieve
       (segmented-sieve int-start int-stop))
      (t
       ;; test for primes one-by-one
       (sequential-prime-range% int-start int-stop)))))

(defun make-prime-array% (&optional (start 0) (stop 0))
  "Helper function. Returns an empty, adjustable
   integer vector. Without args, the size will be 1.
   Takes two optional args START and STOP, which must
   be integers and both default to 0.
   The size of the array returned will be at least 
   1 and will otherwise be approximately the size 
   required to hold the expected number of prime 
   numbers to be contained."
  (assert
   (and (integerp start)
        (integerp stop)))
  (let ((size (max 1 (estimate-prime-array-size% start stop))))
   (make-array size
               :element-type 'integer
               :adjustable t
               :fill-pointer 0)))

(defun estimate-prime-array-size% (start stop)
  "Given two integers START and STOP, return a non-negative integer
   that is a rough estimate of the prime numbers n: 
   START <= n < STOP"
  (assert (and (integerp start)
               (integerp stop)))
  (let* ((stop-1 (1- stop))
         (range (- stop-1 start)))
    (cond
      ((or (< range 1)
           (and (= range 1)
                (evenp start)
                (> start 2)))
       0)
      ((<= start 2)
       (estimate-prime-count stop-1))
      (t
       (round (- (estimate-prime-count stop-1)
                 (estimate-prime-count start)))))))

(defun estimate-prime-count (x)
  "Given real number N, return a rough estimate of 
   the number of prime numbers p: p <= x.
   If x < *small-primes-limit*, the result will be exact."
  (when (not (realp x))
    (error 'type-error*
           :argument x
           :type (type-of x)
           :expected-type 'real
           :location 'estimate-prime-count))
  (let ((n (floor x)))
    (cond
      ((< n 2) 0)
      ((< n *largest-small-prime*)
       (length (serapeum:take-while #'(lambda (p) (< p n)) *small-primes*)))
      ((< n *small-primes-limit*)
       (length *small-primes*))
      (t
       ;; although the common elementary technique is
       ;; based on some kind of rounding of x/ln x, the
       ;; following is more accurate. I do not try to employ
       ;; the li function, as I think this to be overkill in
       ;; this context.
       (let ((ln-n (log n)))
         (round (+ (/ n ln-n) (/ n (expt ln-n 2)) (/ (* 2 n) (expt ln-n 3)))))))))

(defun small-primes-range-subseq% (start stop)
  "Helper function. Given START and STOP, returns a
   vector of prime numbers p: START <= p < stop.
   Performs no error checking. If STOP > *small-primes-limit*, 
   the result will be incorrect."
  (real-val-subseq *small-primes* start stop))

(defun real-val-subseq (seq start &optional stop)
  "Given sorted sequence of real numbers SEQ and 
   real numbers START and optional STOP, returns a new 
   sorted sequence of the same type containing elements
   x of SEQ. If STOP is not provided, returns a tail of 
   SEQ such that for each x, x >= START. Otherwise,
   returns a subsequence such that for each x, 
   START <= x < STOP. 
   The results will be useless if SEQ is not 
   pre-sorted in monotonically increasing order."
  (unless (typep seq 'alexandria:proper-sequence)
    (error 'type-error*
           :argument seq
           :type (type-of seq)
           :expected-type 'proper-sequence
           :location "REAL-VAL-SUBSEQ: seq"))
  (unless (realp start)
    (error 'type-error*
           :argument start
           :type (type-of start)
           :expected-type 'real
           :location "REAL-VAL-SUBSEQ: start"))
  (when stop
    (unless (realp stop)
      (error 'type-error*
             :argument stop
             :type (type-of stop)
             :expected-type '(or null real)
             :location "REAL-VAL-SUBSEQ: stop")))
  (let ((start-index (position-if #'(lambda (x) (>= x start)) seq))
        (stop-index-or-nil
          (when stop
            (position-if #'(lambda (x) (>= x stop)) seq))))
    (cond
      ((not start-index)
       (subseq seq 0 0))
      ((and stop-index-or-nil
            (>= start-index stop-index-or-nil))
       (subseq seq 0 0))
      (t
       (subseq seq start-index stop-index-or-nil)))))

(defun plain-sieve-subseq% (start stop)
  "Helper function. Given non-negative integers"
  (let ((sieve (sieve-of-eratosthenes (1- stop))))
    (real-val-subseq sieve start)))

(defun sieve-of-eratosthenes (x)
  "Given real number x, constructs and returns an 
   adjustable array of prime numbers p such that p <= x"
  (when (not (realp x))
    (error 'type-error*
           :argument x
           :type (type-of x)
           :expected-type 'real
           :location 'sieve-of-erathosthenes))
  (let* ((n (floor x))
         (sieve (sieve-aux% n))
         (primes (make-prime-array% 0 n)))
    (dotimes (i (length sieve) primes)
      (when (plusp (bit sieve i))
        (vector-push-extend i primes)))))

(defun sieve-aux% (n)
  "Auxilliary function. Given integer N, returns 
   a little-endian bit-vector of length max(N+1, 0) For each index i 
   of the bit-vector, if i is prime, the value 
   at i will be 1, otherwise 0"
  (let ((sieve-size (max (1+ n) 0)))
    (case sieve-size
      (0 #*) ;; empty
      (1 #*0) ;; 0 is not prime
      (2 #*00) ;; 0 and 1 are not prime
      (3 #*001) ;; 0 and 1 are not prime; 2 is
      (4 #*0011) ;; ;; 0 and 1 are not prime; 2 and 3 are.
      (otherwise
       (let ((bit-sieve (make-array sieve-size
                          :element-type 'bit
                          :initial-element 1)) ;; 1: number is prime unless proven otherwise
             (sieve-max (isqrt n)))
         ;; 0 and 1 are not prime. Cross them off (by making the value at their indices 0)
         (setf (bit bit-sieve 0) 0
               (bit bit-sieve 1) 0)
         ;; 2 is prime. Leave the value at that index alone. Cross off all multiples of 2.
         (do ((i 4 (+ i 2)))
             ((> i n))
           (setf (bit bit-sieve i) 0))
         ;; All other prime numbers are odd. Consider each
         ;; odd index <= sqrt(n). When finished, return the result.
         (do ((j 3 (+ j 2)))
             ((> j sieve-max) bit-sieve)
           (when (plusp (bit bit-sieve j))
             ;; if the value at index j is 1,j is prime. Leave it alone and cross off its multiples.
             ;; if the value at index j is 0, j is not prime. Its multiples have already been
             ;; crossed off. Move on.
             (do ((i (* j j) (+ i j)))
                 ((> i n))
               (setf (bit bit-sieve i) 0))))))))) 

(defun sequential-prime-range% (start stop)
  "Helper function that returns a vector containing prime 
   numbers p, START <= p < STOP. A slow function that should
   be reserved for cases in which the time cost is preferable to other costs
   (e.g. 1. The range is small and the time cost would be less than sieving; 
         2. STOP is large, in which case the space complexity outweighs the 
            time complexity of this function.)
   Repeatedly calls next-prime to generate the vector."
  (let ((primes (make-prime-array% start stop)))
    (do ((prime (next-prime (1- start)) (next-prime prime)))
        ((>= prime stop) primes)
      (vector-push-extend prime primes))))

(defun next-prime (x)
  "Given real number X, return the smallest prime number p, p > x."
  (when (not (realp x))
    (error 'type-error*
           :argument x
           :type (type-of x)
           :expected-type 'real
           :location 'next-prime))
  (cond
    ((< x 2) 2)
    ((< x 3) 3)
    (t
     (let ((n (floor x)))
       (do ((candidate (if (evenp n) (1+ n) (+ n 2)) (+ candidate 2)))
           ((primep candidate) candidate))))))

(defun segmented-sieve (start stop)
  "Given real numbers START and STOP, 
   sqrt(STOP) < START <= STOP, return a 
   vector containing prime numbers p, 
   START <= p < stop. Employs a segmented sieve."
  (when (not (realp start))
    (error 'type-error*
           :argument start
           :type (type-of start)
           :expected-type 'real
           :location "SEGMENTED-SIEVE: start"))
  (when (not (realp stop))
    (error 'type-error*
           :argument stop
           :type (type-of stop)
           :expected-type 'real
           :location "SEGMENTED-SIEVE: stop"))
  (when (<= start (sqrt stop))
    (error 'domain-error
           :argument (list "START:" start "STOP:" stop)
           :domain "start > sqrt(stop)"
           :location "SEGMENTED-SIEVE: start"))
 
  (let* ((int-start (floor start))
         (int-stop (floor stop))
         (primes (make-prime-array% int-start int-stop)))
    (let ((range (- int-stop int-start)))
      (if (< range 1)
          primes
          (let ((sieve (segment-aux% int-start int-stop)))
            (do* ((sieve-index 0 (1+ sieve-index))
                  (n #1=(+ sieve-index int-start) #1#))
                 ((= sieve-index (length sieve)) primes)
              (when (plusp (bit sieve sieve-index))
                (vector-push-extend n primes))))))))

(defun segment-aux% (start stop)
  "Helper function for segmented-sieve.
   Constructs and returns a bitvector whose
   indices are interpreted as a range of consectutive
   integers. Let the args START and STOP be positive integers,
   START < STOP. Let n, the length of the bit vector, = 
   STOP - START - 1. Then the indices of the bit vector
   0, 1, 2, ..., n-2, n-1 represent the integers
   START, START+1, START+2, ..., STOP-2, STOP-1.
   The value at each index will be 1 if index+START
   is prime, and 0 otherwise."
  (let ((prime-basis (sieve-of-eratosthenes (isqrt stop)))
        (sieve (make-array (- stop start)
                           :element-type 'bit
                           :initial-element 1)))
    (serapeum:do-each (prime prime-basis sieve)
      (do* ((multiple (* (ceiling start prime) prime) (+ multiple prime))
            (sieve-index #1=(- multiple start) #1#))
           ((>= sieve-index (- stop start)))
        (setf (bit sieve sieve-index) 0)))))
