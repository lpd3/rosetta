(in-package :prime-utils-test)

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (progn
    (import '(prime-utils::*small-primes-limit*
              prime-utils::init-small-primes
              prime-utils::*small-primes*
              prime-utils::largest-small-prime
              prime-utils::*baillie-psw-limit*
              prime-utils::find-miller-rabin-d-s
              prime-utils::lucas-d-chooser
              prime-utils::u-v-subscript
              prime-utils::div2mod)
            :prime-utils-test)
    (defparameter *default-test*
      'prime-utils-test-suite)
    (defparameter *reload-all-p* t)))


(defmacro retest (&key (test *default-test* test-supplied-p) (reload-all-p *reload-all-p*))
  "Helper macro for facilitating recompiling systems and relaunching tests. Before running this
   macro, two dynamic (global) parameters must be placed in the packages file for the current
   testing packages. Each must contain either a keyword or string that designate an existing system.
   *source-code-system* will contain the designator for the main code system, and *test-system* 
   will contain a designator for the system of the test-code.

   When run with no args the first time after startup, RETEST queries the userfor the test to run. 
   RETEST then loads both the source-code system
   and the test-system, makes the current package the specified test package and runs the 
   designated test. The package and test are stored as defaults. On subsequent runs with no 
   args, the test-system (not the source-code system) is loaded and the previously specified test is run.

   Takes 2 keyword arguments. :test takes a quoted symbol. This test will be run and will become
   the new default; 
   :reload-all-p takes a Boolean. When true, the source-code system is loaded before and addition
   to the test-system. Returns nil."
  (when *reload-all-p* ;; only t at startup
    (unless test-supplied-p
      (format t "~&~%What test do you want to run?~%")
      (let ((first-test (uiop:with-safe-io-syntax ()
                          (read))))
        (setf *default-test* first-test
              test first-test)))
    (setf *reload-all-p* nil))
  (when test-supplied-p
    (setf *default-test* test))
  `(progn
     (when ,reload-all-p
       (ql:quickload *source-code-system*))
     (ql:quickload *test-system*)
     (parachute:test ,test)))


;; Import all symbols internal to prime-utils

(define-test prime-utils-test-suite)

(define-test useful-dependencies-suite
  :parent prime-utils-test-suite)

(define-test modular-exponentiation-errors-test
  :parent useful-dependencies-suite
  (fail
    (modular-exponentiation 1.3 10 10)
    type-error*)
  (fail
    (modular-exponentiation "five" 10 10)
    type-error*)
  (fail
    (modular-exponentiation -10 10 10)
    domain-error)
  (fail
    (modular-exponentiation 10 7.0 10)
    type-error*)
  (fail
    (modular-exponentiation 10 "ten" 10)
    type-error*)
  (fail
    (modular-exponentiation 10 -10 10)
    domain-error)
  (fail
    (modular-exponentiation 10 10 #\a)
    type-error*)
  (fail
    (modular-exponentiation 10 10 10.0)
    type-error*)
  (fail
    (modular-exponentiation 10 10 0)
    domain-error)
  (fail
    (modular-exponentiation 10 10 -10)
    domain-error)
  (fail
    (modular-exponentiation 0 0 10)
    domain-error))

(define-test modular-exponentiation-outliers
  :parent useful-dependencies-suite
  (dotimes (i 20)
    (declare (ignore i))
    (let* ((n (random 1001))
           (e (if (zerop n)
                  (1+ (random 1000))
                  (random 1001)))
           (m (+ (random 999) 2)))
      (is #'= 0 (modular-exponentiation 0 e m))
      (is #'= 1 (modular-exponentiation 1 e m))
      (is #'= 1 (modular-exponentiation n 0 m))
      (is #'= (mod n m) (modular-exponentiation n 1 m))
      (is #'= 0 (modular-exponentiation n e 1)))))

(define-test modular-exponentiation-correct-results
  :parent useful-dependencies-suite
  (is #'= 445 (modular-exponentiation 4 13 497))
  (is #'= 49 (modular-exponentiation 37 82 52))
  (is #'= 81 (modular-exponentiation 3 2019 99))
  (is #'= 436 (modular-exponentiation 7 644 645))
  (is #'= 22 (modular-exponentiation 123 1001 101))
  (is #'= 27 (modular-exponentiation 75 84 297))
  (is #'= 24 (modular-exponentiation 23 20 29))
  (is #'= 12 (modular-exponentiation 23 391 55))
  (is #'= 26 (modular-exponentiation 31 397 55)))

(define-test jacobi-errors
  :parent useful-dependencies-suite
  (fail (jacobi 67.0 7) type-error*)
  (fail (jacobi '(1) 9) type-error*)
  (fail (jacobi 4 "seven") type-error*)
  (fail (jacobi -3 9.0) type-error*)
  (fail (jacobi 5 0) domain-error)
  (fail (jacobi 5 -3) domain-error)
  (fail (jacobi 5 8) domain-error))

(define-test jacobi-outliers
  :parent useful-dependencies-suite
  ;; The Jacobi symbol (k/1) = 1, k >= 0
  ;; The Jacobi symbol (1/n) = 1, n > 0, n is odd
  (dotimes (i 20)
    (declare (ignore i)
      (let ((k (random 1000))
            (n (1- (* (random 500) 2))))
        (is #'= 1 (jacobi k 1))
        (is #'= 1 (jacobi 1 n))))))

(define-test jacobi-correct-results
  :parent useful-dependencies-suite
  (dolist (args '((17 27)
                  (-23 33)
                  (3 43)))
    (is #'= -1 (apply #'jacobi args)))
  (dolist (args '((-3 33)
                  (25 5)
                  (-14 49)))
    (is #'= 0 (apply #'jacobi args)))
  (dolist (args '((26 37)
                  (-14 5)
                  (13 35)))
    (is #'= 1 (apply #'jacobi args))))

(define-test square-test
  :parent useful-dependencies-suite
  (fail (square "five") type-error*)
  (fail (square #\a) type-error*)
  (finish (square 45.9865))
  (is #'= 1/16 (square 1/4))
  (is #'= #c(16 -30) (square #c(-5 3))))

(define-test perfect-square-p-test
  :parent useful-dependencies-suite
  (fail (perfect-square-p "nine") type-error*)
  (fail (perfect-square-p #\b) type-error*)

  (false (perfect-square-p 0.25))
  (false (perfect-square-p 1/9))
  (false (perfect-square-p #c(9 9)))
  (false (perfect-square-p -25))

  (true (perfect-square-p 0))

  (dotimes (i 10)
    (let ((s (square (1+ (random 1000)))))
      (true (perfect-square-p s))
      (true (perfect-square-p (float s 0L0)))
      (false (perfect-square-p (1+ s))))))

(define-test binary-expansion-test
  :parent useful-dependencies-suite
  (fail (binary-expansion 3.0) type-error*)
  (fail (binary-expansion "one hundred") type-error*)
  (fail (binary-expansion -10) domain-error)

  (is #'equalp #(0) (binary-expansion 0))
  (is #'equalp #(1) (binary-expansion 1))

  (dotimes (i 20)
    (let* ((n (random 1000000))
           (be (binary-expansion n))
           (le (binary-expansion n t)))
      (cond
        ((< n 2)
         (is #'= 1 (length be))
         (is #'= 1 (length le)))
        (t
         (let ((expected-length (1+ (floor (log n 2)))))
           (is #'= expected-length (length be))
           (is #'= expected-length (length le)))))
      (when (> n 2)
        (loop for bit across be
              for multiplier = (expt 2 (1- (length be))) then (/ multiplier 2)
              for value = multiplier then (+ value (* multiplier bit))

              finally
              (is #'= n value "n: ~D value: ~D" n value))
        (loop for bit across le
              for multiplier = 1 then (* multiplier 2)
              for value = bit then (+ value (* multiplier bit))

              finally
              (is #'= n value "n: ~D value: ~D" n value))))))

(define-test helper-function-suite
  :parent prime-utils-test-suite)

(define-test lucas-d-chooser-test
  :parent helper-function-suite

  (dotimes (i 20)
    ;; n must be odd. It will also be greater than 1, but that need not concern us.
    (let ((n (1+ (* 2 (random 100000000000000)))))
      (finish (lucas-d-chooser n)))

    ;; The first value returned k will be an integer
    ;; either k = 0, or
    ;;  5, -7, 9, -11, ...  

    (let* ((n (1+ (* 2 (random 1000000)))))
      (multiple-value-bind
            (result still-probable-p)
            (lucas-d-chooser n)
        (true (or (and (zerop result) (not still-probable-p))
                  (oddp result))))))
  ;; Finally, we check some of the values to make sure they are appropriate. The keys are n's. The values are the d's that should be chosen.
  (let ((test-cases
          '((5238929 -7)
            (76825821 0)
            (25540951 -11)
            (92205009 0)
            (83976687 5)
            (31069973 5)
            (93788473 5)
            (2085731 -11)
            (72280755 0)
            (67214257 5)
            ;; 1009*1009
            (1018081 0))))
    (dolist (test-case test-cases)
      (destructuring-bind
          (n d)
          test-case
        (is #'= d (lucas-d-chooser n)
            "n: ~D" n)))))

(define-test find-miller-rabin-d-s-test
  :parent helper-function-suite
  ;; n is always odd and positive
  (dotimes (i 100)
    (let ((n (1+ (* 2 (random 500000)))))
      (multiple-value-bind
            (d s)
            (find-miller-rabin-d-s n)
        (is #'= (1- n) (* (expt 2 s) d) "~D ?= 2^~D [~D] * ~D" n s (expt 2 s) d)
        (true (oddp d) "~D odd?" d)))))

(define-test div2mod-test
  :parent helper-function-suite
  (do ((i 0 (1+ i))
       (x #1=(random 1000000) #1#)
       (n #2=(+ 3 (* 2 (random 400000))) #2#))
      ((= i 25))
    (let ((result
            (if (oddp x)
                (mod (/ (+ x n) 2) n)
                (mod (/ x 2) n))))
      (is #'= result (div2mod x n)))))

(define-test miller-rabin-suite
  :parent prime-utils-test-suite)

(define-test miller-rabin-errors
  :parent miller-rabin-suite
  (fail (miller-rabin 8) 'domain-error)
  (fail (miller-rabin 3) 'domain-error)
  (fail (miller-rabin 199 1) 'domain-error)
  (fail (miller-rabin 199 198) 'domain-error)

  (finish (miller-rabin 5))
  (finish (miller-rabin 199 197)))


(define-test miller-rabin-test-correct-results
  :parent miller-rabin-suite
  ;; make sure it doesn't crash with random sensical n and legal base.
  (dotimes (i 20)
    (let* ((n (+ 1001 (* 2 (random 1000000000000))))
           (base (+ 2 (random (1- n)))))
      (finish (miller-rabin n base))))
  ;; The smallest strong pseudoprime to base 2 is 2047. Thus, for integers from 5
  ;; through *small-prime-limit*, the Miller-Rabin test should correctly report t
  ;; for primes and nil for odd composites.
  (loop for n from 5 to *small-primes-limit* by 2
        for primep = (loop for p across *small-primes*
                           when (>= p n)
                             do (return (= n p))
                           finally (return nil))
        do
        (is #'eq primep (miller-rabin n) "~D: in *small-primes*: ~A" n primep))
  ;; Next, we check out around some strong pseudoprimes
  ;; 8321 is a strong pseudoprime base 2. In the interval 8300 to
  ;; 8350, there are 3 primes: 8311, 8317 and 8329. Miller-Rabin, when run with
  ;; base 2, should return T for these 3 and the pseudoprime, and nil
  ;; for all other odd numbers in the interval
  (loop for n from 8301 to 8350 by 2
        if (member n '(8311 8317 8329 8321))
          do
             (true (miller-rabin n) "~D should be T" n)
        else
        do
        (false (miller-rabin n) "~D should be NIL" n))
  ;; Try again: range: 80550-80600
  ;;      pseudoprime: 80581
  ;;      primes: 80557, 80567, 80599
  (loop for n from 80551 to 80599 by 2
        if (member n '(80557 80567 80599 80581))
        do
        (true (miller-rabin n) "~D should be T" n)
        else
        do
        (false (miller-rabin n) "-D should be NIL" n))
  ;; One more: range: 90750 to 90800
  ;;     pseudoprime: 90751
  ;;          primes: 90787, 90793
  (loop for n from 90751 to 90800 by 2
        if (member n '(90787 90793 90751))
        do
        (true (miller-rabin n) "~D should be T" n)
        else
        do
        (false (miller-rabin n) "~D should be NIL" n))) 

