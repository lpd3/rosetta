(in-package :prime-utils-test)

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  ;; import internal symbols for testing (external symbols already imported)
  (progn
    (import '(prime-utils::*small-primes-limit*
              prime-utils::*largest-small-prime*
              prime-utils::init-small-primes
              prime-utils::*small-primes*
              prime-utils::*baillie-psw-limit*
              prime-utils::find-miller-rabin-d-s
              prime-utils::lucas-d-chooser
              prime-utils::u-v-subscript
              prime-utils::div2mod
              prime-utils::make-prime-array%
              prime-utils::estimate-prime-array-size%
              prime-utils::sieve-aux%
              prime-utils::segment-aux%)
            :prime-utils-test)
    ;; global variables for the retest macro
    (defvar *source-code-system*
      :rosetta)
    (defvar *test-system*
      :rosetta/tests)
    (defvar *test-package*
      :prime-utils-test)
    (defvar *default-test*
      'prime-utils-test-suite)
    (defvar *first-run-of-session-p*
      t)))

;; retest macro. quickly recompile, reload and retest.

(defmacro retest (&key (test *default-test*) reload-all-p)
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
  (alexandria:with-gensyms (greload-all-p gtest)
    `(let ((,greload-all-p (or ,*first-run-of-session-p* ,reload-all-p))
           (,gtest (or ',test ',*default-test*)))
       (when ,greload-all-p
         (ql:quickload ,*source-code-system*))
       (ql:quickload ,*test-system*)
       (in-package ,*test-package*)
       (setf *first-run-of-session-p* nil
             *default-test* ,gtest)
       (parachute:test ,gtest))))

(define-test prime-utils-test-suite)

;; general utilities

(define-test utilities-suite
  :parent prime-utils-test-suite)

(define-test modular-exponentiation-errors-test
  :parent utilities-suite
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
  :parent utilities-suite
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
  :parent utilities-suite
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
  :parent utilities-suite
  (fail (jacobi 67.0 7) type-error*)
  (fail (jacobi '(1) 9) type-error*)
  (fail (jacobi 4 "seven") type-error*)
  (fail (jacobi -3 9.0) type-error*)
  (fail (jacobi 5 0) domain-error)
  (fail (jacobi 5 -3) domain-error)
  (fail (jacobi 5 8) domain-error))

(define-test jacobi-outliers
  :parent utilities-suite
  ;; The Jacobi symbol (k/1) = 1, k >= 0
  ;; The Jacobi symbol (1/n) = 1, n > 0, n is odd
  (dotimes (i 20)
    (let ((k (random 1000))
          (n (1- (* (random 500) 2))))
      (is #'= 1 (jacobi k 1))
      (is #'= 1 (jacobi 1 n)))))

(define-test jacobi-correct-results
  :parent utilities-suite
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
  :parent utilities-suite
  (fail (square "five") type-error*)
  (fail (square #\a) type-error*)
  (finish (square 45.9865))
  (is #'= 1/16 (square 1/4))
  (is #'= #c(16 -30) (square #c(-5 3))))

(define-test perfect-square-p-test
  :parent utilities-suite
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
  :parent utilities-suite
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

(define-test estimate-prime-count-errors-and-outliers
  :parent utilities-suite
  (fail (estimate-prime-count "five hundred") 'type-error* "should fail with arg five hundred [string]")
  (fail (estimate-prime-count #C(100 1)) 'type-error* "should fail with arg #C(100 100) [complex number]")
  (fail (estimate-prime-count #C(100 0.0)) 'type-error* "should fail. although #C(1 0.0) is actually ~
 real, CL does not coerce it because the imaginary part is a float.")
  (finish (estimate-prime-count #C(100 0)) "with arg #C(100 0), this should not signal, since CL will ~
 coerce it to a real.")
  (dolist (x '(-100000 -600.4326 0.0 1.9 1))
    (is #'eql 0 (estimate-prime-count x) "x: ~A; result should eql 0" x))
  (dotimes (_ 10)
    (let* ((n (serapeum:random-in-range 2 *largest-small-prime*))
           (x (+ n 0.9999))
           (prime-count (length (serapeum:take-while #'(lambda (p) (< p n)) *small-primes*)))
           (n-estimate (estimate-prime-count n))
           (x-estimate (estimate-prime-count x)))
      (is #'= prime-count n-estimate
          "n-estimate ~D should = prime-count ~D" n-estimate prime-count)
      (is #'eql prime-count x-estimate
          "x-estimate ~S should eql prime-count ~D" x-estimate prime-count)))
  (do* ((n *largest-small-prime* (1+ n))
        (x #1=(+ n 0.999) #1#)
        (prime-count (length *small-primes*)))
       ((= n *small-primes-limit*))
    (let* ((n-estimate (estimate-prime-count n))
           (x-estimate (estimate-prime-count x)))
      (is #'= n-estimate prime-count
          "n-estimate ~D should = prime-count ~D" n-estimate prime-count)
      (is #'eql x-estimate prime-count
          "x-estimate ~S should eql prime-count ~D" x-estimate prime-count))))

;; once we get to *small-primes-limit*, the integers returned by
;; estimate-prime-count are estimates. A cursory
;; examination of pi(n) for powers of 10 compared with the results
;; for this function showed that the estimates were accurate to the
;; first two digits only. This is actually better than expected.

(defparameter *prime-count-error-ratio*
  1/30
  "Given a result r  returned by (estimate-prime-count n),
   and the exact pi(n) pc, let e = *prime-count-error-ratio* * r.
   Then abs(r - pc) < e. ")

(defun prime-count-half-margin-of-error (pi-of-n)
  "Given pi-of-n the exact number of primes <= some 
   number n, return an integer which is half the size
   of the expected margin of error for an estimate of 
   pi(n)."
  (round (* pi-of-n *prime-count-error-ratio*)))

(defparameter *prime-count-test-cases*
  '((1000 . 168)
    (10000 . 1229)
    (100000 . 9592)
    (1000000 . 78498)
    (10000000 . 664579)
    (100000000 . 5761455)
    (1000000000 . 50847534)
    (10000000000 . 455052511)
    (100000000000 . 4118054813)
    (1000000000000 . 37607912018)
    (10000000000000 . 346065536839)
    (100000000000000 . 3204941750802)
    (1000000000000000 . 29844570422669))
  "Test cases for the estimating functions.
  An association list with keys integers n, all 
  powers of 10, and values the result of 
  pi(n), that is, the number of prime numbers p
  such that each p <= n.")

(define-test estimate-prime-count-reasonable-answers
  :parent utilities-suite
  (do* ((rest-alist *prime-count-test-cases* (rest rest-alist))
        (prime-count-entry #1=(first rest-alist) #1#))
       ((null prime-count-entry))
    (destructuring-bind
        (n . pi-of-n)
        prime-count-entry
      (let* ((estimate (estimate-prime-count n))
             (margin (prime-count-half-margin-of-error pi-of-n))
             (lower-bound (- pi-of-n margin))
             (upper-bound (+ pi-of-n margin)))
        (true (< lower-bound estimate upper-bound)
              "For n = ~D (~E), pi(n) = ~D and estimate = ~D. pi(n) expected to fall within ~
                 range (~D, ~D)" n (float n) pi-of-n estimate lower-bound upper-bound)))))

(define-test real-val-subseq-test
  :parent utilities-suite
  (fail (real-val-subseq "zero" 100) 'type-error*)
  (fail (real-val-subseq #C(-6 5) 100) 'type-error*)
  (fail (real-val-subseq 3 #\9) 'type-error*)
  (fail (real-val-subseq 3 #C(9 0.0)) 'type-error*)
  (let ((trial-1 nil))
    (do ((i 1 (+ i 2)))
        ((> i 100) (setf trial-1 (nreverse trial-1)))
      (push i trial-1))
    (is #'equal '(33 35 37 39) (real-val-subseq trial-1 32 40))
    (is #'equal '(1 3) (real-val-subseq trial-1 0.5 4.5))
    (is #'equal '(1 3) (rea1-val-subseq trial=1 -1000000 4))
    (is #'equal '(95 97 99) (real-val-subseq trial-1 95))
    (is #'equal trial-1 (real-val-subseq trial-1 -1))
    (is #'equal () (real-val-subseq trial-1 101 101))
    (is #'equal () (real-val-subseq trial-1 75 5))))

;;; primailty testing tests
(define-test primality-testing-test-suite
  :parent prime-utils-test-suite)

(define-test helper-function-suite
  :parent primality-testing-test-suite)

(define-test lucas-d-chooser-test
  :parent helper-function-suite
  :depends-on (jacobi-errors
               jacobi-outliers
               jacobi-correct-results
               perfect-square-p-test)

  (dotimes (i 20)
    ;; n must be odd. It will also be greater than 1, but that need not concern us.
    (let ((n (1+ (* 2 (random 100000000000000)))))
      (finish (lucas-d-chooser n)))

    ;; The function returns an odd integer or nil  

    (let* ((n (1+ (* 2 (random 1000000))))
           (d (lucas-d-chooser n)))
      (when d
        (true (oddp d) "d: ~D. Should be odd or nil." d))))
  ;; Finally, we check some of the values to make sure they are appropriate. The keys are n's. The values are the d's that should be chosen.
  (let ((test-cases
          '((5238929 -7)
            (76825821 nil)
            (25540951 -11)
            (92205009 nil)
            (83976687 5)
            (31069973 5)
            (93788473 5)
            (2085731 -11)
            (72280755 nil)
            (67214257 5)
            ;; 1009*1009
            (1018081 nil))))
    (dolist (test-case test-cases)
      (destructuring-bind
          (n d)
          test-case
        (is #'eql d (lucas-d-chooser n)
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
  :parent primality-testing-test-suite
  :depends-on (find-miller-rabin-d-s-test
               modular-exponentiation-errors-test
               modular-exponentiation-outliers
               modular-exponentiation-correct-results))

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

(define-test lucas-suite
  :parent primality-testing-test-suite
  :depends-on (lucas-d-chooser-test
               binary-expansion-test
               div2mod-test))

(define-test lucas-errors
  :parent lucas-suite
  (fail (lucas "two hundred one" 3 1) type-error* "n: two hundred one [string] (expected integer)")
  (fail (lucas 13.0 3 1) type-error* "n: 5.0 (expected integer)")
  (fail (lucas 1 3 1) domain-error "n: 0 (domain: n > 1, n odd)") 
  (fail (lucas -7 3 1) domain-error "n: -7 (domain: n > 1, n odd")
  (fail (lucas 14 3 1) domain-error "n: 8 (domain: n > 1, n odd")
  (fail (lucas 103 #\5 1) type-error* "d: #\5 (expected integer)")
  (fail (lucas 103 5.0 1) type-error* "d: 5.0 (expected integer)")
  (fail (lucas 103 5 "one") type-error* "p: one [string] (expected integer)")
  (fail (lucas 103 5 1.0) type-error* "p: 1.0 (expected integer)")
  (fail (lucas 103 5 0) domain-error "p: 0 (domain: p > 0)")
  (fail (lucas 103 5 -7) domain-error "p: -7 (domain: p > 0)"))

(define-test lucas-correct-answers
  :parent lucas-suite
  "The Lucas pseudoprimes vary depending on which 
   d and p are chosen. Following the Baillie-PSW
   scheme for determining these parameters, the smallest 
   Lucas pseudoprimes are 323 and 377. All other Lucas 
   pseudoprimes that arise from the Baillie-PSW scheme
   are greater than *small-prime-limit*. Thus, all primes
   in *small-primes*, as well as 323 and 377 should 
   return T. All others should return NIL."
  (do* ((n 15 (+ n 2))
         ;; The lucas-d-chooser might not choose a suitable d for a small prime. The small primes
         ;; will have been already checked at the beginning of primep.
        (d #1=(lucas-d-chooser n) #1#))
       ((>= n *small-primes-limit*))
     (if d
         (if (or (member n '(323 377))
                 (find n *small-primes*))
             (true (lucas n d 1) "~D should be probable prime. d: ~D." n d)
             (false (lucas n d 1) "~D should be composite. d: ~D." n d))
         (false (find n *small-primes*) "~D should be composite. d: ~S" n d)))
  ;; Now we testin the vicinity of three larger pseudoprimes, using the same
  ;; procedure we used when testing miller-rabin.

  ;; 1159 is a pseudoprime. In the range 1150 to 1200
  ;; there are 7 true primes:
  ;; 1151, 1153, 1163, 1171, 1181, 1187, 1193
  (do* ((n 1151 (+ n 2))
        (d #2=(lucas-d-chooser n) #2#))
       ((> n 1199))
    (if d
      (if (member n '(1151 1153 1163 1171 1181 1187 1193 1159))
          (true (lucas n d 1) "~D is probable prime. d: ~D" n d)
          (false (lucas n d 1) "~D is composite. d: ~D" n d))
      (false (member n '(1151 1153 1163 1171 1181 1187 1193)) "~D is composite. d: ~S" n d)))
  ;; pseudoprime: 3827
  ;; range: 3800 to 3850
  ;; true primes: 3803, 3821, 3823, 3833, 3847
  (do* ((n 3801 (+ n 2))
        (d #2# #2#))
       ((> n 3849))
    (if d
      (if (member n '(3803 3821 3823 3833 3847 3827))
          (true (lucas n d 1) "~D is probable prime. d: ~D" n d)
          (false (lucas n d 1) "~D is composite. d: ~D" n d))
      (false (member n '(3803 3821 3823 3833 3847)) "~D is composite. d: ~S" d n)))
  ;; pseudoprime: 10877
  ;; range: 10850 to 10900
  ;; true primes: 10853, 10859, 10861, 10867, 10883, 10889, 10891
  (do* ((n 10851 (+ n 2))
        (d #2# #2#))
       ((> n 10899))
    (if d
      (if (member n '(10853 10859 10861 10867 10883 10889 10891 10877))
          (true  (lucas n d 1) "~D is probable prime. d: ~D" n d)
          (false (lucas n d 1) "~D is composite. d: ~D" n d))
      (false (member n '(10853 10859 10861 10867 10883 10889 10891)) "~D is composite. d: ~S" n d))))

(define-test baillie-psw-suite
  :parent primality-testing-test-suite
  :depends-on (miller-rabin-suite
               lucas-suite
               primes-below-x-suite))

;; The following tests were cribbed from https://github.com/armchaircaver/Baillie-PSW/blob/main/baillie%20PSW%20test%20suite.py

;; Tests large Carmichael numbers (composite)
(define-test baillie-psw-vs-carmichaels
  :parent baillie-psw-suite
  (dolist (c '(79397009999 63278892599 2013745337604001 894221105778001 
          582920080863121 443372888629441 28295303263921 443372888629441))
    (false (baillie-psw c) "Carmichael number ~D" c)))


;; OEIS sequence A217255: Strong Lucas Pseudoprimes
(define-test baillie-psw-vs-strong-lucas
  :parent baillie-psw-suite
  (dolist (slpsp '(5459 5777 10877 16109 18971 22499 24569 25199 40309 58519 75077 97439 100127 113573
                   115639 130139 155819 158399 161027 162133 176399 176471 189419 192509 197801 224369
                   230691 231703 243629 253259 268349 288919 313499 324899))
    (false (baillie-psw slpsp) "Strong Lucas Pseudoprime ~D" slpsp)))

;; OEIS sequence A217719: Extra Strong Lucas Pseudoprimes.
  
(define-test baillie-psw-vs-extra-strong-lucas
  :parent baillie-psw-suite
  (dolist (xslpsp '(3239 5777 10877 27971 29681 30739 31631 39059 72389 73919 75077 100127 113573
                    125249 137549 137801 153931 155819 161027 162133 189419 218321 231703 249331
                    370229 429479 430127 459191 473891 480689 600059 621781 632249 635627))
    (false (baillie-psw xslpsp) "Extra-strong Lucas Pseudoprime ~D" xslpsp)))

;; OEIS A072276: Strong pseudoprimes to bases 2 and 3

(define-test baillie-psw-vs-strong-2-3
  :parent baillie-psw-suite
  (dolist (spsp2-3
           '(1373653 1530787 1987021 2284453 3116107 5173601 6787327 11541307 13694761
             15978007 16070429 16879501 25326001 27509653 27664033 28527049 54029741
             61832377 66096253 74927161 80375707 101649241))
    (false (baillie-psw spsp2-3) "~D a strong pseudoprime to bases 2 and 3" spsp2-3)))

;; small perfect squares

(define-test baillie-psw-vs-small-squares
  :parent baillie-psw-suite
  (do* ((i 33 (+ i 2)) ; 33: ceiling of sqrt of 1001
        (ss (* i i) (* i i)))
       ((> i 10000))
    (false (baillie-psw ss) "~D (~D^2)" ss i)))

;; Large perfect squares

(define-test baillie-psw-vs-large-squares
  :parent baillie-psw-suite
  (do* ((i 0 (1+ i))
        (n (expt 11 60) (+ 2 n))
        (ls (* n n) (* n n)))
       ((= i 2000))
    (false (baillie-psw ls) "~D (~D^2)" ls n)))

(define-test baillie-psw-identifies-primes
  :parent baillie-psw-suite
  (loop for prime across (primes-below-x (expt 10 6))
        when (> prime *small-primes-limit*)
          do
        (true (baillie-psw prime) "n: ~D is prime but baillie-psw said no." prime)))

(define-test baillie-psw-identifies-larger-primes
  :parent baillie-psw-suite
  (let ((primes1 '(1234567891 1234567907 1234567913 1234567927 1234567949 1234567967 1234567981))
        (range1 '(1234567891 1234567999))
        (primes2 '(56789012359 56789012389 56789012393 56789012417 56789012471))
        (range2 '(56789012345 56789012499))
        (primes3 '(777777777841 777777777869 777777777877 777777777901 777777777911))
        (range3 '(777777777777 777777777913))
        (primes4 '(91234567890123456907 91234567890123456937 91234567890123457027))
        (range4 '(91234567890123456789 91234567890123457029)))
    (let ((primelists (list primes1 primes2 primes3 primes4))
          (ranges (list range1 range2 range3 range4)))
      (do ((rest-ranges ranges (rest rest-ranges))
           (rest-primelists primelists (rest rest-primelists)))
          ((null rest-ranges))
        (let ((current-range (first rest-ranges))
              (current-primelist (first rest-primelists)))
          (destructuring-bind
              (start end)
              current-range
            (do ((n start (+ n 2)))
                ((>= n end))
              (if (member n current-primelist)
                  (true (baillie-psw n) "n: ~D is prime" n)
                  (false (baillie-psw n) "n: ~D is composite" n)))))))))

(defun zero-is-false (x)
  "Given any kind of object, returns a generalized Lisp
   boolean equivalent to the object's boolean value in C.
   In C, the values 0 and 0.0 are falsy and all other 
   values and objects are truthy. If x is a zero, returns 
   NIL, otherwise returns T."
  ;; equalp is the laxest general equality operator.
  ;; When given two or more numbers, it compares
  ;; them based on 'visual' equality, without regard to type.
  ;; Thus 0, 0.0, and #C(0.0 0.0) (i.e. 0.0+0.0i)  are all
  ;; equalp. Note also that 0/1 and #C(0 0) are automatically coerced to 0
  ;; and both #C(0 0.0) and #C(0.0 0) are automatically coerced to #C(0.0 0.0)
  (if (equalp x 0)
      nil
      t))

(define-test primep-suite
  :parent primality-testing-test-suite
  :depends-on (baillie-psw-suite))

(define-test primep-errors-and-outliers
  :parent primep-suite
  (fail (primep "balogna sandwich") type-error* "n: bologna sandwich [string]")
  (fail (primep #\7) type-error* "n: #\7 [character]")
  (true (primep 11.0) "n: 11.0 is prime")
  (false (primep 11.1) "n: 11.1 is not prime")
  ;; When a ratio type (i.e. fraction) is integral, Lisp silently converts it
  ;; to the integer type
  (false (primep 35/36) "n: 35/36 is not prime") 
  (false (primep -13) "n: -13 is not prime")
  ;; In numbers of the complex type, when both parts are of the integer
  ;; type and the imaginary part is 0, Lisp silently converts the numbers
  ;; to the integer type.
  (true (primep #C(11.0 0.0)) "n: 11.0+0.0i is prime") 
  (false (primep #C(11 5)) "n 11+5i is not prime."))

(define-test primep-small-numbers
  :parent primep-suite
  (dotimes (n (1+ *small-primes-limit*))
    (let ((in-small-primes-p (sorted-find n *small-primes*))
          (primep-prime-p (primep n)))
      (true
       (or
        (and in-small-primes-p primep-prime-p)
        (and (not in-small-primes-p) (not primep-prime-p)))
       "n: ~D; found in *small-primes*: ~A; (primep n): ~A" n in-small-primes-p primep-prime-p))))

(defun sorted-find (x seq &key (test #'eql) (stop #'>))
  "Similar to 'find', but assumes that items in the sequence are
   sorted. Takes X (object) SEQ (sequence) and kwargs TEST (defaults to
   #'EQL) and STOP (defaults to #'>). Returns two values Sequentially compares elements in SEQ
   to X. If (TEST item X) returns non-nil, returns item and T. If (STOP item X)
   returns T, or if SEQ is exhausted, returns NIL and NIL. The second return value is useful to distinguish
   between success (when the returned item is itself NIL) and failure (in all cases). "
  (dotimes (i (length seq) (values nil nil))
    (let ((item (elt seq i)))
      (when (funcall test item x)
        (return (values item t)))
      (when (funcall stop item x)
        (return (values nil nil))))))

(defparameter *pln-limit*
  (truncate 5e6)
  "PRIMEP-LARGER-NUMBERS will test random positive integers
  up through this number.")

(defparameter *pln-runs*
  10000
  "Number of random integers that 
  PRIMEP-LARGER-NUMBERS tests.")

(define-test primep-larger-numbers
  :parent primep-suite
  (let* ((primes (primes-below-x (1+ *pln-limit*))))
    (dotimes (i *pln-runs*)
      (let ((n (random (1+ *pln-limit*))))
        (if (binary-search n primes #'= #'<)
            (true (primep n) "n: ~D in primes list" n)
            (false (primep n) "n: ~D not in primes list" n))))))

;;; generation of prime numbers

(define-test prime-generation-test-suite
  :parent prime-utils-test-suite)

(define-test prime-gen-helpers-test-suite
  :parent prime-generation-test-suite)

(define-test estimate-prime-array-size%-errors-and-outliers
  :parent prime-gen-helpers-test-suite
  :depends-on (estimate-prime-count-errors-and-outliers)
  (fail (estimate-prime-array-size% '|one million one| 1234567899) 'error "A START arg of one million one [symbol] should fail.")
  (fail (estimate-prime-array-size% 1000001.00 123456789) 'error "A START of 1000001.00 should fail.")
  (fail (estimate-prime-array-size% 3 #\8) 'error "A STOP value of #\8 should fail.")
  (fail (estimate-prime-array-size% 3 124567.00) 'error "A STOP value of 123467.00 should fail.")
  (dolist (test-case '((12345 12345)
                       (12345 35)
                       (1000 1001)
                       (12345 -12345)
                       (-12345 1)
                       (-12345 -12345)))
    (let ((estimate (apply #'estimate-prime-array-size% test-case)))
      (true (zerop estimate) "Args: ~A" test-case))))

(defparameter *epas-test-runs*
  50
  "The number of times to run estimate-prime-array-size%-reasonable-results")

(define-test estimate-prime-array-size%-reasonable-results
  :parent prime-gen-helpers-test-suite
  :depends-on (estimate-prime-count-reasonable-answers
               estimate-prime-array-size%-errors-and-outliers)
  (dotimes (i *epas-test-runs*)
    (let ((case-1 #1=(alexandria:random-elt *prime-count-test-cases*))
          (case-2 #1#))
      (destructuring-bind
          (start . pi-of-start)
          case-1
        (destructuring-bind
            (stop . pi-of-stop)
            case-2
          (let ((estimate (estimate-prime-array-size% start stop)))
            (if (< start stop)
                (let* ((pi-of-n (- pi-of-stop pi-of-start))
                       (margin (prime-count-half-margin-of-error pi-of-n))
                       (lower-bound (- pi-of-n margin))
                       (upper-bound (+ pi-of-n margin)))
                  (true (< lower-bound  estimate upper-bound)
                        "start: ~D (~E); stop: ~D (~E); pi(n): ~D estimate: ~D; range: (~D, ~D)"
                        start (float start) stop (float stop)  pi-of-n estimate lower-bound upper-bound))
                (true (zerop estimate) "Start: ~D (~E); Stop: ~D (~E); estimate: ~D"
                      start (float start) stop (float stop) estimate))))))))

(define-test make-prime-array%-errors-and-outliers
  :parent prime-gen-helpers-test-suite
  :depends-on (estimate-prime-array-size%-reasonable-results)
  (fail (make-prime-array% #\8 2) 'error "start: #\8")
  (fail (make-prime-array% 200.00 100) 'error "start: 200.00")
  (fail (make-prime-array% 1234 "one hundred") 'error "stop: one hundred [string]")
  (fail (make-prime-array% 1234 100.12) 'error "stop: 100.12")

  (let ((test-cases '((1234 1234) (1234 100) (-1234 -5))))
    (dolist (test-case test-cases)
      (let ((array (apply #'make-prime-array% test-case)))
        (is #'equalp #() array "args: ~S" test-case)
        (is #'= 1 (array-dimension array 0) "args: ~S" test-case)
        (true (adjustable-array-p array) "args: ~S" test-case)))))

(defparameter *mpaw-runs*
  100
  "Number of times to run make-prime-array%-works")

(define-test make-prime-array%-works
  :parent prime-gen-helpers-test-suite
  :depends-on (make-prime-array%-errors-and-outliers)
  (dotimes (i *mpaw-runs*)
    (let* ((start (random 1000000))
           (stop (random 1500000))
           (size (estimate-prime-array-size% start stop))
           (array (make-prime-array% start stop)))
      (is #'equalp #() array "args: ~D ~D" start stop)
      (is #'= (max size 1) (array-dimension array 0) "args: ~D ~D" start stop))))

(define-test sieve-aux%-test
  :parent prime-gen-helpers-test-suite
  :depends-on (make-prime-array%-works)
  (is #'equal #* (sieve-aux% -25))
  (is #'equal #*0 (sieve-aux% 0))
  (is #'equal #*00 (sieve-aux% 1))
  (is #'equal #*001 (sieve-aux% 2))
  (is #'equal #*0011 (sieve-aux% 3))
  (let ((sieve (sieve-aux% *small-primes-limit*))
        (array (make-prime-array% 0 *small-primes-limit*)))
    (dotimes (n *small-primes-limit*)
      (when (plusp (bit sieve n))
        (vector-push-extend n array)))
    (is #'equalp *small-primes* array)))

(defparameter *ss-test-cases*
 '(((:start . 999950) (:end . 1000000) (:primes . #(999953 999959 999961 999979 999983)))
   ((:start . 55555555) (:end . 55555654) (:primes . #(55555559 55555573 55555579 55555631 55555651)))
   ((:start . 1234567890) (:end . 1234567989) (:primes . #(1234567891 1234567907 1234567913 1234567927 1234567949 1234567967 1234567981)))
   ((:start . 1099511627776) (:end 1099511627903) (:primes . #(1099511627791 1099511627803 1099511627831 1099511627873 1099511627891)))
   ((:start . 17592186044287) (:end 17592186044416) (:primes . #(17592186044287 17592186044297 17592186044299 17592186044399))))
  "Test data for segmented-sieve and segment-aux%")

(define-test segment-aux%-correct-answers
  :parent prime-gen-helpers-test-suite
  :depends-on (sieve-of-eratosthenes-errors-and-outliers
               sieve-of-eratosthenes-correct-answers)
  (dolist (test-case *ss-test-cases*)
    (let* ((start (assoc :start test-case))
           (stop (assoc :stop test-case))
           (prime-array (assoc :prime-array test-case))
           (sieve (segment-aux start stop)))
      (is #'= (- stop start) (length sieve))
      (do ((i start (1+ i)))
          ((= i stop))
        (if (find i prime-array)
            (is #'= 1 (bit sieve (- i start)))
            (is #'= 0 (bit sieve (- i start))))))))

(define-test segment-aux%-works-big-ranges
  :parent prime-gen-helpers-test-suite
  :depends-on (segment-aux%-correct-answers)
  (let ((stop (1- *segmented-sieve-limit*)))
    (do* ((difference 10 (* difference 10))
          (start #1=(- start difference) #1#))
         ((> difference (expt 10 6)))
      (let ((sieve (segment-aux% start stop)))
        (is #'= (- stop start) (length sieve))))))

(define-test small-primes-range-subseq%-works
  :parent prime-gen-helpers-test-suite
  :depends-on (real-val-subseq-test)
  (dotimes (i 10)
    (let ((n (serapeum:random-in-range 2 *small-primes-limit*)))
      (is #'equalp small-primes-range-subseq% n n)))
  (dotimes (i 50)
    (let* ((bigger (serapeum:random-in-range 100 *small-primes-limit*))
           (smaller (random bigger)))
      (is #'equalp #() (small-primes-range-subseq% bigger smaller))
      (let* ((useful-subseq (small-primes-subseq% smaller bigger))
             (first-useful (alexandria:first-elt useful-subseq))
             (last-useful (alexandria:last-elt useful-subseq)))
        (true (>= first-useful start))
        (true (< last-useful stop))
        (true (notany #'(lambda (p)
                          (and
                           (< p first-useful)
                           (>= p start)))
                      *small-primes*))
        (true (notany #'(lambda (p)
                          (and
                           (> p last-useful)
                           (< p stop)))
                      *small-primes*))))))

(define-test plain-sieve-subseq%-works
  :parent prime-gen-helpers-test-suite
  :depends-on (sieve-of-eratosthenes-errors-and-outliers
               sieve-of-eratosthenes-cotrect-answers
               real-val-subseq-test)
  (dotimes (i 50)
    (let* ((stop (random *plain-sieve-limit*))
           (range (random (min stop 10000)))
           (start (- stop range))
           (subseq (plain-sieve-subseq% start stop)))
      (cond
        ((uiop:emptyp subseq)
         (false
          (primes-in-range-p start stop)))
        (t
         (let ((first-subseq (alexandria:first-elt subseq))
               (last-subseq (alexandria:last-elt subseq)))
           (true (primep first-subseq))
           (true (primep last-subseq))
           (dotimes (i 3)
             (true (primep (alexandria:random-elt subseq))))
           (false (primes-in-range-p start first-subseq))
           (false (primes-in-range-p (1+ last-subseq) stop))))))))

(defun primes-in-range-p (start stop)
  "Helper function for testing prime ranges. Returns
   the first prime number p found sunch that START <= p < STOP.
   If no prime numbers are found, returns nil."
  (do ((i (max start 2) (if (< i 3) (+ i 1) (+ i 2))))
      ((>= i stop) nil)
    (when (primep i)
      (return-from primes-in-range-p i))))

(define-test sequential-prime-range%-works
  :parent prime-gen-helpers-test-suite
  :depends-on (make-prime-array%-errors-and-outliers
               make-prime-array%-works
               next-prime-errors-and-outliers
               next-prime-correct-answers)
  (dotimes (i 50)
    (let* ((stop (serapeum:random-in-range 3 most-positive-fixnum))
           (range (random 1000))
           (start (- stop range))
           (subseq (sequential-prime-range% start stop)))
      (cond
        ((uiop:emptyp subseq)
         (false (primes-in-range-p start stop)))
        (t
         (let ((first-subseq (alexandria:first-elt subseq))
               (last-subseq (alexandria:last-elt subseq)))
           (true (primep first-subseq))
           (true (primep last-subseq))
           (dotimes (i 3)
             (true (primep (alexandria:random-elt subseq))))
           (false (primes-in-range-p start first-subseq))
           (false (primes-in-range-p (1+ last-subseq) stop))))))))



(define-test prime-gen-main
  :parent prime-generation-test-suite)

(define-test sieve-of-eratosthenes-errors-and-outliers
  :parent prime-gen-main
  :depends-on (sieve-aux%-test)
  (fail (sieve-of-eratosthenes "one thousand") 'type-error*)
  (fail (sieve-of-eratosthenes #C(1 2)) 'type-error*)
  (finish (sieve-of-eratosthenes #C(1 0)))
  (dolist (x '(-35 0 0.99 1/2))
    (is #'equalp #() (sieve-of-eratosthenes x))))

(define-test sieve-of-eratosthenes-correct-results
  :parent prime-gen-main
  :depends-on (sieve-aux%-test)
  (dolist (x `(1 1.5 10 10.75 40 40.1 95 110 ,*small-primes-limit*))
    (is #'equalp (serapeum:take-while #'(lambda (p) (<= p x)) *small-primes*)
        (sieve-of-eratosthenes x))))

(define-test next-prime-errors-and-outliers
  :parent prime-gen-main
  (fail (next-prime #\8) 'type-error*)
  (fail (next-prime #C(1 1)) 'type-error*)
  (dolist (x '(-12345 -1324.6763 0 0.0 1 1.9999 1/2 -13/64))
    (is #'= 2 (next-prime x)))
  (dolist (x '(2 2.0 5/4 2.7 2.999 2999/1000))
    (is #'= 3 (next-prime x))))

(define-test next-prime-correct-answers
  :parent prime-gen-main
  :depends-on (primep-suite)
  (let ((test-cases '((0 2)
                      (1 11)
                      (2 101)
                      (3 1009)
                      (4 10007)
                      (5 100003)
                      (6 1000003)
                      (7 10000019)
                      (8 100000007)
                      (9 1000000007)
                      (10 10000000019))))
    (dolist (test-case test-cases)
      (is #'= (second test-case) (next-prime (expt 10 (first test-case))))))
  (let ((test-cases '((6 7)
                      (68 71)
                      (399 401)
                      (4354 4357)
                      (37999 38011)
                      (982704 982741)
                      (3042599 3042607)
                      (15854369 15854381)
                      (989775274 989775281)
                      (6764602577 6764604591))))
    (dotimes (test-case test-cases)
      (is #'= (second test-case) (next-prime (first test-case))))))

(define-test segmented-sieve-errors-and-outliers
  :parent prime-gen-main
  :depends-on (make-prime-array%-errors-and-outliers
               make-prime-array%-works
               segment-aux%-works
               segment-aux%-works-big-ranges)
  (fail (segmented-sieve "one hundred twenty-seven" 401) 'type-error*)
  (fail (segmented-sieve #C(127 1) 401) 'type-error*)
  (fail (segmented-sieve 127 "four hundred one") 'type-error*)
  (fail (segmented-sieve 127 #C(401 1)) 'type-error*)
  (fail (segmented-sieve 1000 1000000) 'domain-error)
  (fail (segmented-sieve 5 49) 'domain-error*)

  (dolist (test-case '((505 . 505)
                       (10000 . 505)
                       (504.3 . 504.9)
                       (134.765 . 65.789)))
    (is #'equalp #() (segmented-sieve (car test-case) (cdr test-case)))))

(define-test segmented-sieve-correct-answers
  :parent prime-gen-main
  :depends-on (segmented-sieve-errors-and-outliers)
  (dolist (test-case *ss-test-cases*)
    (let ((start (assoc :start test-case))
          (stop (assoc :stop test-case))
          (primes (assoc :primes test-case)))
      (is #'equalp primes (segmented-sieve start stop))
      (is #'equalp primes (segmented-sieve (+ 0.8 start) stop))
      (is #'equalp primes (segmented-sieve start (+ 0.8 stop)))
      (is #'equalp primes (segmented-sieve (+ 9/10 start) (+ 9/10 stop))))))
(defparameter *ss-test-runs*
  100
  "Number of times to run segmented-sieve-random-arg-test")

(define-test segmented-sieve-random-arg-test
  :parent prime-gen-main
  :depends-on (segmented-sieve-errors-and-outliers)
  (dotimes (i *ss-test-runs*)
    (let* ((start (serapeum:random-in-range *plain-sieve-limit* (- *segmented-sieve-limit* 10001)))
           (range (serapeum:random-in-range 100 10000))
           (stop (+ start range)))
      (finish (segmented-sieve start stop)))))

(define-test primes-in-range-errors-and-outliers
  :parent prime-gen-main
  :depends-on (prime-gen-helpers-test-suite)
  (fail (primes-in-range "one million" 1000100) 'type-error*)
  (fail (primes-in-range #C(5 1) 1000) 'type-error*)
  (fail (primes-in-range 1000000 "one million one hundred") 'type-error*)
  (fail (primes-in-range 1000000 #C(1000100 5)) 'type-error*)
  (fail (primes-in-range 1 100000000) 'large-range-error)

  (dolist (test-case '((100 100)
                       (100 101)
                       (101 100)
                       (-50 -2)
                       (-1990 2)))
    (is #'equalp #() (apply #'primes-in-range test-case))
    (is #'equalp #() (primes-in-range (+ (first test-case) 0.9) (- (second test-case 0.9))))
    (is #'equalp #() (primes-in-range (+ (first test-case) 9/10) (- (second test-case 9/10))))))

(define-test primes-in-range-works
  :parent prime-gen-main
  :depends-on (primes-in-range-errors-and-outliers)
  ;; primes-in-range uses different techniques to
  ;; provide results depending on the input args. We need to make sure
  ;; all manner of args not covered by primes-in-range-errors-and-outliers

  ;; when the range is completely within the range of *small-primes*
  (is #'equalp *small-primes* (primes-in-range 0 *small-primes-limit*))
  (is #'equalp *small-primes* (primes-in-range -123456789 *small-primes-limit*))
  (dotimes (i 20)
    (let* ((stop (random *small-primes-limit*))
           (start (serapeum:random-in-range -100 stop))
           (results (primes-in-range start stop)))
      (cond
        ((uiop:emptyp results)
         (false (primes-in-range-p start stop)))
        (t
         (is #'equalp results
             (subseq
              *small-primes*
              (position-if #'(lambda (p)
                               (>= p start))
                           *small-primes*)
              (position-if #'(lambda (p)
                               (>= p stop))
                           *small-primes*)))))))
  ;; The testing for the rest of the scenarios can be accomplished the
  ;; same way for each scenario
  (let ((small-range (- *sequential-range-lower-ceiling* 10))
        (big-range (+ *sequential-range-lower-ceiling* 1000)))
    (dolist (params '((*small-primes-limit* *plain-sieve-limit*)
                      (*plain-sieve-limit* *segmented-sieve-limit*)
                      (*segmented-sieve-limit* (square most-positive-fixnum))))
      (destructuring-bind
          (min-start max-stop)
          params
        (dotimes (i 50)
          (let ((stop (serapeum:random-in-range (+ min-start big-range) max-stop))
                (high-start (max min-start (- stop small-range)))
                (low-start (max min-start (- stop big-range)))
                (small-seq (primes-in-range high-start stop))
                (big-seq (primes-in-range low-start stop)))
            (dolist (params '((small-seq high-start)
                              (big-seq low-start)))
              (destructuring-bind
                  (seq start)
                  params           
                (cond
                  ((uiop:emptyp seq)
                   (false (primes-in-rangep start stop)))
                  (t
                   (let ((first-seq (alexandria:first-elt seq))
                         (last-seq (alexandria:last-elt seq)))
                     (true (>= first-seq start))
                     (true (< last-seq stop))
                     (true (primep first-seq))
                     (true (primep last-seq))
                     (dotimes (j 10)
                       (true (primep (alexandria:random-elt seq)))))))))))))))

(define-test primes-below-x-test
  :parent prime-gen-main
  :depends-on (primes-in-range-correct-answers)
  (fail (primes-below-x "one million") 'type-error*)
  (fail (primes-below-x #C(1000000 1)) 'type-error*)
  (dotimes (i 50)
    (let* ((x (random 10000.00)))
           (primes (primes-below-x x))
      (cond
        ((< x 3)
         (true (uiop:emptyp primes)))
        (t
         (let ((pfirst (alexandria:first-elt primes))
               (plast (alexandria:last-elt primes)))
           (is #'= 2 pfirst)
           (true (primep plast))
           (false (primee-in-range-p (1+ plast) x))
           (dotimes (j 10)
             (true (primep (alexandria:random-elt primes)))))))))
  (let* ((big-primes (primes-below-x *soft-range-limit*))
         (pfirst (alexandria:first-elt big-primes))
         (plast (alexandria:last-elt big-primes)))
    (is #'= 2 pfirst)
    (true (primep plast))
    (false (primes-in-range-p (1+ plast) *soft-range-max*))
    (dotimes (i 10)
      (true (primep (alexandria:random-elt big-primes))))))
