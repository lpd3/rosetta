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
              prime-utils::div2mod
              prime-utils::sieve-to%)
            :prime-utils-test)
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
       (handler-case
         (parachute:test ,gtest)
         (error (e)
           #1=(setf *first-run-of-session-p* nil
                    *default-test* ,gtest)
           (error e))
         (:no-error () #1#)))))


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
    (let ((k (random 1000))
          (n (1- (* (random 500) 2))))
      (is #'= 1 (jacobi k 1))
      (is #'= 1 (jacobi 1 n)))))

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
  :parent prime-utils-test-suite
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
  :parent prime-utils-test-suite
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
  :parent prime-utils-test-suite
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
  :parent prime-utils-test-suite
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

