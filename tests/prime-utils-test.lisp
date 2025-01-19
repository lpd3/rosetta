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
    (defparameter *test-params-path*
      
      #P"~/coding/lisp/rosetta/tests/.test-params.conf")
    (defparameter *default-test-path*
      #P"~/coding/lisp/rosetta/tests/.current-test.conf")
    (defparameter *test-package*
      :prime-utils-test)))

(defmacro retest (&key test reload-all-p)
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
  (unless (probe-file *test-params-path*)
    (init-retest))
  (alexandria:with-gensyms (source-code-system test-system config f current-test all?)
    `(let ((,current-test ,test)
           (,all? ,reload-all-p)
           (,source-code-system nil)
           (,test-system nil)
           (,config nil)
           (,f nil))
       (with-open-file (,f *test-params-path*
                        :direction :input)
         (uiop:with-safe-io-syntax (:package *test-package*)
           (setf ,config (read ,f))))
       (setf ,source-code-system (cdr (assoc :source-code-system ,config))
             ,test-system (cdr (assoc :test-system ,config)))
       (if ,current-test
           (with-open-file (,f *default-test-path* 
                              :direction :output
                              :if-exists :overwrite
                              :if-does-not-exist :create)
             (prin1 ,current-test ,f))
           (setf ,current-test (with-open-file (,f *default-test-path*
                                                  :direction :input)
                                 (uiop:with-safe-io-syntax (:package *test-package*)
                                   (read ,f)))))
       (when ,all?
         (ql:quickload ,source-code-system))
       (ql:quickload ,test-system)
       (parachute:test ,current-test))))

(defun init-retest ()
  (let ((source-code-system nil)
        (test-system nil)
        (test nil)
        (test-package *test-package*))
    (uiop:with-safe-io-syntax (:package test-package)
      (format t "~&~%Which source-code system?~%")
      (setf source-code-system (read))
      (format t "~&~%Which test system?~%")
      (setf test-system (read))
      (format t "~&~%Which test?~%")
      (setf test (read)))
    (with-open-file (f *test-params-path*
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :overwrite)
      (prin1 `((:source-code-system . ,source-code-system)
               (:test-system . ,test-system))
             f))
    (with-open-file (f *default-test-path*
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :overwrite)
      (prin1 test f))))

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

(define-test primes-below-x-suite
  :parent prime-utils-test-suite)

(define-test primes-below-x-errors
  :parent primes-below-x-suite
  (fail (primes-below-x "one hundred") type-error* "x: one hundred (string)")
  (fail (primes-below-x #C(10 10)) type-error* "x: #C(10 10) (the complex number 10+10i)"))

(define-test primes-below-x-outliers
  :parent primes-below-x-suite
  (dolist (x '(-5 -5.0 -3/2 0 0.0 1 1.0 3/2 1.5 2 2.0 5/2 2.99))
    (let ((result (primes-below-x x)))
      (is #'equalp #() result "With x = ~A, result should be #(). Instead, it is ~S" x result))))

(define-test sieve-to%-errors
  :parent primes-below-x-suite
  (dolist (n '("five" 3/2 22.3 -53 0 1))
    (fail (sieve-to% n) error "~S of type ~A should not be a permitted argument" n (type-of n))))

(define-test sieve-to%-works
  :parent primes-below-x-suite
  (do  ((n 2 (* n 5)))
       ((> n 10000000))
    (finish (sieve-to% n) "n: ~D" n))
  (do ((i 0 (1+ i))
       (n #1=(+ 2 (random (- 10000000 2))) #1#))
      ((= i 20))
    (finish (sieve-to% n) "n: ~D" n)))

(define-test primes-below-x-small
  :parent primes-below-x-suite
  :depends-on (sieve-to%-errors
               sieve-to%-works)
  (do* ((x 3 (1+ x))
        (primes #1=(primes-below-x x) #1#))
       ((> x *small-primes-limit*))
    (true (alexandria:starts-with-subseq primes *small-primes* :test #'=) "For x = ~D, did not return a prefix of *small-primes*" x)))

(define-test primes-below-x-big
  :parent primes-below-x-suite
  :depends-on '(primes-below-x-small)
  (do ((x *small-primes-limit* (* x 5))
       (primes nil nil))
      ((> x 5000000))
    (finish (setq primes (primes-below-x x)) "x: ~D" x)
    (true (alexandria:starts-with-subseq *small-primes* primes) "x: ~D" x))
  (do ((i 0 (1+ i))
       (x #1=(+ *small-primes-limit* (random (- 5000000 *small-primes-limit*))) #1#)
       (primes nil nil))
      ((= i 10))
    (finish (setq primes (primes-below-x x)) "x: ~D" x)
    (true (alexandria:starts-with-subseq *small-primes* primes) "x: ~D" x))
  (let ((primes
          #(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107
            109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227
            229 233 239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349
            353 359 367 373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467
            479 487 491 499 503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613
            617 619 631 641 643 647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751
            757 761 769 773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883 887
            907 911 919 929 937 941 947 953 967 971 977 983 991 997 1009 1013 1019 1021 1031 1033
            1039 1049 1051 1061 1063 1069 1087 1091 1093 1097 1103 1109 1117 1123 1129 1151 1153
            1163 1171 1181 1187 1193 1201 1213 1217 1223 1229 1231 1237 1249 1259 1277 1279 1283
            1289 1291 1297 1301 1303 1307 1319 1321 1327 1361 1367 1373 1381 1399 1409 1423 1427
            1429 1433 1439 1447 1451 1453 1459 1471 1481 1483 1487 1489 1493 1499 1511 1523 1531
            1543 1549 1553 1559 1567 1571 1579 1583 1597 1601 1607 1609 1613 1619 1621 1627 1637
            1657 1663 1667 1669 1693 1697 1699 1709 1721 1723 1733 1741 1747 1753 1759 1777 1783
            1787 1789 1801 1811 1823 1831 1847 1861 1867 1871 1873 1877 1879 1889 1901 1907 1913
            1931 1933 1949 1951 1973 1979 1987 1993 1997 1999 2003 2011 2017 2027 2029 2039 2053
            2063 2069 2081 2083 2087 2089 2099 2111 2113 2129 2131 2137 2141 2143 2153 2161 2179
            2203 2207 2213 2221 2237 2239 2243 2251 2267 2269 2273 2281 2287 2293 2297 2309 2311
            2333 2339 2341 2347 2351 2357 2371 2377 2381 2383 2389 2393 2399 2411 2417 2423 2437
            2441 2447 2459 2467 2473 2477 2503 2521 2531 2539 2543 2549 2551 2557 2579 2591 2593
            2609 2617 2621 2633 2647 2657 2659 2663 2671 2677 2683 2687 2689 2693 2699 2707 2711
            2713 2719 2729 2731 2741 2749 2753 2767 2777 2789 2791 2797 2801 2803 2819 2833 2837
            2843 2851 2857 2861 2879 2887 2897 2903 2909 2917 2927 2939 2953 2957 2963 2969 2971
            2999 3001 3011 3019 3023 3037 3041 3049 3061 3067 3079 3083 3089 3109 3119 3121 3137
            3163 3167 3169 3181 3187 3191 3203 3209 3217 3221 3229 3251 3253 3257 3259 3271 3299
            3301 3307 3313 3319 3323 3329 3331 3343 3347 3359 3361 3371 3373 3389 3391 3407 3413
            3433 3449 3457 3461 3463 3467 3469 3491 3499 3511 3517 3527 3529 3533 3539 3541 3547
            3557 3559 3571 3581 3583 3593 3607 3613 3617 3623 3631 3637 3643 3659 3671 3673 3677
            3691 3697 3701 3709 3719 3727 3733 3739 3761 3767 3769 3779 3793 3797 3803 3821 3823
            3833 3847 3851 3853 3863 3877 3881 3889 3907 3911 3917 3919 3923 3929 3931 3943 3947
            3967 3989 4001 4003 4007 4013 4019 4021 4027 4049 4051 4057 4073 4079 4091 4093 4099
            4111 4127 4129 4133 4139 4153 4157 4159 4177 4201 4211 4217 4219 4229 4231 4241 4243
            4253 4259 4261 4271 4273 4283 4289 4297 4327 4337 4339 4349 4357 4363 4373 4391 4397
            4409 4421 4423 4441 4447 4451 4457 4463 4481 4483 4493 4507 4513 4517 4519 4523 4547
            4549 4561 4567 4583 4591 4597 4603 4621 4637 4639 4643 4649 4651 4657 4663 4673 4679
            4691 4703 4721 4723 4729 4733 4751 4759 4783 4787 4789 4793 4799 4801 4813 4817 4831
            4861 4871 4877 4889 4903 4909 4919 4931 4933 4937 4943 4951 4957 4967 4969 4973 4987
            4993 4999 5003 5009 5011 5021 5023 5039 5051 5059 5077 5081 5087 5099 5101 5107 5113
            5119 5147 5153 5167 5171 5179 5189 5197 5209 5227 5231 5233 5237 5261 5273 5279 5281
            5297 5303 5309 5323 5333 5347 5351 5381 5387 5393 5399 5407 5413 5417 5419 5431 5437
            5441 5443 5449 5471 5477 5479 5483 5501 5503 5507 5519 5521 5527 5531 5557 5563 5569
            5573 5581 5591 5623 5639 5641 5647 5651 5653 5657 5659 5669 5683 5689 5693 5701 5711
            5717 5737 5741 5743 5749 5779 5783 5791 5801 5807 5813 5821 5827 5839 5843 5849 5851
            5857 5861 5867 5869 5879 5881 5897 5903 5923 5927 5939 5953 5981 5987 6007 6011 6029
            6037 6043 6047 6053 6067 6073 6079 6089 6091 6101 6113 6121 6131 6133 6143 6151 6163
            6173 6197 6199 6203 6211 6217 6221 6229 6247 6257 6263 6269 6271 6277 6287 6299 6301
            6311 6317 6323 6329 6337 6343 6353 6359 6361 6367 6373 6379 6389 6397 6421 6427 6449
            6451 6469 6473 6481 6491 6521 6529 6547 6551 6553 6563 6569 6571 6577 6581 6599 6607
            6619 6637 6653 6659 6661 6673 6679 6689 6691 6701 6703 6709 6719 6733 6737 6761 6763
            6779 6781 6791 6793 6803 6823 6827 6829 6833 6841 6857 6863 6869 6871 6883 6899 6907
            6911 6917 6947 6949 6959 6961 6967 6971 6977 6983 6991 6997 7001 7013 7019 7027 7039
            7043 7057 7069 7079 7103 7109 7121 7127 7129 7151 7159 7177 7187 7193 7207 7211 7213
            7219 7229 7237 7243 7247 7253 7283 7297 7307 7309 7321 7331 7333 7349 7351 7369 7393
            7411 7417 7433 7451 7457 7459 7477 7481 7487 7489 7499 7507 7517 7523 7529 7537 7541
            7547 7549 7559 7561 7573 7577 7583 7589 7591 7603 7607 7621 7639 7643 7649 7669 7673
            7681 7687 7691 7699 7703 7717 7723 7727 7741 7753 7757 7759 7789 7793 7817 7823 7829
            7841 7853 7867 7873 7877 7879 7883 7901 7907 7919 7927 7933 7937 7949 7951 7963 7993
            8009 8011 8017 8039 8053 8059 8069 8081 8087 8089 8093 8101 8111 8117 8123 8147 8161
            8167 8171 8179 8191 8209 8219 8221 8231 8233 8237 8243 8263 8269 8273 8287 8291 8293
            8297 8311 8317 8329 8353 8363 8369 8377 8387 8389 8419 8423 8429 8431 8443 8447 8461
            8467 8501 8513 8521 8527 8537 8539 8543 8563 8573 8581 8597 8599 8609 8623 8627 8629
            8641 8647 8663 8669 8677 8681 8689 8693 8699 8707 8713 8719 8731 8737 8741 8747 8753
            8761 8779 8783 8803 8807 8819 8821 8831 8837 8839 8849 8861 8863 8867 8887 8893 8923
            8929 8933 8941 8951 8963 8969 8971 8999 9001 9007 9011 9013 9029 9041 9043 9049 9059
            9067 9091 9103 9109 9127 9133 9137 9151 9157 9161 9173 9181 9187 9199 9203 9209 9221
            9227 9239 9241 9257 9277 9281 9283 9293 9311 9319 9323 9337 9341 9343 9349 9371 9377
            9391 9397 9403 9413 9419 9421 9431 9433 9437 9439 9461 9463 9467 9473 9479 9491 9497
            9511 9521 9533 9539 9547 9551 9587 9601 9613 9619 9623 9629 9631 9643 9649 9661 9677
            9679 9689 9697 9719 9721 9733 9739 9743 9749 9767 9769 9781 9787 9791 9803 9811 9817
            9829 9833 9839 9851 9857 9859 9871 9883 9887 9901 9907 9923 9929 9931 9941 9949 9967
            9973)))
    (is #'equalp primes (primes-below-x 10000) "for x = 10000 did not match the constant vector of primes below 10000")))



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
    (false (baillie-psq spsp2-3) "~D a strong pseudoprime to bases 2 and 3" spsp2-3)))

;; small perfect squares

(define-test baillie-psw-vs-small-squares
  :parent baillie-psw-suite
  (do* ((i 1001 (+ i 1))
        (ss (* i i) (* i i)))
       ((= i 10000))
    (false (baillie-psw ss) "~D (~D^2)" ss i)))

;; Large perfect squares

(define-test baillie-psw-vs-large-squares
  :parent baillie-psw-suite
  (do* ((i 0 (1+ i))
        (n (expt 10 60) (1+ n))
        (ls (* n n) (* n n)))
       ((= i 2000))
    (false (baillie-psw ls) "~D (~D^2)" ls n)))

(define-test baillie-psw-identifies-primes
  :parent baillie-psw-suite
  (loop for prime across (primes-below-x (expt 10 6))
        when (> prime *small-primes-limit*)
          do
        (true (baillie-psw prime) "n: ~D is prime but baillie-psw said no." prime)))
