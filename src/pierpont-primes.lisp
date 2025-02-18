;;;; rosetta/src/pierpont-primes.lisp

#| A Pierpont prime is a prime number of the form: 2^u * 3^v + 1 for some non-negative integers u
   and v. A Pierpont prime of the second kind is a prime number of the form: 2^u * 3^v - 1 for some
   non-negative integers u and v.
  
   The term "Pierpont primes" is generally understood to mean the first definition, but will be
   called "Pierpont primes of the first kind" on this page to distinguish them.


   TASK

   * Write a routine (function, procedure, whatever) to find Pierpont primes of
     the first & second kinds.

   * Use the routine to find and display here, on this page, the first 50 Pier-
     pont primes of the first kind.

   * Use the routine to find and display here, on this page, the first 50 Pier-
     pont primes of the second kind.

   * If your language supports large integers, find and display here, on this
     page, the 250th Pierpont prime of the first kind and the 250th Pierpont
     prime of the second kind.
|#

;;;; Some salient facts about Pierpont primes:

;;;; 1. There is only one Pierpont prime where u = 0: 2
;;;; 2. There are only 6 known Pierpont primes for which v = 0.
;;;;    Besides 2, these are the only known Fermat primes: 3, 5, 17, 257, and 65,537.
;;;; 3. All other Pierpont primes are identical to 1 mod 6.
;;;; 4. It is conjectured that there are an infinite number of Pierpont primes.
;;;; 5. The following chart gives a power of 10 and the number of Pierpont primes less than that
;;;;    number:

;;;;    limit       count
;;;;    10            4
;;;;    100          10
;;;;    10,000       25
;;;;    100,000,000  57
;;;;    10^16       125
;;;;    10^32       250

;;;; 6. Distribution: There should be approximately 9n Pierpont primes for numbers less than 10^n

;;;; Let's compare

;;;; n  9n   actual count
;;;; 0  0    0
;;;; 1  9    4
;;;; 2  18   10
;;;; 4  36   25
;;;; 8  72   57
;;;; 16 144  125
;;;; 32 288  250

;;;; 7. Numbers of the form N = k * 2^n + 1, where k is odd and k < 2^n are called
;;;;    Proth numbers. Proth numbers can be tested for primality by using Proth's theorem:
;;;;    A Proth number p is prime only if there exists an integer a such that
;;;;    a((p-1)/2) is identical to -1 mod p. This suggests a Las Vegas probabilistic test
;;;;    for primality. Of Pierpont prime candidates (2^u * 3^v + 1), those candidates
;;;;    for which 2^u > 3^v are Proth numbers.

;;;; 8. When a Pierpont candidate p is not a Proth number (i.e., 2^u < 3^v),
;;;;    alternative primality tests of factorizations of p-1, being a small even number
;;;;    multiplied by a large power of 3.

;;;; These primality tests are nice to know about. But I will stick with the tests I have
;;;; written.

;;;; The task is perhaps not so formidable as it would appear at first glance. Powers spread out
;;;; very, very quickly. For this reason, we can roughly assume that for numbers with n digits,
;;;; roughly 1/n of the numbers 2^u * 3^v + 1 will be Pierpont primes. At the very most, we should
;;;; have to examime maybe 513 numbers to find the first 50 Pierpont primes. Given the 250th
;;;; Pierpont prime, we should need to search at most 8,000 numbers. This assumes we locate every
;;;; Pierpont prime through the 250th, which is not required.

;;;; Approach:

;;;; 1. Collect the first 50 Pierpont primes
;;;;    1.1 Create two arrays of size 100 (array 1 and array 2)
;;;;    1.2 Push 2 and the Fermat primes to array 1
;;;;    1.3 Create a list of primes from 5 to 10,000
;;;;    1.4 Create two temporary arrays, temp 1 and temp 2 (size: 500 each)
;;;;    1.5 (We need not consider any type 1 primes for u = 0. These have been already added.
;;;;        But we do need to consider type 2 primes with u = 0.)
;;;;       Create a temp array.
;;;;    1.6 loop for v from 1 to 16.
;;;;       1.6.1 Add 3^v - 1 to the temp array
;;;;    1.7 Using the sieve, push the prime elements of the temp array to array 2.
;;;;    1.8 loop for u from 1 to 26.
;;;;       1.8.1 create two temporary arrays: temp1, temp2
;;;;       1.8.2 loop for v from 1 while 2^u * 3^v - 1 < 10^8
;;;;           1.8.2.1 add all 2^u * 3^v + 1 that are less than 10^9 to temp1
;;;;           1.8.2.2 add all 2^u * 3^v - 1 to temp2
;;;;       1.8.3 Push all prime elements of temp1 to array1
;;;;       1.8.4 Push all prime elememts of temp2 to array2
;;;;    1.9 Sort array1
;;;;    1.10 Sort array2
;;;;    1.11 truncate array1 to 50 elememts
;;;;    1.12 truncate array2 to 50 elements
;;;; 2. Find the 250th of type1 and type2
;;;;    2.1 Initialize two vars, type1 and type2, to 2.
;;;;       2.2.1 loop for u = 1 to 106
;;;;           2.2.1 initialize type1-done and type2-done to nil
;;;;           2.2.2 loop for v = max(2^u * 3^v - 1) < 10^32, decrementing downward.
;;;;                 2.2.2.1 Find the type 1 number
;;;;                 2.2.2.2 If it is < type1
;;;;                         2.2.2.2.1 If type2-done is t, break from inner loop
;;;;                         2.2.2.2.2 Else, set type1-done to t
;;;;                 2.2.2.3 Else If the type 1 is prime
;;;;                         2.2.2.3.1 Set type1 to the type 1 number
;;;;                         2.2.2.3.2 If type2-done is t, exit from inner loop
;;;;                         2.2.2.3.3 Otherwise, set type1-done to t
;;;;                 2.2.2.4 Find next type 2 number
;;;;                 2.2.2.5 If type 2 number < type2
;;;;                         2.2.2.5.1 If type1-done is t, exit from inner loop
;;;;                         2.2.2.5.2 Else set type2-done to t
;;;;                 2.2.2.6 Else if the type 2 number is prime
;;;;                         2.2.2.6.1 Set type2 to the type 2 number
;;;;                         2.2.2.6.2 If type1-done is t, exit from inner loop
;;;;                         2.2.2.6.3 Else set type2-done to t
;;;; 3. Report the results

(defun pierpont-primes ()
  (multiple-value-bind
        (type1 type2)
        (first-50-pierponts-types-1-and-2)
    (multiple-value-bind
          (250th-type1 250th-type2)
          (250th-pierponts-types-1-and-2)
      (display-results type1 type2 250th-type1 250th-type-2))))

(defun first-50-pierponts-types-1-and-2 ()
  (let ((type1 (make-array 64
                 :element-type 'fixnum
                 :fill-pointer 0))
        (type2 (make-array 64
                 :element-type 'fixnum
                 :fill-pointer 0))
        (primes (primes-below-x 10000)))
    (dolist (zero-u-prime '(2 3 5 17 257 65537))
      (vector-push zero-u-prime type1))
    (let ((temp (make-array 16
                            :element-type 'fixnum
                            :fill-pointer 0)))
      (do* ((v 1 (1+ v))
            (candidate #1=(- (expt 3 v) 1) #1#))
           ((= v 17))
        (vector-push candidate temp))
      (do-each (candidate temp)
        (let ((stop (isqrt candidate)))
          (do-each (p primes (vector-push candidate type2))
            (when (> p stop)
              (vector-push candidate type2)
              (return))
            (when (zerop (mod candidate prime))
              (return))))))
    (do ((u 1 (1+ u)))
        ((> (- (* (expt 2 u) 3) 1) 1e8))
      (let ((temp1 #2=(make-array 500
                        :element-type 'fixnum
                        :fill-pointer 0
                        :adjustable t))
            (temp2 #2#))
        (do ((v 1 (1+ v)))
            (nil)
          (let* ((number1 (+ (* (expt 2 u) (expt 3 v)) 1))
                 (number2 (- number1 2)))
            (when (> number2 1e8)
              (return))
            (when (< number1 1e8)
              (vector-push-extend number1 temp1))
            (vector-push-extend number2 temp2)))
        (do-each (candidate temp1)
          (let ((stop (isqrt candidate)))
            (do-each (p primes (vector-push candidate type1))
              (when (> p isqrt)
                (vector-push candidate type1)
                (return))
              (when (zerop (mod candidate prime))
                (return)))))
        (do-each (candidate temp2)
          (let ((stop (isqrt candidate)))
            (do-each (p primes (vector-push candidate type2))
              (when (> p stop)
                (vector-push candidate type2)
                (return))
              (when (zerop (mod candidate p))
                (return)))))))
    (sort type1 #'<)
    (sort type2 #'<)
    (setf (fill-pointer type1) 50)
    (setf (fill-pointer type2) 50)
    (values type1 type2)))
         
