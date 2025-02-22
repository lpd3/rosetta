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

;;;; Well, it would not be required if we knew ceilings for
;;;; Pierpont primes of the 2nd type. We know there are 57
;;;; type 1 primes less than 1e8 and there are 250 less than
;;;; 1e32. Although the distribution of the two types of primes
;;;; appears to be similar (up to the 43rd prime of each type)
;;;; it would be foolhardy to say that the ceilings given for type 1
;;;; would have the same exact values for type 2. This suggests two
;;;; different approaches. We can find the first 57 type 1 primes,
;;;; sort them and take the smallest 50. Then we can search for the single
;;;; largest type 1 prime less than 1e32.

;;;; For the type 2 primes, it looks like we will have to search until we
;;;; find the 250th prime, displaying 1-50 and #250.

;;;; We can avoid a small amount of repetition by searching for type2 primes
;;;; while we search for the first 57 type 1 primes. But then we must finish
;;;; the job for type 2s.

(in-package :pierpont-primes)

(defun pierponts ()
  "Solves the problem described above."
  (multiple-value-bind
        (first-50-type-1 small-type-2 57th-type-1)
      (find-small-pierponts)
    (let ((250th-type-1 (find-250th-type-1 57th-type-1))
          (type-2 (add-rest-type-2 small-type-2)))
      (display-results first-50-type-1 (take 50 type-2) 250th-type-1 (last-elt type-2)))))

(defun find-small-pierponts ()
  "Finds the first 50 Pierpont primes of both types.
   Returns three values. The first is a sorted array of the first Pierpont type 1 primes.
   The second is an (unsorted) array of the Pierpont type 2 primes that are less than 10^8.
   The third is an integer: the 57th Pierpont type 1 prime."
  (format *trace-output* "~&Finding small Pierponts.~%")
  (let* ;; There are precisely 57 Pierpont type 1 primes less than 10^8. I assume that there
      ;; are at least 50 Pierpont type 2 primes below 10^8. But it doesn't matter, since
      ;; I will be filling the second array with more primes later.
      ((limit (expt 10 8))
       (primes (primes-in-range 3 (isqrt limit)))
       ;; No candidates except 2 will be even. There will be some (type 2 where v = 0) that
       ;; are divisible by 3.
       (type-1 (make-array 64
                 :element-type 'fixnum
                 :adjustable t
                 :fill-pointer 0))
       (type-2 (make-array 256
                 :element-type 'integer
                 :adjustable t
                 :fill-pointer 0)))
    ;; There are only 6 type-1 Pierpont primes for which v is 0
    ;; And of these, 2 is the only possible prime of either type
    ;; for which u is 0. We simply add all 5 to type-1 and the
    ;; number 2 to type-2.
    (dolist (zero-v '(2 3 5 17 257 65537))
      (vector-push-extend zero-v type-1))
    (vector-push-extend 2 type-2)
    ;; For type 2 primes, I don't know if there is
    ;; a finite number of examples for which v = 0.
    ;; I do know that the smallest such number that is
    ;; prime occurs for u = 2. (with u = 0, we get 0 and
    ;; u = 1 we get 1. But u = 2 gives us 3.
    ;; I will search all possible candidates, then test them
    ;; for primality.
    (let ((temp (make-array 32
                            :element-type 'fixnum
                            :fill-pointer 0)))
      (do*  ((u 2 (1+ u))
             (zero-v #2=(1- (expt 2 u)) #2#))
            ((> zero-v limit))
        (vector-push zero-v temp))
      (setf type-2
            (extract-primes-and-push temp primes type-2)))
    ;; Now, find all other candidates for both types
    (let ((temp-1 #3=(make-array 256
                                 :element-type 'fixnum
                                 :fill-pointer 0
                                 :adjustable t))
          (temp-2 #3#))
      (do* ((u 1 (1+ u))
            (two-power (expt 2 u) (expt 2 u)))
           ((> (1- (* 3 two-power)) limit))
        (do* ((v 1 (1+ v))
              (three-power (expt 3 v) (expt 3 v))
              (type-1-candidate #4=(1+ (* two-power three-power)) #4#)
              (type-2-candidate #5=(- type-1-candidate 2) #5#))
             ((> type-2-candidate limit))
          (vector-push-extend type-1-candidate temp-1)
          (vector-push-extend type-2-candidate temp-2)))
      (setf type-1 (extract-primes-and-push temp-1 primes type-1))
      (setf type-2 (extract-primes-and-push temp-2 primes type-2)))
    (sort type-1 #'<)
    (let ((57th-type-1 (last-elt type-1)))
      ;; The type-1 array should have 57 elements. We need only 50. Make the others "invisible."
      (setf (fill-pointer type-1) 50)
      ;; I am not done with the type-2 array yet. It is too early to sort or truncate it.
      (values type-1 type-2 57th-type-1))))

(defun extract-primes-and-push (candidates primes results)
  "Takes 3 arrays, all containing numbers. CANDIDATES could contain numbers
   of any type. PRIMES is an array of prime numbers in order. RESULTS should
   already contain prime numbers (unsorted) when received. The function finds
   all prime numbers in CANDIDATES and adds them to RESULTS. Returns RESULTS."
  (do-each (cand candidates results)
    (let ((cand-sqrt (isqrt cand)))
      (block inner-loop
        (do-each (p primes (vector-push-extend cand results))
          ;; If the next prime number in PRIMES is greater than the square root of the candidate,
          ;; the candidate must be prime. If candidate is 2 or 3, all the primes will be greater than
          ;; the square root of the candidate.
          (when (> p cand-sqrt)
            (vector-push-extend cand results)
            (return-from inner-loop))
          ;; Candidate cannot be equal to the prime, since the prime must be
          ;; less than or equal to the square root of the candidate to come under
          ;; consideration. So if prime divides candidate, the candidate must be a
          ;; multiple of the prime, and thus, composite.
          (when (zerop (mod cand p))
            (return-from inner-loop)))))))

(defun find-250th-type-1 (57th-type-1)
  "Finds the 250th Pierpont prime type-1. Takes an
   integer: the 57th Pierpont type 1 prime (The largest
   such prime less than 10^8). Returns an 
   integer: the 250th such prime."
  (format *trace-output* "~&Finding 250th type 1.~%")
  ;; There are precisely 250 Pierpont type-1 primes less than 10^32.
  ;; We need to find the largest Pierpont prime type 1 that is less than 10^32.
  (let ((limit (expt 10 32))
        ;; big-type-1 will hold our answer. For now, it is set to 2.
        (big-type-1 57th-type-1))
    ;; Start with the smallest possible threes-part: 3.
    (do ((threes-part 3 (* threes-part 3)))
        ((>= (1+ (* 2 threes-part)) limit) big-type-1)
      (block inner-loop
        ;; We start with the largest possible twos-part such that multiplying twos-part by
        ;; threes-part and adding 1 is less than 10^32. Then we
        ;; bit-shift the twos-part left by 1 on each loop. This is the
        ;; same as, but faster than, dividing it by 2.
        (do* ((u (floor (log (/ (1- limit) threes-part) 2)) (1- u))
              (twos-part (expt 2 u) (ash twos-part -1))
              (candidate #1=(1+ (* twos-part threes-part)) #1#))
             ;; This limit should never be reached. It is here as a guard against an
             ;; infinite loop in the case that the logic of the function
             ;; is unsound or there is some other error.
             ((zerop u))
          ;; If this candidate is smaller than the current big-type-1, stop the inner loop. 
          (when (< candidate big-type-1)
            (return-from inner-loop))
          ;; If this candidate is prime, set it as the new big-type-1 and exit the inner loop.
          (when (primep candidate)
            (setf big-type-1 candidate)
            (return-from inner-loop)))))))

(defparameter *upper-limit* (expt 10 34))


(defun add-rest-type-2 (results)
  (format *trace-output* "~&Finding larger type 2's~%")
  (let ((lower-limit (expt 10 8)))
    (do ((twos-part 1 (ash twos-part 1)))
        ((> (* 3 twos-part) *upper-limit*))
      ;; Start inner loop with a threes-part that
      ;; is sufficiently large so that the type-2 prime
      ;; will be greater than 10^8
      (do* ((v (if (< twos-part lower-limit)
                   (ceiling (log (/ (1+ lower-limit) twos-part) 3))
                   0)
               (1+ v))
            (threes-part (expt 3 v) (expt 3 v))
            (candidate #2=(1- (* twos-part threes-part))
                       #2#))
           ((> candidate *upper-limit*))
        (when (primep candidate)
          (vector-push-extend candidate results)))))
  (assert (>= (length results) 250))
  (sort results #'<)
  (setf (fill-pointer results) 250)
  (format *trace-output* "~&Finished~%")
  results)

(defun display-results (first-50-type-1 first-50-type-2 250th-type-1 250th-type-2)
  (format t "~&~%Pierpont primes type 1:")
  (do ((n 1 (1+ n))
       (i 0 n))
      ((= i 50))
    (format t "~%#~D: ~D" n (aref first-50-type-1 i)))
  (format t "~%.~%.~%.~%#250: ~D" 250th-type-1)
  (format t "~&~%Pierpont primes type 2:")
  (do ((n 1 (1+ n))
       (i 0 n))
      ((= i 50))
    (format t "~%#~D: ~D" n (aref first-50-type-2 i)))
  (format t "~%.~%.~%.~%#250: ~D~%" 250th-type-2))

#|

PIERPONT-PRIMES> (pierponts)
Finding small Pierponts.
Finding 250th type 1.
Finding larger type 2's
Finished

Pierpont primes type 1:
#1: 2
#2: 3
#3: 5
#4: 7
#5: 13
#6: 17
#7: 19
#8: 37
#9: 73
#10: 97
#11: 109
#12: 163
#13: 193
#14: 257
#15: 433
#16: 487
#17: 577
#18: 769
#19: 1153
#20: 1297
#21: 1459
#22: 2593
#23: 2917
#24: 3457
#25: 3889
#26: 10369
#27: 12289
#28: 17497
#29: 18433
#30: 39367
#31: 52489
#32: 65537
#33: 139969
#34: 147457
#35: 209953
#36: 331777
#37: 472393
#38: 629857
#39: 746497
#40: 786433
#41: 839809
#42: 995329
#43: 1179649
#44: 1492993
#45: 1769473
#46: 1990657
#47: 2654209
#48: 5038849
#49: 5308417
#50: 8503057
.
.
.
#250: 62518864539857068333550694039553

Pierpont primes type 2:
#1: 2
#2: 3
#3: 5
#4: 7
#5: 11
#6: 17
#7: 23
#8: 31
#9: 47
#10: 53
#11: 71
#12: 107
#13: 127
#14: 191
#15: 383
#16: 431
#17: 647
#18: 863
#19: 971
#20: 1151
#21: 2591
#22: 4373
#23: 6143
#24: 6911
#25: 8191
#26: 8747
#27: 13121
#28: 15551
#29: 23327
#30: 27647
#31: 62207
#32: 73727
#33: 131071
#34: 139967
#35: 165887
#36: 294911
#37: 314927
#38: 442367
#39: 472391
#40: 497663
#41: 524287
#42: 786431
#43: 995327
#44: 1062881
#45: 2519423
#46: 10616831
#47: 17915903
#48: 18874367
#49: 25509167
#50: 30233087
.
.
.
#250: 4111131172000956525894875083702271
NIL

On my initial attempt, the 250th type-2 was
wrong because my upper cutoff for the search 
was too low and I had not run an assertion that
there be at least 250 elements in the corresponding
array.

There was also a bit of bad logic. I had started the
v's at 1. They should have started at 0.

It turned out that this ran MUCH quicker than expected.
I could have saved a lot of effort by writing a less efficient
program. (Find all 250 of both sequences separately.)
|#

