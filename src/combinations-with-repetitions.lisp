;;;; Combinations With Repetitions

(eval-when (:compile-toplevel
            :load-toplevel
            :evaluate)
  (defpackage :combs-with-reps
    (:use :cl)
    (:export
     :combinations-with-repetitions
     :multiset-coefficie)))

(in-package :combs-with-reps)

(defun find-next-indices (prev last-idx)
  (if 
   (every
    #'(lambda (idx)
        (eql idx last-idx))
    prev)
   nil
   (iter
    (with next = (copy-seq prev))
    (with end-next = (1- (length next)))
    (for i from end-next downto 0)
    (when (< (aref next i) last-idx)
      (incf (aref next i))
      (iter
       (for j from (1+ i) to end-next)
       (setf (aref next j)
             (aref next (1- j))))
      (return-from find-next-indices next)))))
           
(defun combinations-with-repetitions (set k)
  "Given a list ``set'' and a non-negative
  integer ``k'', return a list of all
  k-sized combinations with repetition
  from the set."
  (assert
   (and (integerp k)
        (not (minusp k))))
  (cond
   ((or (null set)
        (zerop k))
    nil)
   ((= k 1)
    (mapcar #'list set))
   (t
    (iter
      (with set-vec = (coerce set 'vector))
      (with last-idx = (1- (length set-vec)))
      (for indices
             initially
               (make-array k
                 :initial-element 0)
             then
               (find-next-indices
                indices last-idx))
      (while indices)
      (collect
       (map 'list
         #'(lambda (idx)
             (aref set-vec idx))
           indices))))))

(defun factorial (n)
  "Factorial function for use in the
  multiset coefficient function."
  (assert
   (and (integerp n)
        (not (minusp n))))
  (if (zerop n)
      1
      (apply 
        #'*
        (iota n :start 1))))

(defun multiset-coefficient (n k)
  "Given integers n and k, return the
  number of k-length distinct combinations
  with repetition that can be made with n distinct
  elements from which to choose."
  (if (or (zerop n)
          (zerop k))
      0
      (/
        (factorial (+ n (1- k)))
        (* (factorial k)
           (factorial (1- n))))))
