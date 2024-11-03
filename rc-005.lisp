;;;; rosetta/rc-005.lisp

(in-package :ros-01)

#| Order two numerical lists
   
Tasks in this category have to do with 
sorting collections of objects. 
        
Write a function that orders two lists or 
arrays filled with numbers. The function 
should accept two lists as arguments and 
return true if the first list should be 
ordered before the second, and false 
otherwise.

The order is determined by lexicographic 
order: Comparing the first element of each 
list. If the first elements are equal, then 
the second elements should be compared, and 
so on, until one of the list has no more 
elements. If the first list runs out of 
elements the result is true. If the second 
list or both run out of elements the result 
is false.
|#

;; Lists or arrays could be used for this
;; purpose. We could build one function
;; that works for both, but it would be
;; inefficient with lists. Or we could
;; restrict ourselves to one or another.
;; Which is too limiting. Instead, let's
;; build a generic function that does
;; both efficiently.

;; this is called if both args are lists
(defmethod seq-less-than ((seq-a list)
                          (seq-b list))
  (labels ((rec (rest-a rest-b)
             (cond
              ((null rest-b)
               nil)
              ((null rest-a)
               t)
              ((< (first rest-a)
                  (first rest-b))
               t)
              ((> (first rest-a)
                  (first rest-b))
               nil)
              (t
               (rec (rest rest-a)
                    (rest rest-b))))))
    (rec seq-a seq-b)))
    
;; this is called if both args are vectors:
;; 1-dimensional arrays

(defmethod seq-less-than ((seq-a vector)
                          (seq-b vector))
  (loop 
    for a-elt across seq-a
    for b-elt across seq-b
    do
    (cond
     ((< a-elt b-elt)
      (return-from seq-less-than t))
     ((> a-elt b-elt)
      (return-from seq-less-than nil)))
    finally
    (return-from seq-less-than
      (< (length seq-a) (length seq-b)))))

;; Finally, if we want to mix arg types:

(defmethod seq-less-than (seq-a seq-b)
  (let* ((len-a (length seq-a))
         (len-b (length seq-b))
         (len-min (min len-a len-b)))
    (dotimes (i len-min (< len-a len-b))
      (let ((elt-a (elt seq-a i))
            (elt-b (elt seq-b i)))
        (cond
         ((< elt-a elt-b)
          (return-from seq-less-than t))
         ((> elt-a elt-b)
          (return-from seq-less-than nil)))))))
  
(defun slt-main ()
  (let* ((list-a '(1 2 3 4 5))
         (list-b '(5 4 3 2 1))
         (list-c '(1 2 3))
         (vector-a #(1 2 3 4 5))
         (vector-b #(5 4 3 2 1))
         (vector-c #(1 2 3))
         (sequences
          (list
           list-a list-b list-c
           vector-a vector-b vector-c)))
    (dolist (seq-a sequences (values))
      (dolist (seq-b sequences)
        (format t "~&(seq-less-than ~A ~A) => ~A~%"                                      
          seq-a 
          seq-b 
          (seq-less-than seq-a seq-b))))))

#|

"(1 2 3 4 5) \"<\" (1 2 3 4 5)?: NIL
(1 2 3 4 5) \"<\" (5 4 3 2 1)?: T
(1 2 3 4 5) \"<\" (1 2 3)?: NIL
(1 2 3 4 5) \"<\" #(1 2 3 4 5)?: NIL
(1 2 3 4 5) \"<\" #(5 4 3 2 1)?: T
(1 2 3 4 5) \"<\" #(1 2 3)?: NIL
(5 4 3 2 1) \"<\" (1 2 3 4 5)?: NIL
(5 4 3 2 1) \"<\" (5 4 3 2 1)?: NIL
(5 4 3 2 1) \"<\" (1 2 3)?: NIL
(5 4 3 2 1) \"<\" #(1 2 3 4 5)?: NIL
(5 4 3 2 1) \"<\" #(5 4 3 2 1)?: NIL
(5 4 3 2 1) \"<\" #(1 2 3)?: NIL
(1 2 3) \"<\" (1 2 3 4 5)?: T
(1 2 3) \"<\" (5 4 3 2 1)?: T
(1 2 3) \"<\" (1 2 3)?: NIL
(1 2 3) \"<\" #(1 2 3 4 5)?: T
(1 2 3) \"<\" #(5 4 3 2 1)?: T
(1 2 3) \"<\" #(1 2 3)?: NIL
#(1 2 3 4 5) \"<\" (1 2 3 4 5)?: NIL
#(1 2 3 4 5) \"<\" (5 4 3 2 1)?: T
#(1 2 3 4 5) \"<\" (1 2 3)?: NIL
#(1 2 3 4 5) \"<\" #(1 2 3 4 5)?: NIL
#(1 2 3 4 5) \"<\" #(5 4 3 2 1)?: T
#(1 2 3 4 5) \"<\" #(1 2 3)?: NIL
#(5 4 3 2 1) \"<\" (1 2 3 4 5)?: NIL
#(5 4 3 2 1) \"<\" (5 4 3 2 1)?: NIL
#(5 4 3 2 1) \"<\" (1 2 3)?: NIL
#(5 4 3 2 1) \"<\" #(1 2 3 4 5)?: NIL
#(5 4 3 2 1) \"<\" #(5 4 3 2 1)?: NIL
#(5 4 3 2 1) \"<\" #(1 2 3)?: NIL
#(1 2 3) \"<\" (1 2 3 4 5)?: T
#(1 2 3) \"<\" (5 4 3 2 1)?: T
#(1 2 3) \"<\" (1 2 3)?: NIL
#(1 2 3) \"<\" #(1 2 3 4 5)?: T
#(1 2 3) \"<\" #(5 4 3 2 1)?: T
#(1 2 3) \"<\" #(1 2 3)?: NIL
"  
|#

#| Multifactorial
   
The factorial of a number, written as n!, is 
defined as n! = n(n − 1)(n − 2)...(2)(1).
        
Multifactorials generalize factorials as follows:

    n ! = n ( n − 1 ) ( n − 2 ) . . . ( 2 ) ( 1 ) {\displaystyle n! = n(n-1)(n-2)...(2)(1)}
    n ! ! = n ( n − 2 ) ( n − 4 ) . . . {\displaystyle n!! = n(n-2)(n-4)...}
    n ! ! ! = n ( n − 3 ) ( n − 6 ) . . . {\displaystyle n!! ! = n(n-3)(n-6)...}
    n ! ! ! ! = n ( n − 4 ) ( n − 8 ) . . . {\displaystyle n!! !! = n(n-4)(n-8)...}
    n ! ! ! ! ! = n ( n − 5 ) ( n − 10 ) . . . {\displaystyle n!! !! ! = n(n-5)(n-10)...}

In all cases, the terms in the products are 
positive integers.

If we define the degree of the 
multifactorial as the difference in 
successive terms that are multiplied 
together for a multifactorial (the number of 
exclamation marks), then the task is twofold:

    Write a function that given n and the 
    degree, calculates the multifactorial.
    
    Use the function to generate and display 
    here a table of the first ten members 
    (1 to 10) of the first five degrees of 
    multifactorial.

Note: The wikipedia entry on multifactorials 
gives a different formula. This task uses 
the Wolfram mathworld definition. 
|#

(defun multifactorial (n degree)
  (assert (and (integerp n)
               (plusp n)))
  (assert (and (integerp degree)
               (plusp degree)))
  (do* ((multiplier n (- multiplier degree))
        (result n (* multiplier result)))
       ((<= multiplier degree) result)))

(defun multimain ()
  (do ((degree 1 (1+ degree)))
      ((> degree 5))
    (format t "~%~%Degree ~D:~%" degree)
    (do ((mem 1 (1+ mem)))
        ((> mem 10))
      (format t "~%~D: ~D~%"
              mem (multifactorial mem degree)))))
  
#|


Degree 1:

1: 1

2: 2

3: 6

4: 24

5: 120

6: 720

7: 5040

8: 40320

9: 362880

10: 3628800


Degree 2:

1: 1

2: 2

3: 3

4: 8

5: 15

6: 48

7: 105

8: 384

9: 945

10: 3840


Degree 3:

1: 1

2: 2

3: 3

4: 4

5: 10

6: 18

7: 28

8: 80

9: 162

10: 280


Degree 4:

1: 1

2: 2

3: 3

4: 4

5: 5

6: 12

7: 21

8: 32

9: 45

10: 120


Degree 5:

1: 1

2: 2

3: 3

4: 4

5: 5

6: 6

7: 14

8: 24

9: 36

10: 50
|#

                        