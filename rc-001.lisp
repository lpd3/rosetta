;;;; rc-001.lisp: The Power Series problem. 
;;;; A whole file is dedicated to this 
;;;; because it involves shadowing the 
;;;; basic arithmetic functions.

(in-package :power-series)
            
#| Formal Power Series
   
A power series is an infinite sum of the form 
language you may know.

a0 + a1 ⋅ x + a2 ⋅ x^2 + a3 ⋅ x^3 + ⋯ 


The a[i] are called the coefficients of the 
series. Such sums can be added, multiplied 
etc., where the new coefficients of the 
powers of x are calculated according to the 
usual rules.

If one is not interested in evaluating such 
a series for particular values of x, or in 
other words, if convergence doesn't play a 
role, then such a collection of coefficients 
is called formal power series. It can be 
treated like a new kind of number.

Task: Implement formal power series as a 
numeric type. Operations should at least 
include addition, multiplication, division 
and additionally non-numeric operations like 
differentiation and integration (with an 
integration constant of zero). Take care 
that your implementation deals with the 
potentially infinite number of coefficients.

As an example, define the power series of 
sine and cosine in terms of each other using 
 integration.

Goals: Demonstrate how the language handles 
new numeric types and delayed (or lazy) 
evaluation. 
           
|#
 
;; The series package attempts to turn all
;; series objects into compile-time loops.
;; When series objects are stored in 
;; data structures, this is not 
;; possible. Since we are principally 
;; interested in the laziness of series 
;; objects, and not optimization, we shut 
;; off the warnings 

(setf *suppress-series-warnings* t)       
        
(defclass fps ()
  ((contents
    :accessor cont
    :initarg :cont
    :initform (scan-range)
    :type (series number)
    :documentation
    "A SERIES object, it contains the
    coefficients of the fps"))
  (:documentation
   "Represents a formal power series."))

(defun fpsp (obj)
  "Is this an fps?"
  (typep obj 'fps))
  
(defun fps (fn)
  "Simplified constructor for fps. Takes
  a function which describes the behavior
  of the coefficients"
  (make-instance 'fps
    :cont
    (map-fn t
      #'(lambda (n)
          (values
            (funcall fn n)))
        (scan-range))))

(defmethod print-object ((obj fps) stream)
  (print-unreadable-object (obj stream :type t)
     (destructuring-bind
          (a0 a1 a2 a3)
          (collect
           (subseries
            (cont obj)
            0 4))
       (format stream
         "~%~A + ~AX + ~AX^2 + ~AX^3 + ..."
         a0 a1 a2 a3))))

(defun extract (index object)
  "Given an index and an fps, return 
  the coefficient at that index"
  (collect-nth index (cont object)))

(defun extended-number-p (obj)
  "Returns T if the argument is a native
  Common Lisp number or an fps. Returns
  NIL otherwise."
  (or (numberp obj)
      (fpsp obj)))

;;; The native Lisp types cannot be 
;;; subclassed. Moreover, "overriding operators"
;;; is not an idiomatic Lisp technique.
;;; Nevertheless, the functionality of the 
;;; task is achievable. We note that basic
;;; arithmeric "operators" are not 
;;; operators in Lisp, but rather, ordinary
;;; functions. The first thing to do 
;;; is to shadow the functions we need.
;;; This happens in the package definition,
;;; where all symbols of Common Lisp are 
;;; directly imported except +, -, *, /, and
;;; expt. We must recreate the operation of
;;; these functions on numbers. Note that
;;; Common Lisp accepts + and * with zero or 
;;; more arguments. CL accepts - and / with
;;; one or more arguments.
                         
(defgeneric generic-+ (x y)
  (:documentation "Handles addition of 
    numbers and formal power series."))

(defun + (&rest xs)
  "In Common Lisp, we can call + with 
  no arguments. 0 is returned. So we begin
  there.
  We need this ``gateway function''
  because generic functions cannot 
  specialize on optional parameters."
  (case (length xs)
    (0 0)
    (1 (car xs))
    (2 (generic-+ (car xs) (cadr xs)))
    (otherwise
     (apply
      #'+
      (generic-+ (car xs) (cadr xs))
      (cddr xs)))))

(defgeneric negate (x)
  (:documentation
   "Negation extended to fps."))

(defgeneric generic-- (x y)
  (:documentation "Extends subtraction
    to formal power series."))
 
(defun - (x &rest xs)
  (case (length xs)
    (0 (negate x))
    (1 (generic-- x (car xs)))
    (otherwise
     (apply
      #'-
      (generic-- x (car xs))
      (cdr xs)))))   


(defgeneric generic-* (x y)
  (:documentation "Extends multiplication
    to formal power series."))
   
(defun * (&rest xs)
  "Common Lisp accepts * with no 
  arguments. 1 is returned. We need this
  ``gateway function'' because Common 
  Lisp generic functions cannot specialize
  on optional parameters."
  (case (length xs)
    (0 1)
    (1 (etypecase (car xs)
         (number (car xs))
         (fps (car xs))))
    (2 (generic-* (car xs) (cadr xs)))
    (otherwise
     (apply
      #'*
      (generic-* (car xs) (cadr xs))
      (cddr xs)))))

(defgeneric invert (x)
  (:documentation
   "Inverse extended to fps."))

(defgeneric generic-/ (x y)
  (:documentation 
   "Division of two arguments, extended
    to formal power series."))

(defun / (x &rest xs)
  (case (length xs)
    (0 (invert x))
    (1 (generic-/ x (car xs)))
    (otherwise
     (apply
      #'/
      (generic-/ x (car xs))
      (cdr xs)))))

(defgeneric expt (base exponent)
  (:documentation
   "Exponentiation extended to formal
   power series."))      
   
(defmethod generic-+ ((x number) (y number))
  "Re-implement + for numbers."
  (cl:+ x y)) ; Invoke the native function.
                                          
(defmethod negate ((x number))
  (cl:- x))
                                          
(defmethod generic-- ((x number) (y number))
  "When both args are cl numbers."
  (cl:- x y))

(defmethod generic-* ((x number) (y number))
  "If both args are numbers, call native
  multiplication."
  (cl:* x y))

(defmethod invert ((x number))
  (cl:/ x))

(defmethod generic-/ ((x number) (y number))
  (cl:/ x y))

(defmethod expt ((base number) (exponent number))
  (cl:expt base exponent))

(defmethod generic-+ ((x fps) (y fps))
  (make-instance 'fps
    :cont
    (map-fn t
     #'(lambda (x-elt y-elt)
         (values
          (cl:+ x-elt y-elt)))
       (cont x)
       (cont y))))

(defmethod negate ((x fps))
  (make-instance 'fps
    :cont
    (map-fn t
      #'(lambda (x-elt)
          (values (cl:- x-elt)))
        (cont x))))

(defmethod generic-- ((x fps) (y fps))
  (make-instance 'fps
    :cont
    (map-fn t
      #'(lambda (x-elt y-elt)
          (values
           (cl:- x-elt y-elt)))
        (cont x)
        (cont y))))
  
(defmethod generic-* ((x fps) (y number))
  (make-instance 'fps
    :cont
    (map-fn t
      #'(lambda (n)
          (values
            (* n y)))
        (cont x))))

(defmethod generic-* ((x number) (y fps))
  (generic-* y x))

(defmethod generic-* ((x fps) (y fps))
  "Naive solution. Quadratic time. Faster
  algorithms are available."
  (make-instance 'fps
    :cont
    (map-fn t
     #'(lambda (index)
         (apply
          #'cl:+
          (collect
           (map-fn t
            #'(lambda (x-inner-index)
                (* (extract x-inner-index x)
                   (extract
                    (- index x-inner-index)
                    y)))
              (scan-range 
                :upto index)))))
       (scan-range))))

(defmethod invert ((x fps))
  (let ((a0 (collect-first (cont x))))
    (assert (not (zerop a0))
      ()
      "A formal power series whose a0 = 0 ~%~
      is not invertible.")
    (let ((inv-a0 (cl:/ a0))
          (neg-inv-a0 (cl:- (cl:/ a0)))
          (prev-dict (dict)))
      (setf 
        (gethash 0 prev-dict) inv-a0)
      (labels ((rec (idx)
                 (if (zerop idx)
                     inv-a0
                     (let ((dval
                            (gethash idx prev-dict)))
                       (if dval
                           dval
                           (setf
                            (gethash idx prev-dict)
                            (* neg-inv-a0
                               (apply #'+
                                 (mapcar
                                  #'(lambda (inner-idx)
                                      (* (extract inner-idx x)
                                         (rec (1- idx))))
                                    (iota idx :start 1))))))))))
        (make-instance 'fps
          :cont
          (map-fn t
            #'(lambda (idx)
                (rec idx))
              (scan-range)))))))

(defmethod generic-/ ((x fps) (y number))
  (if (zerop number)
      (error
       "Division of a fps by zero")
      (* x (cl:/ y))))

(defmethod generic-/ ((x fps) (y fps))
  (assert
   (not (zerop (extract 0 y)))
   ()
   "Cannot divide an fps by an fps whose ~%~
   a0 i= 0.")
  (* x (invert y)))

(defmethod expt ((x fps) (y integer))
  (cond
   ((minusp y)
    (expt (invert x) (cl:- y)))
   ((zerop y)
    1)
   (t
    (let ((a0 (extract 0 x)))
      (assert (not (zerop a0))
        ()
        "Cannot perform exponentiation ~%~
        on an fps whose a0 = 0.")
      (let ((a0^y (cl:expt a0 y))
            (inv-a0 (cl:/ a0))
            (prev-dict (dict)))
        (setf (gethash 0 prev-dict) a0^y)
        (labels ((rec (idx)
                   (if (zerop idx)
                       a0^y
                       (let ((c 
                               (gethash idx prev-dict)))
                         (if c
                             c
                             (setf (gethash idx prev-dict)
                               (cl:* 
                                 inv-a0
                                 (cl:/ idx)
                                 (apply
                                  #'cl:+
                                  (mapcar
                                   #'(lambda (inner-idx)
                                       (cl:*
                                        (cl:+
                                         (cl:*
                                          inner-idx
                                          y)
                                         (cl:- idx)
                                         inner-idx)
                                        (extract idx x)
                                        (rec
                                         (- idx
                                            inner-idx))))
                                     (iota idx :start 1))))))))))
          (make-instance 'fps
            :cont
            (map-fn t
              #'(lambda (idx)
                  (values
                   (rec idx)))
              (scan-range)))))))))

(defmethod derivative ((x fps))
  (make-instance 'fps
    :cont
    (map-fn t
      #'(lambda (n)
          (values
           (cl:*
            (extract n x)
            n)))
        (scan-range :from 1))))

(defmethod integral ((x fps))
  (make-instance 'fps
    :cont
    (map-fn t
      #'(lambda (n)
          (if (zerop n)
              (values 0)
              (values
               (cl:/ (extract (1- n) x)
                     n))))
        (scan-range))))  
                        
;;;; Following are the power series for the
;;;; sin and cos functions
                         
(defun factorial (x)
  "Factorial function, used for the 
  formal power series of the sin and cos
  formal power series."
  (assert
   (and (integerp x)
        (not (minusp x))))
  (if (zerop x)
      1
      (apply #'cl:*
        (iota x :start 1))))

(defparameter sin-fps`
  (fps
   #'(lambda (n)
       (if (evenp n)
           0
           (cl:/ (cl:expt -1 (floor n 2))
                 (factorial n)))))
  "Sin function represented as a formal
  power series.")

(defparameter cos-fps
  (fps
   #'(lambda (n)
       (if (oddp n)
           0
           (cl:/ (cl:expt -1 (cl:/ n 2))
                 (factorial n)))))
  "cos function represented as a formal
  power series.")

                   
                                       
                                   
                              
                                      
                                      
                                          
                

                            
 
  

      
                      
             
   
    
                                                                                                                                          