;;;; Hickerson Series of Almost Integers

#| The following function, due to D. Hickerson, is said to generate "Almost integers" by the "Almost
Integer" page of Wolfram MathWorld, (December 31 2013). (See formula numbered 51.) 

The function is:

h(n) = n!/(2(ln 2)^(n + 1))

It is said to produce "almost integers" for   n   between   1   and   17. The purpose of the task is
to verify this assertion.

Assume that an "almost integer" has either a nine or a zero as its first digit after the decimal
point of its decimal string representation.

Task

Calculate all values of the function checking and stating which are "almost integers".

Note: Use extended/arbitrary precision numbers in your calculation if necessary to ensure you have
adequate precision of results as for example: 

h(18) = 3385534663256845326.39...

|#

(in-package :hickerson)

;;;; Common Lisp has no built-in facility for arbitrary-precision floats.
;;;; The precision of floats in Common Lisp varies by implementation.
;;;; In SBCL, the implementation employed here, the maximum float precision
;;;; corresponds to a C double. In practice, this is almost never an issue,
;;;; because arbitrary-precision ratios are a primitive type. In this case,
;;;; however, we really do need a float with more precision than is available
;;;; out of the box. The community standard package for arbitrary-precision
;;;; floats is the COMPUTABLE-REALS project. I have imported the following
;;;; symbols from that library into the :hickerson package:
;;;; (The computable-reals float type is called a CREAL.)

;;;; *PRINT-PREC* : global variable; stores the desired print precision
;;;; *CREAL-TOLERANCE* : global variable; stores the desired tolerance
;;;; *R : function; multiplication for creal's. Takes zero or more numbers. Returns a creal.
;;;;      If no args are supplied, returns 1. If one arg x is supplied,
;;;;      returns x. Otherwise, for args x1, x2, x3, ..., xn, returns the
;;;;      product x1 * x2 * x3 * ... * xn.
;;;;      Corresponds to CL *.
;;;; /R : function; division for creal's. Takes one or more numbers. Returns one creal.
;;;;      If only one arg x is given, 1/x is returned and x != 0. Otherwise, given args
;;;;      x1, x2, x3, ... xn, the quotient x1 / x2 / x3 / ... / xn is returned, and no arg except
;;;;      the first may be zero. Equivalent to CL /.
;;;; +LOG2-R+ : constant; A creal: the natural logarithm of 2, to arbitrary precision.
;;;; EXPT-R : Takes two numbers, x and y. Returns one creal: x^y. Equivalent to CL EXPT. If x = 0 then y != 0.
;;;;          If y = 0, x != 0.
;;;; TRUNCATE-R : function; Corresponds to CL TRUNCATE. With one creal arg, truncates the
;;;;    arg to the integer closest to zero. With two creal args d1 and d2, returns the
;;;;    truncation of d1/d2. Two creal values are always returned: the truncation
;;;;    (rounding to the nearest integer in the direction of 0), and the remainder.
;;;; MAKE-REAL : function: takes a function with arg k. Returns a creal.
;;;.    creals are calculared for some result x, we are seeking a rule of
;;;;    computation that computes a number a such that abs((2^k)*x - a) <= 1 for any
;;;;    k > 0. Thus, to our function should return (2^k)*x


(setf *print-prec* 30)

(defun hickerson-main ()
  "Entry point of the program. Solves the problem given in the task description
   and prints the results."
  (do ((i 1 (1+ i)))
      ((> i 17))
    (let* ((hickerson (calculate-hickerson i))
           (tenth-digit (calculate-tenth-digit hickerson))
           (fulfills-criterion-p (zero-or-nine-p tenth-digit)))
      (format t "~%h(~D) = ~A" i hickerson)
      (format t "~%The digit after the decimal point is ~D" tenth-digit)
      (format t "~%h(~D) ~A~%"
              i
              (if fulfills-criterion-p
                  "IS an almost integer"
                  "IS NOT an almost integer")))))

(defun calculate-hickerson (n)
  "Given integer N, returns a creal: the Nth element of the Hickerson series."
  (/r (factorial-r n)
      (*r 2 (expt-r +log2-r+ (1+ n)))))

(defun factorial-r (n)
  "Given a non-negative integer N, returns N! as a creal."
  (assert
   (and (integerp n)
        (not (minusp n))))
  (if (zerop n)
      (make-real (lambda (k)
                   (* (expt 2 k) 1)))
      (do ((result 1 (* result i))
           (i 2 (1+ i)))
          ((> i n) (make-real #'(lambda (k)
                                  (* (expt 2 k) result)))))))
(defun calculate-tenth-digit (creal)
  "Given a creal CREAL returns an integer which is the place value of
   of the 10ths column of CREAL."
  (let ((fractional-part (nth-value 1 (truncate-r creal))))
    (truncate-r (*r fractional-part 10))))

(defun zero-or-nine-p (n)
  "Given number N, returns t if N is in the set {0, 9}, nil otherwise"
  (or
   (zerop n)
   (= n 9)))

#|
HICKERSON> (hickerson-main)

h(1) = +1.040684490502803898934790801867...
The digit after the decimal point is 0
h(1) IS an almost integer

h(2) = +3.002780707156905443499767407219...
The digit after the decimal point is 0
h(2) IS an almost integer

h(3) = +12.996290505276966462224884542964...
The digit after the decimal point is 9
h(3) IS an almost integer

h(4) = +74.998735447661600127634550375644...
The digit after the decimal point is 9
h(4) IS an almost integer

h(5) = +541.001518516423507569202774618256...
The digit after the decimal point is 0
h(5) IS an almost integer

h(6) = +4683.001247262257437180467152479009...
The digit after the decimal point is 0
h(6) IS an almost integer

h(7) = +47292.998731314623904822835487786309...
The digit after the decimal point is 9
h(7) IS an almost integer

h(8) = +545834.997907485167067291039794492363...
The digit after the decimal point is 9
h(8) IS an almost integer

h(9) = +7087261.001622899120979187515908220487...
The digit after the decimal point is 0
h(9) IS an almost integer

h(10) = +102247563.005271042011088388594199470232...
The digit after the decimal point is 0
h(10) IS an almost integer

h(11) = +1622632572.997550049852874861078499079054...
The digit after the decimal point is 9
h(11) IS an almost integer

h(12) = +28091567594.981572440715189176099525798491...
The digit after the decimal point is 9
h(12) IS an almost integer

h(13) = +526858348381.001248286180489380836208516395...
The digit after the decimal point is 0
h(13) IS an almost integer

h(14) = +10641342970443.084531927095070150392639196205...
The digit after the decimal point is 0
h(14) IS an almost integer

h(15) = +230283190977853.037436039125977106831038630748...
The digit after the decimal point is 0
h(15) IS an almost integer

h(16) = +5315654681981354.513076743456805577995390173051...
The digit after the decimal point is 5
h(16) IS NOT an almost integer

h(17) = +130370767029135900.457985349196773630223904940013...
The digit after the decimal point is 4
h(17) IS NOT an almost integer
NIL
HICKERSON> 

|#
