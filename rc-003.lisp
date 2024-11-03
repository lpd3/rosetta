;;;; rc-003.lisp

(in-package :ros-01)

;;;; Stack

#|

A stack is a container of elements with last 
in, first out access policy. Sometimes it 
also called LIFO. 
     
The stack is accessed through its top.

The basic stack operations are:

      push   stores a new element onto the 
             stack top;
      pop   returns the last pushed stack 
            element, while removing it from 
            the stack;
      empty tests if the stack contains no 
            elements.


Sometimes the last pushed stack element is 
made accessible for immutable access (for 
read) or mutable access (for write):

      top   (sometimes called peek to keep 
            with the p theme) returns the 
            topmost element without 
            modifying the stack.


Stacks allow a very simple hardware 
implementation.

They are common in almost all processors.

In programming, stacks are also very popular 
for their way (LIFO) of resource management, 
usually memory.

Nested scopes of language objects are naturally
implemented by a stack (sometimes by 
 multiple stacks).

This is a classical way to implement local 
variables of a re-entrant or recursive 
subprogram. Stacks are also used to 
describe a formal computational framework.

See stack machine.

Many algorithms in pattern matching, 
compiler construction (e.g. recursive 
descent parsers), and machine learning 
(e.g. based on tree traversal) have a 
natural representation in terms of stacks.


Task

Create a stack supporting the basic 
operations: push, pop, empty. |#
            
#| Lisp is an acronym for LISt Processing.
As such, the (singly-linked) list is 
the backbone data structure of the language.
Singly-linked lists are perfect for 
implementing stacks. The stack operators
are built-in in Common Lisp
    
PUSH and POP are built-in functions.
     
There is also CONS, a non-destructive 
"push".

The "empty" function is NULL.
    
The "top" or "peek" function has two 
names: CAR (an historical name) and
FIRST

(defparameter stack '(a b c))
-> STACK
(push 'banana stack)
-> '(BANANA A B C)
(pop stack)
-> BANANA
stack
-> (A B C)
(car stack)
-> A
stack
-> (A B C)
(null stack)
-> NIL
(null ())
-> T
   
|#

;;; Greatest Element of a Set

#| 
  
Create a function that returns the maximum 
value in a provided set of values,
where the number of values may not be known 
until run-time. |#
      
#| 
  
CL, like most modern languages, has a 
built-in MAX function. It takes an 
arbitrary number of real numbers. To
find the maximum of a set (read: list), 
use APPLY and MAX:
    
(max 42 12 -32 4.377 6)
-> 42
(apply #'max '(42 12 -32 4.377 6))
-> 42
|#

;;; Vampire Number

#|

A vampire number is a natural decimal number 
with an even number of digits, that can be 
factored into two integers. 
         
These two factors are called the fangs, and 
must have the following properties:

   they each contain half the number of the 
   decimal digits of the original number
              
   together they consist of exactly the same 
   decimal digits as the original number
              
   at most one of them has a trailing zero


An example of a vampire number and its fangs: 
1260 : (21, 60) 

Task

    Print the first   25   vampire numbers 
    and their fangs.
    
    Check if the following numbers are 
    vampire numbers and, if so, print them 
    and their fangs:

 16758243290880, 
 24959017348650,  
 14593825548650 


Note that a vampire number can have more than 
one pair of fangs. 
    
|#

;; It looks like there is no simple
;; algorithm to find a vampire number;
;; Brute force must be employed. According
;; to online reference, there are 7 4-digit
;; vampire numbers, and 148 6-digit Vampire
;; numbers. This means we will not need to
;; look at more than 6 digits to find 
;; the first 25. We might imagine that
;; permutations could be used. This is 
;; fine for 4-digit numbers (24 permutations),
;; is borderline for 6-digit numbers
;; (720 permutations) and is prohibitive 
;; for 14-digit numbers (81,178,291,200
;; permutations). A smarter approach is 
;; necessary.    

;; Starting with 4-digit numbers, the first
;; integer whose square is a 
;; four-digit number is 32 (1024).
;; The smallest integer that when
;; multiplied by 99 produces a 4-digit 
;; number is 11.
               
;; We need to find the divisors of n. 
;; But not all the divisors. Only those
;; divisors that have half the length of 
;; n. Furthermore, if we find divisors, 
;; we know the quotient, which might be 
;; another apropos divisor. Any checking
;; past the smallest quotient is a waste
;; of time. So the divisor search should
;; be brute force. We could get further gains
;; by weeding out even divisor checks for
;; odd ns, etc. But let's just KISS.

(defun applicable-divisors (n)
  "Returns two lists, one of divisors, the
  other of corresponding quotients. Both
  divisors and quotients meet the following
  criteria: (1) digit count is 1/2 of the digit 
  count of n. (2) At most one of the pair
  ends in at most one zero. Returns two
  nils if no applicable divisors found."
  (assert
   (and (integerp n)
        (not (minusp n))))
  (if (oddp (ceiling (log n 10)))
      nil
      (do* ((candidate (expt 10 (floor (log n 10) 2))
                      (1+ candidate))
            (divisors nil)
            (quotients nil))
           ((if quotients
                (>= candidate (car quotients))
                (>= candidate
                    (expt 10
                      (1+ (floor (log n 10) 2)))))
            (values divisors quotients))
        (when (zerop (mod n candidate))
          (let ((quotient (/ n candidate)))
            (when
              (and 
                (= (ceiling (log candidate 10))
                   (ceiling (log quotient 10)))
                (if (zerop (mod candidate 10))
                    (not (zerop (mod quotient 10)))
                    t)
                (not (zerop (mod candidate 100)))
                (not (zerop (mod quotient 100))))
              (push candidate divisors)
              (push quotient quotients)))))))
              
(defun hash-equal (table1 table2 &key
                   (test #'eql)
                   same-test-p)
  "Tests two hash-tables for equality by
  comparing sets of keys and values. Kwargs
  include :test (defaulting to #'eql) and
  :same-test-p (defaulting to nil), which,
  if true, will also compare hash test 
  functions for equality."
  (when same-test-p
    (when (not (eql (hash-table-test table1)
                    (hash-table-test table2)))
      (return-from hash-equal nil)))
  (and
   (set-equal
    (hash-table-keys table1)
    (hash-table-keys table2)
    :test test)
   (set-equal
    (hash-table-values table1)
    (hash-table-values table2)
    :test test)))
              
(defun fangs (n app-div app-quo)
  (if (null app-div)
      nil
      (iter
       (with n-freq = (frequencies
                        (format nil "~D" n)))
       (for d in app-div)
       (for q in app-quo)
       (let ((composite-freq
               (frequencies
                (format nil "~A~A"
                        (format nil "~D" d)
                        (format nil "~D" q)))))
         (when
           (hash-equal n-freq composite-freq)
           (return-from fangs
             (list n (list d q))))))))

(defun vampire-main ()
  (format t "~%The first 25 vampire numbers with fangs.~%")
  (let ((count 0))
    (iter
      (for n from 1260 to 9999)
      (multiple-value-bind
         (divisors quotients)
         (applicable-divisors n)
       (when-let ((fang-list (fangs n divisors quotients)))
         (format t "~%~A" fang-list)
         (incf count))))
    (iter
     (for n from 100001 to 999999)
     (until (>= count 25))
     (multiple-value-bind
         (divisors quotients)
         (applicable-divisors n)
       (when-let ((fang-list (fangs n divisors quotients)))
         (format t "~%~A" fang-list)
         (incf count))))
    (format t "~%~%For~%~
            16758243290880, ~%~
            24959017348650, ~%~
            and ~%~
            14593825548650, ~%~
            we have:~%")  
    (dolist (n 
              '(16758243290880
                24959017348650
                14593825548650))
      (multiple-value-bind
          (divisors quotients)
          (applicable-divisors n)
        (when-let ((fang-list (fangs n divisors quotients)))
          (format t "~%~A" fang-list))))))
   
#|
"
The first 25 vampire numbers with fangs.

(1260 (21 60))
(1395 (15 93))
(1435 (35 41))
(1530 (30 51))
(1827 (21 87))
(2187 (27 81))
(6880 (80 86))
(8918 (91 98))
(100091 (101 991))
(100192 (101 992))
(100980 (110 918))
(101090 (110 919))
(101136 (301 336))
(102190 (110 929))
(102480 (210 488))
(102510 (201 510))
(102850 (121 850))
(102870 (127 810))
(104260 (260 401))
(104346 (306 341))
(104784 (148 708))
(105123 (201 523))
(105210 (210 501))
(105264 (204 516))
(105266 (206 511))

For
16758243290880, 
24959017348650, 
and 
14593825548650, 
we have:

(16758243290880 (3186540 5259072))
(24959017348650 (4826790 5170935))"
                
|#

;;; Input Loop

#|

Read from a text stream either word-by-word 
or line-by-line until the stream runs out of 
data.

Task

The stream will have an unknown amount of 
data on it.
     
|#

(defun slurp-file (path)
  "Returns a list of lines."
  (with-open-file (f path)
    (do ((lines nil)
         (line #1=(read-line f nil :eof nil)
               #1#))
        ((eq line :eof) (nreverse lines))
      (push line lines))))
      
;;; Largest Int From Concatenated Ints

#|

Given a set of positive integers, write a 
function to order the integers in such a 
way that the concatenation of the numbers 
forms the largest possible integer and 
return this integer. 
       
Task

Use the following two sets of integers as 
 tests and show your program output here.

 {1, 34, 3, 98, 9, 76, 45, 4}
 {54, 546, 548, 60}


Possible algorithms

    A solution could be found by trying all 
    combinations and return the best.
    
    Another way to solve this is to note that 
    in the best arrangement, for any two 
    adjacent original integers X and Y, the 
    concatenation X followed by Y will be 
    numerically greater than or equal to the 
    concatenation Y followed by X.
    
    Yet another way to solve this is to pad 
    the integers to the same size by 
    repeating the digits then sort using 
    these repeated integers as a sort key.|# 
    
(defun sort-ints-for-biggest-concat (num-list)
  (let ((num-dict
         (dict))
        (max-len
         (apply 
           #'max
           (mapcar
             #'(lambda (n)
                 (ceiling (log n 10)))
               num-list))))
    (dolist (n num-list)
      (let* ((log10 (log n 10))
             (len (if (zerop (mod log10 1))
                      (1+ log10)
                      (ceiling log10))))
        (if (= len max-len)
            (setf (gethash n num-dict) n)
            (iter
              (for key initially (* n 10)
                       then (* key 10))
              (for new-len initially (1+ len)
                           then (1+ new-len))
              (until (= new-len max-len))
              (finally
               (setf (gethash n num-dict) key))))))
    (sort num-list 
          #'> 
          :key
          #'(lambda (m)
              (gethash m num-dict)))))

(defun greatest-concatenation (num-list)
  (let ((sorted 
          (sort-ints-for-biggest-concat
           num-list)))
    (nth-value 
      0
      (parse-integer
        (format nil "~{~D~}" sorted)))))
          
;; 989754643431, 6054854654
                          
;;; Roots of a Quadratic Equation

#|

Write a program to find the roots of a 
quadratic equation, i.e., solve the equation 
ax^2 + bx + c = 0. Your program must correctly 
handle non-real roots, but it need not check 
that a ≠ 0.
     
This task has been clarified. Its programming examples are in need of review to ensure that they still fit the requirements of the task.

The problem of solving a quadratic equation 
is a good example of how dangerous it can be 
to ignore the peculiarities of floating-point 
arithmetic. The obvious way to implement the 
quadratic formula suffers catastrophic loss 
of accuracy when one of the roots to be 
found is much closer to 0 than the other. In 
their classic textbook on numeric methods 
Computer Methods for Mathematical 
Computations, George Forsythe, Michael 
Malcolm, and Cleve Moler suggest trying the 
naive algorithm with a = 1, b = −10^5, and 
c = 1. (For double-precision floats, set 
b = −10^9.) Consider the following 
implementation in Ada: 
               
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;  
use Ada.Numerics.Elementary_Functions;

procedure Quadratic_Equation is
   type Roots is array (1..2) of Float;
   function Solve (A, B, C : Float) return Roots is
      SD : constant Float := sqrt (B**2 - 4.0 * A * C);
      AA : constant Float := 2.0 * A;
   begin
      return ((- B + SD) / AA, (- B - SD) / AA);
   end Solve;

   R : constant Roots := Solve (1.0, -10.0E5, 1.0);
begin
   Put_Line ("X1 =" & Float'Image (R (1)) & " X2 =" & Float'Image (R (2)));
end Quadratic_Equation;
#include <stdio.h> int main() { return 0; }
Output:

X1 = 1.00000E+06 X2 = 0.00000E+00

As we can see, the second root has
lost all significant figures. The right 
answer is that X2 is about 10^−6. The naive 
method is numerically unstable.

Suggested by Middlebrook (D-OA), a better 
numerical method: to define two parameters 
q = ac/b and f = 1/2 + sqrt(1−4q^2)/2

and the two roots of the quardratic are: 
(−b/a)f and −c/(bf).
              
Task: do it better. This means that given 
a = 1, b = −10^9, and c = 1, both of the 
roots your program returns should be greater 
than 10^−11. Or, if your language can't do 
floating-point arithmetic any more precisely 
than single precision, your program should 
be able to handle b = −10^6. Either way, 
show what your program gives as the roots 
of the quadratic in question. See page 9 of 
"What Every Scientist Should Know About 
Floating-Point Arithmetic" for a possible 
algorithm. |# 
             
(let ((*read-default-float-format*
       'long-float))     
  (defun quadratic (a b c)
    (assert
      (not (zerop a)))
    (if (zerop b)
        (values
          (sqrt (/ (- c) a))
          (- (sqrt (/ (- c) a))))
        (let* ((q (/ (sqrt (* a c)) b))
               (f (+ 1/2 (/ (sqrt (- 1 (* 4 q q))) 2))))
          (values
            (* (/ (- b) a) f)
            (/ (- c) (* b f)))))))

;;; Jensen's Device

#|
This task is an exercise in call by name. 
     
Jensen's Device is a computer programming 
technique devised by Danish computer 
scientist Jørn Jensen after studying the 
ALGOL 60 Report.

The following program was proposed to 
illustrate the technique. It computes the 
100th harmonic number: 
       
begin
   integer i;
   real procedure sum (i, lo, hi, term);
      value lo, hi;
      integer i, lo, hi;
      real term;
      comment term is passed by-name, and so is i;
   begin
      real temp;
      temp := 0;
      for i := lo step 1 until hi do
         temp := temp + term;
      sum := temp
   end;
   comment note the correspondence between the mathematical notation and the call to sum;
   print (sum (i, 1, 100, 1/i))
end  
    
The above exploits call by name to produce 
the correct answer (5.187...). It depends on 
the assumption that an expression passed as 
an actual parameter to a procedure would be 
re-evaluated in the caller's context every 
time the corresponding formal parameter's 
value was required. If the last parameter to 
sum had been passed by value, and assuming 
the initial value of i were 1, the result 
would have been 100 × 1/1 = 100.

Moreover, the first parameter to sum, 
representing the "bound" variable of the 
summation, must also be passed by name (or 
at least by reference), otherwise changes to 
it (made within sum) would not be visible in 
the caller's context when computing each of 
the values to be added. (On the other hand, 
the global variable does not have to use the 
same identifier, in this case i, as the 
formal parameter.)

Donald Knuth later proposed the Man or Boy 
Test as a more rigorous exercise.    
     
|#       
        
;;; I stole this from the CL implementation
;;; on the rosettacode site. At issue is 
;;; the fact that CL does not have pass by
;;; name. This is simulated by using 
;;; a variable-capturing macro.

(declaim (inline %jensen-sum))

(defun %jensen-sum (lo hi fun)
  (loop
    for i from lo to hi
    sum (funcall fun i)))

(defmacro jensen-sum (i lo hi term)
  `(%jensen-sum ,lo ,hi
      (lambda (,i)
        ,term)))

;; (float (jensen-sum i 1 100 (/ i)) 0l0)
;; -> 5.187377517639620260805117675658253l0

;;; Holidays Related to Easter

#|

Calculate the dates of: 
          
Task

       Easter
       Ascension Thursday
       Pentecost
       Trinity Sunday
       Corpus Christi feast (for Catholic)
       All Saints' Sunday (for Orthodox)


As an example, calculate for the first year 
of each century from;

   years 400 to 2100 CE   
     and for
   years 2010 to 2020 CE.

Note

From the year 325 CE on, Easter Sunday has 
been defined as the first Sunday after the 
first full moon on or after the day of the 
March equinox. However, the actual 
astronomical values for the moments of the 
full moon and equinox are not used. Instead, 
approximations are used, the first one being 
that the equinox is assumed to fall on March 
21st every year. The tracking of the moon 
phases is similarly done with relatively 
straightforward arithmetic (compared to the 
sort required for astronomical accuracy) 
which amounts to maintaining a lunisolar 
calendar in parallel to our standard 
purely-solar one.

When Pope Gregory reformed the Catholic 
calendar in 1582 CE, the drifting of Easter 
with respect to the seasons was the driving 
motivation, and the rules for determining it 
(called the computus) were altered to correct 
that drift. Catholic nations adopted both 
the new calendar and the new computus right 
away, while Western Protestant nations 
adopted them more gradually over the next 
350 years or so. Eventually, even nations 
dominated by the Eastern Orthodox church 
adopted a similar calendar reform (the 
Revised Julian calendar), so pretty much the 
whole world agrees on what day it is for 
civil purposes. But the Eastern churches 
never adopted the corresponding Easter rule 
changes; they still use the original Julian 
calendar and computus to determine the date 
of what is known in the West as "Orthodox 
Easter". Therefore, your output should 
indicate which computus was used to 
calculate the dates and, at least for 
historical dates where the calendar can't
be assumed or is location-dependent, which 
calendar those dates are given in.

You may find algorithms on the Computus 
Wikipedia page. Some of the results:

In the year 400 CE, Easter Sunday was April 
1st (in the contemporary Julian calendar), 
making Ascension Thursday May 10th and 
Pentecost May 20th. It is ahistorical to 
give a date so far back for either Trinity 
Sunday or Corpus Christi, neither of which 
were observed until centuries later, but 
they would have been May 27th and 31st. If 
you extend the modern civil calendar back 
that far, those days are instead assigned 
the subsequent dates: Easter on April 2nd, 
Ascension on May 11th, Pentecost on May 
21st.

Skipping forward to the year 2100 CE, 
assuming the rules don't change between now 
and then, the Western churches will observe 
Easter on March 28, Ascension Thursday May 
6th, Pentecost May 16th, Trinity Sunday May 
23rd and Corpus Christi May 27th. Heading 
East, the Orthodox rules place Easter on 
April 18 in the original Julian calendar; 
the corresponding civil date is May 2nd. 
That puts the Ascension on June 10th and 
Pentecost June 20th. Orthodox Trinity Sunday 
is the same day as Pentecost, but they 
observe All Saints' Sunday the following 
week, June 27th. Corpus Christi is a purely 
Catholic date that has no Orthodox version.

Test values of Easter dates

Year 	Orthodox 	Catholic 	Calendar
400 	01 Apr 	— 	Jul.
800 	19 Apr 	— 	Jul.
1200 	09 Apr 	— 	Jul.
2000 	30 Apr 	23 Apr 	Gr.
2020 	19 Apr 	12 Apr 	Gr.     
     
|# 
  
#|
Modernity
Computus--algorithm for calculating
the date of Easter in Catholicism and
Protestantism:

First Sunday after the Paschal Full Moon, 
which is the first full moon on or after
March 21, an approximation of the vernal 
equinox. |#
         
(defun gregorian-easter (year)
  "System for determining the date of 
  Easter since 1583 in Catholic countries
  and in Protestant countries beginning
  at various years up until 1845,
  by means of an anonymous algorithm that
  appeared in the journal Nature in 1876 
  (Meeus/Jones/Butcher algorithm) and as 
  tidied up by a 1961 article in New
  Scientist. Returns 2 values: day and
  integer (1-12) month."
  (assert (<= 1583 year 2100)
    ()
    "Date ~D out of range (1583-2100) for ~%~
    gregorian-easter."
    year)
  (let* ((a (mod year 19))
         (b (floor year 100))
         (c (mod year 100))
         (d (floor b 4))
         (e (mod b 4)) ; f removed 1961
         (g (floor ; g changed 1961
             (+ (* 8 b) 13)
             25))
         (h (mod
             (+ (* 19 a) b (- d) (- g) 15)
             30))
         (i (floor c 4))
         (k (mod c 4)) ; never j
         (l (mod 
              (+ 32 (* 2 e) (* 2 i) (- h) (- k))
              7))
         (m (floor
              (+ a (* 11 h) (* 19 l))
              433)) ; m changed 1961
         (month 
            (floor
              (+ h l (- (* 7 m)) 90)
              25)) ; n changed 1961
         (day 
           (mod
             (+ h l (- (* 7 m)) (* 33 month) 
                19)
             32))) ; o removed and replaced with p 1961
    (values day month))) ; The day and month
                                     
(defun julian-easter (year)
  "Method of calculating (Orthodox) Easter. 
  Returns two values: day and month. The
  date returned is in the Julian calendar.
  Since the date of Easter was formally
  proclaimed ad hoc before about 440, 
  it will not accept years before then. 
  For years 440 through 1582, the date 
  returned was Easter everywhere. From
  1583 on, the date corresponds with 
  Orthodox Easter, and up unil 1845
  Easter in various protestant 
  countries (the date of adoption
  of the Gregorian calendar was 1583 for
  Catholic countries with Protestant 
  countries changing individually at 
  various times. While even all Eastern 
  countries eventually adopted the 
  Gregorian calendar as a Civil 
  calendar by the end of the 1920s, the 
  Julian calendar was retained for
  religious use. The algorithm is Meeus's
  algorithm."
  (assert (<= 400 year 2100)
    ()
    "Year ~D out of range (400-2100) for ~%~
    julian-easter"
    year)
  (let* ((a (mod year 4))
         (b (mod year 7))
         (c (mod year 19))
         (d (mod
             (+ (* 19 c) 15)
             30))
         (e (mod
             (+ (* 2 a) (+ 4 b) (- d) 34)
             7))
         (month (floor
                 (+ d e 114)
                 31))
         (day (1+
               (mod
                (+ d e 114)
                31))))
    (values day month)))

(defun date->day (day month)
  (case month
    (3 day)
    (4 (+ day 31))
    (5 (+ day 61))
    (6 (+ day 92))
    (7 (+ day 122))))

(defun day->date (day)
  (cond
   ((<= day 31)
    (values day 3))
   ((<= day 61)
    (values (- day 31) 4))
   ((<= day 92)
    (values (- day 61) 5))
   ((<= day 122)
    (values (- day 92) 6))
   ((<= day 153)
    (values (- day 122) 7))
   (t
    (error "Day out of range day->date"))))

(defun date-+ (day month days)
  (day->date 
    (+ (date->day day month) days)))

(defun easter-date (year)
  "Given a year between AD 440 and 2100, 
  returns three values: day, month, 
  and either :julian (440-1582) or
  :gregorian (1583-2100)."
  (assert
   (<= 440 year 2100)
   ()
   "Year ~D out of range (440-2100) ~%~
    for easter-date.")
   (if (< year 1583)
       (multiple-value-bind
           (day month)
           (julian-easter year)
         (values day month :julian))
       (multiple-value-bind
           (day month)
           (gregorian-easter year)
         (values day month :gregorian))))
         
(defun n->month (n)
  (nth n '(nil jan feb mar apr may jun jul
               aug sep oct nov dec)))

(defun easter-main ()
  (format t 
    "1. Easter 2. Orthodox Easter 3. Ascension ~%~
    4. Orthodox Ascension 5. Pentecost 6. Orthodox Pentecost ~%~
    7. Trinity Sunday 8. Orthodox Trinity ~%~
    9. Corpus Christi (Western only) ~%~
    10. All Saints Sunday (Orthodox Only)")
  (iter
   (with years = (append
                  (iota 17 :start 500
                    :step 100)
                  (iota 11 :start 2010)))
   (for year in years)
   (for (values day month) = (easter-date year))
   (for (values jday jmonth) = (julian-easter year))
   (for (values aday amonth) = (date-+ day month 39))
   (for (values oaday oamonth) = (date-+ jday jmonth 39))
   (for (values pday pmonth) = (date-+ day month 49))
   (for (values opday opmonth) = (date-+ jday jmonth 49))
   (for (values tday tmonth) = (date-+ day month 56))
   (for (values otday otmonth) = (date-+ jday jmonth 56))
   (for (values ccday ccmonth) = (date-+ day month 60))
   (for (values asday asmonth) = (date-+ jday jmonth 73))
   (format t "~%~%1. ~A ~D, ~D; ~A"
     (n->month month) day year
     (if (< year 1583)
         "Jul." "Gre."))
   (format t "~%2. ~A ~D, ~D; Jul."
     (n->month jmonth) jday year)
   (format t "~%3. ~A ~D, ~D: ~A"
     (n->month amonth) aday year
     (if (< year 1583)
         "Jul." "Gre."))
   (format t "~%4. ~A ~D, ~D; Jul."
     (n->month oamonth) oaday year)
   (format t "~%5. ~A ~D, ~D; ~A."
     (n->month pmonth) pday year
     (if (< year 1583)
         "Jul." "Gre."))
   (format t "~%6. ~A ~D, ~D; Jul."
     (n->month opmonth) opday year)
   (when (> year 1315)
     (format t "~%7. ~A ~D, ~D; ~A"
       (n->month tmonth) tday year
         (if (< year 1583)
             "Jul." "Gre."))
     (format t "~%8. ~A ~D, ~D; Jul."
       (n->month otmonth) otday year))
   (when (> year 1245)
     (format t "~%9. ~A ~D, ~D; ~A"
       (n->month ccmonth) ccday year
       (if (< year 1583)
           "Jul." "Gre.")))
   (format t "~%10. ~A ~D, ~D; Jul."
     (n->month asmonth) asday year)))

#|

"1. Easter 2. Orthodox Easter 3. Ascension 
4. Orthodox Ascension 5. Pentecost 6. Orthodox Pentecost 
7. Trinity Sunday 8. Orthodox Trinity 
9. Corpus Christi (Western only) 
10. All Saints Sunday (Orthodox Only)

1. APR 4, 500; Jul.
2. APR 4, 500; Jul.
3. MAY 13, 500: Jul.
4. MAY 13, 500; Jul.
5. MAY 23, 500; Jul..
6. MAY 23, 500; Jul.
10. JUN 16, 500; Jul.

1. APR 6, 600; Jul.
2. APR 6, 600; Jul.
3. MAY 15, 600: Jul.
4. MAY 15, 600; Jul.
5. MAY 25, 600; Jul..
6. MAY 25, 600; Jul.
10. JUN 18, 600; Jul.

1. APR 15, 700; Jul.
2. APR 15, 700; Jul.
3. MAY 24, 700: Jul.
4. MAY 24, 700; Jul.
5. JUN 3, 700; Jul..
6. JUN 3, 700; Jul.
10. JUN 27, 700; Jul.

1. APR 17, 800; Jul.
2. APR 17, 800; Jul.
3. MAY 26, 800: Jul.
4. MAY 26, 800; Jul.
5. JUN 5, 800; Jul..
6. JUN 5, 800; Jul.
10. JUN 29, 800; Jul.

1. APR 19, 900; Jul.
2. APR 19, 900; Jul.
3. MAY 28, 900: Jul.
4. MAY 28, 900; Jul.
5. JUN 7, 900; Jul..
6. JUN 7, 900; Jul.
10. JUL 1, 900; Jul.

1. MAR 31, 1000; Jul.
2. MAR 31, 1000; Jul.
3. MAY 9, 1000: Jul.
4. MAY 9, 1000; Jul.
5. MAY 19, 1000; Jul..
6. MAY 19, 1000; Jul.
10. JUN 12, 1000; Jul.

1. APR 2, 1100; Jul.
2. APR 2, 1100; Jul.
3. MAY 11, 1100: Jul.
4. MAY 11, 1100; Jul.
5. MAY 21, 1100; Jul..
6. MAY 21, 1100; Jul.
10. JUN 14, 1100; Jul.

1. APR 4, 1200; Jul.
2. APR 4, 1200; Jul.
3. MAY 13, 1200: Jul.
4. MAY 13, 1200; Jul.
5. MAY 23, 1200; Jul..
6. MAY 23, 1200; Jul.
10. JUN 16, 1200; Jul.

1. APR 13, 1300; Jul.
2. APR 13, 1300; Jul.
3. MAY 22, 1300: Jul.
4. MAY 22, 1300; Jul.
5. JUN 1, 1300; Jul..
6. JUN 1, 1300; Jul.
9. JUN 12, 1300; Jul.
10. JUN 25, 1300; Jul.

1. APR 15, 1400; Jul.
2. APR 15, 1400; Jul.
3. MAY 24, 1400: Jul.
4. MAY 24, 1400; Jul.
5. JUN 3, 1400; Jul..
6. JUN 3, 1400; Jul.
7. JUN 10, 1400; Jul.
8. JUN 10, 1400; Jul.
9. JUN 14, 1400; Jul.
10. JUN 27, 1400; Jul.

1. APR 24, 1500; Jul.
2. APR 24, 1500; Jul.
3. JUN 2, 1500: Jul.
4. JUN 2, 1500; Jul.
5. JUN 12, 1500; Jul..
6. JUN 12, 1500; Jul.
7. JUN 19, 1500; Jul.
8. JUN 19, 1500; Jul.
9. JUN 23, 1500; Jul.
10. JUL 6, 1500; Jul.

1. APR 2, 1600; Gre.
2. MAR 29, 1600; Jul.
3. MAY 11, 1600: Gre.
4. MAY 7, 1600; Jul.
5. MAY 21, 1600; Gre..
6. MAY 17, 1600; Jul.
7. MAY 28, 1600; Gre.
8. MAY 24, 1600; Jul.
9. JUN 1, 1600; Gre.
10. JUN 10, 1600; Jul.

1. APR 11, 1700; Gre.
2. MAR 31, 1700; Jul.
3. MAY 20, 1700: Gre.
4. MAY 9, 1700; Jul.
5. MAY 30, 1700; Gre..
6. MAY 19, 1700; Jul.
7. JUN 6, 1700; Gre.
8. MAY 26, 1700; Jul.
9. JUN 10, 1700; Gre.
10. JUN 12, 1700; Jul.

1. APR 13, 1800; Gre.
2. APR 2, 1800; Jul.
3. MAY 22, 1800: Gre.
4. MAY 11, 1800; Jul.
5. JUN 1, 1800; Gre..
6. MAY 21, 1800; Jul.
7. JUN 8, 1800; Gre.
8. MAY 28, 1800; Jul.
9. JUN 12, 1800; Gre.
10. JUN 14, 1800; Jul.

1. APR 15, 1900; Gre.
2. APR 11, 1900; Jul.
3. MAY 24, 1900: Gre.
4. MAY 20, 1900; Jul.
5. JUN 3, 1900; Gre..
6. MAY 30, 1900; Jul.
7. JUN 10, 1900; Gre.
8. JUN 6, 1900; Jul.
9. JUN 14, 1900; Gre.
10. JUN 23, 1900; Jul.

1. APR 23, 2000; Gre.
2. APR 13, 2000; Jul.
3. JUN 1, 2000: Gre.
4. MAY 22, 2000; Jul.
5. JUN 11, 2000; Gre..
6. JUN 1, 2000; Jul.
7. JUN 18, 2000; Gre.
8. JUN 8, 2000; Jul.
9. JUN 22, 2000; Gre.
10. JUN 25, 2000; Jul.

1. MAR 28, 2100; Gre.
2. APR 22, 2100; Jul.
3. MAY 6, 2100: Gre.
4. MAY 31, 2100; Jul.
5. MAY 16, 2100; Gre..
6. JUN 10, 2100; Jul.
7. MAY 23, 2100; Gre.
8. JUN 17, 2100; Jul.
9. MAY 27, 2100; Gre.
10. JUL 4, 2100; Jul.

1. APR 4, 2010; Gre.
2. MAR 23, 2010; Jul.
3. MAY 13, 2010: Gre.
4. MAY 1, 2010; Jul.
5. MAY 23, 2010; Gre..
6. MAY 11, 2010; Jul.
7. MAY 30, 2010; Gre.
8. MAY 18, 2010; Jul.
9. JUN 3, 2010; Gre.
10. JUN 4, 2010; Jul.

1. APR 24, 2011; Gre.
2. APR 16, 2011; Jul.
3. JUN 2, 2011: Gre.
4. MAY 25, 2011; Jul.
5. JUN 12, 2011; Gre..
6. JUN 4, 2011; Jul.
7. JUN 19, 2011; Gre.
8. JUN 11, 2011; Jul.
9. JUN 23, 2011; Gre.
10. JUN 28, 2011; Jul.

1. APR 8, 2012; Gre.
2. APR 4, 2012; Jul.
3. MAY 17, 2012: Gre.
4. MAY 13, 2012; Jul.
5. MAY 27, 2012; Gre..
6. MAY 23, 2012; Jul.
7. JUN 3, 2012; Gre.
8. MAY 30, 2012; Jul.
9. JUN 7, 2012; Gre.
10. JUN 16, 2012; Jul.

1. MAR 31, 2013; Gre.
2. APR 21, 2013; Jul.
3. MAY 9, 2013: Gre.
4. MAY 30, 2013; Jul.
5. MAY 19, 2013; Gre..
6. JUN 9, 2013; Jul.
7. MAY 26, 2013; Gre.
8. JUN 16, 2013; Jul.
9. MAY 30, 2013; Gre.
10. JUL 3, 2013; Jul.

1. APR 20, 2014; Gre.
2. APR 10, 2014; Jul.
3. MAY 29, 2014: Gre.
4. MAY 19, 2014; Jul.
5. JUN 8, 2014; Gre..
6. MAY 29, 2014; Jul.
7. JUN 15, 2014; Gre.
8. JUN 5, 2014; Jul.
9. JUN 19, 2014; Gre.
10. JUN 22, 2014; Jul.

1. APR 5, 2015; Gre.
2. MAR 30, 2015; Jul.
3. MAY 14, 2015: Gre.
4. MAY 8, 2015; Jul.
5. MAY 24, 2015; Gre..
6. MAY 18, 2015; Jul.
7. MAY 31, 2015; Gre.
8. MAY 25, 2015; Jul.
9. JUN 4, 2015; Gre.
10. JUN 11, 2015; Jul.

1. MAR 27, 2016; Gre.
2. APR 15, 2016; Jul.
3. MAY 5, 2016: Gre.
4. MAY 24, 2016; Jul.
5. MAY 15, 2016; Gre..
6. JUN 3, 2016; Jul.
7. MAY 22, 2016; Gre.
8. JUN 10, 2016; Jul.
9. MAY 26, 2016; Gre.
10. JUN 27, 2016; Jul.

1. APR 16, 2017; Gre.
2. APR 4, 2017; Jul.
3. MAY 25, 2017: Gre.
4. MAY 13, 2017; Jul.
5. JUN 4, 2017; Gre..
6. MAY 23, 2017; Jul.
7. JUN 11, 2017; Gre.
8. MAY 30, 2017; Jul.
9. JUN 15, 2017; Gre.
10. JUN 16, 2017; Jul.

1. APR 1, 2018; Gre.
2. MAR 24, 2018; Jul.
3. MAY 10, 2018: Gre.
4. MAY 2, 2018; Jul.
5. MAY 20, 2018; Gre..
6. MAY 12, 2018; Jul.
7. MAY 27, 2018; Gre.
8. MAY 19, 2018; Jul.
9. MAY 31, 2018; Gre.
10. JUN 5, 2018; Jul.

1. APR 21, 2019; Gre.
2. APR 17, 2019; Jul.
3. MAY 30, 2019: Gre.
4. MAY 26, 2019; Jul.
5. JUN 9, 2019; Gre..
6. JUN 5, 2019; Jul.
7. JUN 16, 2019; Gre.
8. JUN 12, 2019; Jul.
9. JUN 20, 2019; Gre.
10. JUN 29, 2019; Jul.

1. APR 12, 2020; Gre.
2. APR 5, 2020; Jul.
3. MAY 21, 2020: Gre.
4. MAY 14, 2020; Jul.
5. MAY 31, 2020; Gre..
6. MAY 24, 2020; Jul.
7. JUN 7, 2020; Gre.
8. MAY 31, 2020; Jul.
9. JUN 11, 2020; Gre.
10. JUN 17, 2020; Jul."   
    
|#

;;; Averages/Pythagorean Means

#|

Compute all three of the Pythagorean means 
of the set of integers 1 through 10 
(inclusive). 
               
Show that A(x1,...,x[n]) >=
          G(x1,...,x[n]) >=
          H(x1,...,x[n]) for this set of 
positive integers.
         
The most common of the three means, the 
arithmetic mean, is the sum of the list 
divided by its length:     
        
A(x1,...,x[n]) = (x1 + ... + x[n])/n   
                                      
The geometric mean is the nth root of the 
product of the list:      
        
G(x1,...,x[n]) = (x1 * ... * x[n])^(1/n)
                                   
The harmonic mean is n divided by the sum of 
the reciprocal of each item in the list:   
    
H(x1,...,x[n]) = n/(1/x1 + ... + 1/x[n]) 
                                        
|#          
           
(defun amean (num-list)
  (if (null num-list)
      0.0
      (float (/ (apply #'+ num-list)
                (length num-list))
             0d0)))    
                                
(defun gmean (num-list)
  (if (null num-list)
      1
      (float (expt
               (apply #'* num-list)
               (/ (length num-list)))
             0d0)))

(defun hmean (num-list)
  (assert 
    (and num-list
        (notany #'zerop num-list))
    ()
    "Cannot take the harmonic mean of an ~%~
    empty list or one that contains 0.")
  (let ((denom
         (reduce #'+
           (mapcar #'/ num-list))))
    (if (zerop denom)
        (error
         "Sum of reciprocals of num-list ~%~
         is 0. hmean.")
        (float (/ (length num-list)
                  denom)
               0d0))))
                     
(defun average/pythag-main ()
  (let ((nums (iota 10 :start 1)))
    (format t "~%nums: ~A"
            nums)
    (format t "~%Arithmetic Mean: ~F"
            (amean nums))
    (format t "~%Geometric Mean: ~F"
            (gmean nums))
    (format t "~%Harmonic Mean: ~F"
            (hmean nums))))

#|

"
nums: (1 2 3 4 5 6 7 8 9 10)
Arithmetic Mean: 5.5
Geometric Mean: 4.52872896194458
Harmonic Mean: 3.414171521474055" 
         
|#       
        
;;; Tree Datastructures

#|  
   
The following shows a tree of data with 
nesting denoted by visual levels of 
indentation:

RosettaCode
    rocks
        code
        comparison
        wiki
    mocks
        trolling    
        
A common datastructure for trees is to 
define node structures having a name and a, 
(possibly empty), list of child nodes. The 
nesting of nodes captures the indentation 
of the tree. Lets call this the nest form. 
   
# E.g. if child nodes are surrounded by brackets
#      and separated by commas then:
RosettaCode(rocks(code, ...), ...)
# But only an _example_   
  
Another datastructure for trees is to 
construct from the root an ordered list of 
the nodes level of indentation and the name 
of that node. The indentation for the root 
node is zero; node 'rocks is indented by one 
level from the left, and so on. Lets call 
this the indent form.

0 RosettaCode
1 rocks
2 code
...

Task

    Create/use a nest datastructure format 
    and textual representation for arbitrary 
    trees.
    
    Create/use an indent datastructure format 
    and textual representation for arbitrary 
    trees.
    
    Create methods/classes/proceedures/
        routines etc to:
        
        Change from a nest tree 
        datastructure to an indent one.
        
        Change from an indent tree 
        datastructure to a nest one
    
    Use the above to encode the example at 
    the start into the nest format, and show 
    it.
    
    transform the initial nest format to 
    indent format and show it.
    
    transform the indent format to final nest 
    format and show it.
     
    Compare initial and final nest formats 
    which should be the same.

Note

    It's all about showing aspects of the 
    contrasting datastructures as they hold 
    the tree.
    Comparing nested datastructures is 
    secondary - saving formatted output as 
    a string then a string compare would 
    suffice for this task, if its easier.


Show all output on this page.    |#   
     
;; In CL, trees are traditionally implemented
;; as nested (singly-linked) lists.
;; There is no need to create a new class,
;; since this implementation is very
;; simple. Moreover, it seems to me that
;; it makes little sense to change the 
;; data structure simply to display different
;; aspects of the data. Instead, use the 
;; data structure we have and convert between
;; different data representations using
;; functions. "Better to have one data
;; structure with 100 functions than to 
;; have 10 data structures, each having
;; 10 dedicated functions."

;; A tree is a list of lists. Each node
;; is a list containing a value followed
;; by an arbitrary number of children.
;; This recursive definition allows us
;; to contain the entire tree in the 
;; root node. Climbing the tree will most
;; easily be performed by recursive techniques.

;; tree. Each node begins with its value, which
;; is followed by children. The example given
;; has variable numbers of children, so 
;; we will use that

;; The example given would look like this:

;; '("RosettaCode" ("rocks" 
;;                    ("code") 
;;                    ("comparison") 
;;                    ("wiki")) 
;;                 ("mocks"
;;                    ("trolling")))
                                   
;; Thus, each node is itself a tree

(defun tval (tree)
  "Value of a tree/node"
  (first tree)) ; or (car tree)

(defun tchildren (tree)
  "Children of a tree/node, as a 
  list of nodes, or nil if there are no
  children"
  (rest tree)) ; or (cdr tree)   
                                
(defun nth-tchild (n tree)
  "Retrieve the nth child, 0 indexed,
  left to right, of the node/tree, or nil
  if there is no such child."   
  (nth n (tchildren tree)))   
                             
(defun print-nest (tree &optional (indent 0))
  (format t "~%~VT~A" indent (tval tree))
  (dolist (child (tchildren tree))
    (print-nest child (+ indent 5))))

(defun print-indent (tree &optional (level 0))
  (format t "~%~D. ~A" level (tval tree))
  (dolist (child (tchildren tree))
    (print-indent child (1+ level))))

;; There is no need to check for equality,
;; because the underlying data structure 
;; is not altered. If we need to 
;; test for equality, there is the
;; built-in TREE-EQUAL.

(defun tree-main ()
  (let ((my-tree
         '("RosettaCode"
           ("rocks"
            ("code")
            ("comparison")
            ("wiki"))
           ("mocks"
            ("trolling")))))
    (format t "~%Underlying Structure:~%")
    (format t "~%~A" my-tree)
    (format t "~2%Nesting display:~%")
    (print-nest my-tree)
    (format t "~2%Indented display:~%")
    (print-indent my-tree)))

#|
"
Underlying Structure:

(RosettaCode (rocks (code) (comparison) (wiki)) (mocks (trolling)))

Nesting display:

 RosettaCode
     rocks
          code
          comparison
          wiki
     mocks
          trolling

Indented display:

0. RosettaCode
1. rocks
2. code
2. comparison
2. wiki
1. mocks
2. trolling"   |#
                                                   
                                                                                                                                 