;;;; rc-004.lisp

(in-package :ros-01)

;;; Numerical and Alphabetical Suffixes

#|

This task is about expressing numbers with 
an attached (abutted) suffix multiplier(s),  
the suffix(es) could be: 
    
    an alphabetic (named) multiplier which 
       could be abbreviated
    metric  multiplier(s) which can be 
       specified multiple times
    "binary" multiplier(s) which can be 
       specified multiple times
    explanation marks (!) which indicate a 
       factorial or multifactorial


The (decimal) numbers can be expressed 
generally as: 
          
{±} {digits} {.} {digits}
────── or ──────
{±} {digits} {.} {digits} {E or e} {±}   
    {digits}   
    
where:

      numbers won't have embedded blanks   
             (contrary to the expaciated 
              examples above where 
              whitespace was used for 
              readability)
      this task will only be dealing with 
             decimal numbers, both in the 
             mantissa and exponent
      ± indicates an optional plus or minus 
             sign (+ or -)
      digits are the decimal digits   
             (0 ──► 9)  
             
            the digits can have comma(s) interjected to separate the   periods   (thousands)   such as:   12,467,000
              .   is the decimal point, sometimes also called a   dot
              e   or   E   denotes the use of decimal exponentiation   (a number multiplied by raising ten to some power)


This isn't a pure or perfect definition of the way we express decimal numbers,   but it should convey the intent for this task.

The use of the word   periods   (thousands) is not meant to confuse, that word (as used above) is what the comma separates;
the groups of decimal digits are called periods,   and in almost all cases, are groups of three decimal digits.

If an   e   or   E   is specified, there must be a legal number expressed before it,   and there must be a legal (exponent) expressed after it.

Also, there must be some digits expressed in all cases,   not just a sign and/or decimal point.

Superfluous signs, decimal points, exponent numbers, and zeros   need not be preserved.

I.E.:       +7   007   7.00   7E-0   7E000   70e-1     could all be expressed as 7

All numbers to be "expanded" can be assumed 
to be valid and there won't be a requirement 
to verify their validity.

Abbreviated alphabetic suffixes to be 
supported (where the capital letters signify 
the minimum abbreation that can be used)  
    
PAIRs        n x 2   
SCOres       n x 20
DOZens       n x 12       
GRoss        n x 144     
GREATGRoss   n x 1728   
GOOGOLs      n x 10^100    
             
Note that the plurals are supported, even 
though they're usually used when expressing 
exact numbers (She has 2 dozen eggs, and 
dozens of quavas)
       
Metric suffixes to be supported (whether or 
not they're officially sanctioned)
    
K    n x 10^3
M    n x 10^6
G    n x 10^9
T    n x 10^12
P    n x 10^15
E    n x 10^18
Z    n x 10^21
Y    n x 10^24
X    n x 10^27
W    n x 10^30
V    n x 10^33  
U    n x 10^36
     
Binary suffixes to be supported (whether or 
not they're officially sanctioned)
    
Ki   n x 2^10
Mi   n x 2^20
Gi   n x 2^30
Ti   n x 2^40
Pi   n x 2^50
Ei   n x 2^60
Zi   n x 2^70
Yi   n x 2^80
Xi   n x 2^90
Wi   n x 2^100
Vi   n x 2^110
Ui   n x 2^120
     
All of the metric and binary suffixes can be 
expressed in lowercase, uppercase, or mixed 
case.
All of the metric and binary suffixes can be 
stacked (expressed multiple times), 
and also be intermixed:
    
I.E.: 123k 123K 123GKi 12.3GiGG 12.3e-7T   
      .78E100e  
      
Factorial suffixes to be supported

! compute the (regular) factorial product: 
     5! is  5 × 4 × 3 × 2 × 1  =  120
!! compute the  double factorial product:   
     8!! is  8 × 6 × 4 × 2  =  384
!!! compute the triple factorial product:   
     8!!! is  8 × 5 × 2  =   80
!!!! compute the quadruple factorial product:
     8!!!! is  8 × 4  =  32
!!!!! compute the quintuple factorial 
      product:   
     8!!!!! is  8 × 3  = 24
     
··· the number of factorial symbols that can 
    be specified is to be unlimited   
    (as per what can be entered/typed) ···
    
Factorial suffixes aren't, of course, the usual type of multipliers, but are used here in a similar vein.


Multifactorials aren't to be confused with   super─factorials     where   (4!)!   would be   (24)!.


Task

Using the test cases (below),   
      show the "expanded" numbers here, on 
           this page.
      For each list, show the input on one 
           line, and also show the output on 
           one line.
      When showing the input line, keep the 
           spaces (whitespace) and case 
           (capitalizations) as is.
      For each result (list) displayed on one 
           line, separate each number with 
           two blanks.
      Add commas to the output numbers were 
           appropriate.
           
Test cases

    2greatGRo   24Gros  288Doz  1,728pairs  172.8SCOre
    1,567      +1.567k    0.1567e-2m
    25.123kK    25.123m   2.5123e-00002G
    25.123kiKI  25.123Mi  2.5123e-00002Gi  +.25123E-7Ei
    -.25123e-34Vikki      2e-77gooGols
    9!   9!!   9!!!   9!!!!   9!!!!!   9!!!!!!   9!!!!!!!   9!!!!!!!!   9!!!!!!!!!
    
where the last number for the factorials has 
nine factorial symbols (!) after the 9.
     
|#

;;; I think the best way to go about this
;;; is to use regular expressions. These
;;; are not native to CL, but there is a 
;;; defacto standard 3rd-party library
;;; cl-ppcre, which I will import and use.
;;; (see rosetta.asd and packagelisp)
                                     
(defparameter *multiplicands*
  (dict
   :pair 2
   :sco 20
   :doz 12
   :gr 144
   :greatgr 1728
   :googol #.(expt 10 100)
   :k 1000
   :m 1000000
   :g 1000000000
   :t #.(expt 10 12)
   :p #.(expt 10 15)
   :e #.(expt 10 18)
   :z #.(expt 10 21)
   :y #.(expt 10 24)
   :x #.(expt 10 27)
   :w #.(expt 10 30)
   :v #.(expt 10 33)
   :u #.(expt 10 36)
   :ki #.(expt 2 10)
   :mi #.(expt 2 20)
   :gi #.(expt 2 30)
   :ti #.(expt 2 40)
   :pi #.(expt 2 50)
   :ei #.(expt 2 60)
   :zi #.(expt 2 70)
   :yi #.(expt 2 80)
   :xi #.(expt 2 90)
   :wi #.(expt 2 100)
   :vi #.(expt 2 110)
   :ui #.(expt 2 120)))

(defparameter *keys*
  '(("^(?i)pair(s)?" . :pair)
    ("^(?i)sco(res|re|r)?" . :sco)
    ("^(?i)doz(ens|en|e)?" . :doz)
    ("^(?i)greatgr(oss|os|o)?" . :greatgr)
    ("^(?i)gr(oss|os|o)?" . :gr)
    ("^(?i)googol(s)?" . :googol)
    ("^(?i)ki" . :ki)
    ("^(?i)mi" . :mi)
    ("^(?i)gi" . :gi)
    ("^(?i)ti" . :ti)
    ("^(?i)pi" . :pi)
    ("^(?i)ei" . :ei)
    ("^(?i)zi" . :zi)
    ("^(?i)yi" . :yi)
    ("^(?i)xi" . :xi)
    ("^(?i)wi" . :wi)
    ("^(?i)vi" . :vi)
    ("^(?i)ui" . :ui)
    ("^[kK]" . :k)
    ("^[mM]" . :m)
    ("^[gG]" . :g)
    ("^[tT]" . :t)
    ("^[pP]" . :p)
    ("^[eE]" . :e)
    ("^[zZ]" . :z)
    ("^[yY]" . :y)
    ("^[xX]" . :x)
    ("^[wW]" . :w)
    ("^[vV]" . :v)
    ("^[uU]" . :u)))

(defun multi-fac (!count)
  (lambda (n)
    (apply #'*
      (iter
        (for i from n downto 1 by !count)
        (collect i)))))
       
(defun multi-branch (key)
  (lambda (n)
    (* n (gethash key *multiplicands*))))     
                                     
(defun num-and-suffixes-strings (str)
  (let* ((no-commas (remove #\, str))
         (num
          (scan-to-strings
           "[+-]?\\d*\\.?\\d*([eE][-+]?\\d+)?"
           no-commas)))
    (let ((mismatch-index
            (mismatch num no-commas)))
      (if mismatch-index
          (values
           num
           (subseq no-commas mismatch-index))
          (values
           num
           "")))))

(defun num-string->num (num-string)
  (cond 
    ((zerop (length num-string))
     0)
    ((every #'digit-char-p num-string)
     (parse-integer num-string))
    ((and (= (length num-string) 1)
          (digit-char-p (char num-string 0)))
     (parse-integer num-string))
    ((and (= (length num-string) 2)
          (member (char num-string 0) '(#\- #\+))
          (digit-char-p (char num-string 1)))
     (parse-integer num-string))
    ((and (= (length num-string) 2)
          (digit-char-p (char num-string 0))
          (char= (char num-string 1) #\.))
     (parse-integer num-string :junk-allowed t))
    ((and (> (length num-string) 2)
          (or (digit-char-p (char num-string 0))
              (member 
                (char num-string 0) 
                '(#\+ #\-)))
          (every #'digit-char-p
            (subseq num-string 1 
             (- (length num-string) 1)))
          (or (digit-char-p 
                (char num-string 
                  (1- (length num-string))))
              (char= 
                (char num-string 
                  (1- (length num-string)))
                     #\.)))
     (parse-integer num-string :junk-allowed t))
    (t 
     (let ((*read-default-float-format* 
            'double-float))
       (parse-float num-string)))))

(defun suffixes->fn-list (suffixes)
  (if (zerop (length suffixes))
      nil
      (multiple-value-bind
          (start end _ _)
          (scan "^!+" suffixes)
        (if start
            (append
              (suffixes->fn-list
               (subseq suffixes end))
              (list (multi-fac end)))
            (iter
              (for (regex . key) in *keys*)
              (multiple-value-bind
                  (start end _ _)
                  (scan regex suffixes)
                (when start
                  (return
                   (append
                     (suffixes->fn-list
                      (subseq suffixes end))
                     (list (multi-branch key))))))
              (finally
               (error
                "No matching regex for ~S ~%~
                suffixes->fn-list"
                suffixes)))))))
                
(defun calculate (num fn-list)
  (if (null fn-list)
      (if (integerp num)
          num
          (float num 0d0))
      (funcall (car fn-list)
        (calculate num (cdr fn-list)))))

(defun parse-and-calc (str)
  (multiple-value-bind
      (num-str suffixes)
      (num-and-suffixes-strings str)
    (calculate
     (num-string->num num-str)
     (suffixes->fn-list suffixes))))

(defparameter *na-data*
  "2greatGRo   24Gros  288Doz  1,728pairs  172.8SCOre
    1,567      +1.567k    0.1567e-2m
    25.123kK    25.123m   2.5123e-00002G
    25.123kiKI  25.123Mi  2.5123e-00002Gi  +.25123E-7Ei
    -.25123e-34Vikki      2e-77gooGols
    9!   9!!   9!!!   9!!!!   9!!!!!   9!!!!!!   9!!!!!!!   9!!!!!!!!   9!!!!!!!!!    ")
     
;; Lisp has a mechanism to format integers
;; with arbitrary digit groups and 
;; arbitrary separators. But no such
;; native facility exists for floats.

(defun float-commas (real)
  (cond
   ((integerp real)
    (format nil "~:D" real))
   ((< -1000 real 1000)
    (format t "~G" real))
   (t
    (multiple-value-bind
        (int frac)
        (truncate real 1000)
      (cond
       ((>= (abs frac) 100)
        (format nil "~:D,~F" int (abs frac)))
       ((>= (abs frac) 10)
        (format nil "~:D,0~F" int (abs frac)))
       (t
        (format nil "~:D,00~F" int (abs frac))))))))

(defun numeric-and-alphabetical-main ()
  (let ((lines (uiop:split-string
                 *na-data* 
                 :separator 
                 '(#\Newline))))
    (dolist (line lines)
      (let ((expressions (tokens line)))
        (format t "~%~A" line)
        (format t "~%~{~A~^  ~}"
                (mapcar 
                  (lambda (ex)
                    (float-commas
                     (parse-and-calc ex)))
                  expressions))))))

#|

"
2greatGRo   24Gros  288Doz  1,728pairs  172.8SCOre
3,456  3,456  3,456  3,456  3,456.0
    1,567      +1.567k    0.1567e-2m
1,567  1,567.0  1,567.0
    25.123kK    25.123m   2.5123e-00002G
25,123,000.0  25,123,000.0  25,123,000.0
    25.123kiKI  25.123Mi  2.5123e-00002Gi  +.25123E-7Ei
26,343,374.8480000011623  26,343,374.8480000011623  26,975,615.8443519994617  28,964,846,960.2378158569336
    -.25123e-34Vikki      2e-77gooGols
-33,394.19493810443964  199,999,999,999,999,967,232,000.0
    9!   9!!   9!!!   9!!!!   9!!!!!   9!!!!!!   9!!!!!!!   9!!!!!!!!   9!!!!!!!!!    
362,880  945  162  45  36  27  18  9  9" 
         
Not bad. But we are dealing with floats, 
so things are inaccurate.
   
Also. Commas? With floats? Really?                   
 
|#

;;; Amb

#|

Define and give an example of the Amb 
operator.

The Amb operator (short for "ambiguous") 
expresses nondeterminism. This doesn't refer 
to randomness (as in "nondeterministic 
universe") but is closely related to the 
term as it is used in automata theory 
("non-deterministic finite automaton").

The Amb operator takes a variable number of 
expressions (or values if that's simpler in 
the language) and yields a correct one which 
will satisfy a constraint in some future 
computation, thereby avoiding failure.

Problems whose solution the Amb operator 
naturally expresses can be approached with 
other tools, such as explicit nested 
iterations over data sets, or with pattern 
matching. By contrast, the Amb operator 
appears integrated into the language. 
Invocations of Amb are not wrapped in any 
visible loops or other search patterns; they 
appear to be independent.

Essentially Amb(x, y, z) splits the 
computation into three possible futures: a 
future in which the value x is yielded, a 
future in which the value y is yielded and a 
future in which the value z is yielded. The 
future which leads to a successful 
subsequent computation is chosen. The other 
"parallel universes" somehow go away. Amb 
called with no arguments fails.

For simplicity, one of the domain values 
usable with Amb may denote failure, if that 
is convenient. For instance, it is 
convenient if a Boolean false denotes 
failure, so that Amb(false) fails, and thus 
constraints can be expressed using Boolean 
expressions like Amb(x * y == 8) which 
unless x and y add to four.

A pseudo-code program which satisfies this 
constraint might look like:

let x = Amb(1, 2, 3)
let y = Amb(7, 6, 4, 5)
Amb(x * y = 8)
print x, y

The output is 2 4 because Amb(1, 2, 3) 
correctly chooses the future in which x has 
value 2, Amb(7, 6, 4, 5) chooses 4 and 
consequently Amb(x * y = 8) produces a 
success.

Alternatively, failure could be represented using strictly Amb():

unless x * y = 8 do Amb()

Or else Amb could take the form of two operators or functions: one for producing values and one for enforcing constraints:

let x = Ambsel(1, 2, 3)
let y = Ambsel(4, 5, 6)
Ambassert(x * y = 8)
print x, y

where Ambassert behaves like Amb() if the 
Boolean expression is false, otherwise it 
allows the future computation to take place, 
without yielding any value.

The task is to somehow implement Amb, and 
demonstrate it with a program which chooses 
one word from each of the following four 
sets of character strings to generate a 
four-word sentence:

    "the" "that" "a"
    "frog" "elephant" "thing"
    "walked" "treaded" "grows"
    "slowly" "quickly"

The constraint to be satisfied is that the 
last character of each word (other than the 
last) is the same as the first character of 
its successor.

The only successful sentence is "that thing 
grows slowly"; other combinations do not 
satisfy the constraint and thus fail.

The goal of this task isn't to simply 
process the four lists of words with 
explicit, deterministic program flow such 
as nested iteration, to trivially 
demonstrate the correct output. The goal is 
to implement the Amb operator, or a 
facsimile thereof that is possible within 
the language limitations. |#
    
;; This implementation is taken from 
;; Paul Graham's On Lisp. It uses macros
;; to simulate Scheme continuations, which
;; are employed to make the amb operator.

;; First, we need a global LEXICALLY scoped
;; parameter that will reference a 
;; continuation. 

(setq *cont* #'identity)

;; Next, the basic machinery for amb

(defparameter *paths* nil
  "Used to store paths of future computation
  that have not yet been tried.")

(defconstant +failsym+ '@
  "Symbol to indicate failure. We decided
  to use a dedicated symbol for amb failure,
  since NIL already has 3 meanings: the
  empty list, Boolean false and search 
  failure.")

(defun fail ()
  "When a choice fails, we try the next choice.
  If there is no next choice, we return 
  +failsym+"
  (if *paths*
      (funcall (pop *paths*))
      +failsym+))

(defmacro amb (&rest choices)
  (if choices
      `(progn
         ,@(mapcar 
             #'(lambda (c)
                 `(push 
                    #'(lambda () ,c) 
                    *paths*))
               (reverse (cdr choices)))
         ,(car choices))
       '(fail)))

(defun cb (fn choices)
  "Auxiliary function for amb-bind."
  (if choices
      (progn 
        (when (cdr choices)
            (push 
              #'(lambda () (cb fn (cdr choices)))
              *paths*))
        (funcall fn (car choices)))
      (fail)))

(defmacro amb-bind (var choices &body body)
  "Convenience macro to facilitate supplying
  amb with choices that are functions. Not
  strictly necessary"
  `(cb #'(lambda (,var) ,@body) ,choices))

;; Now we need a continuations mechanism.
;; This allows us to actually use amb.
;; A continuation stores the environment
;; of a specific code stack frame, allowing
;; us to use it at a moment's notice.
;; The lexical global parameter *cont*,
;; above, is necessary for the code that 
;; follows.

;; The majority of the following 
;; macros simulate native CL operators, 
;; but they make sure information about
;; the stack frame is always stored.
;; The CL operators have the same names
;; as these, without the initial =.

(defmacro =lambda (parms &body body)
  "Creates a local, anonymous function."
  `#'(lambda (*cont* ,@parms) ,@body))

(defmacro =defun (name parms &body body)
  "Define a new dynamic/global function.
  Unlike defun, =defun requires a 
  special operator in functions to 
  return values. This is =values, below."
  (let ((f (intern (concatenate 'string
                     "="
                     (symbol-name
                      name)))))
    `(progn
       (defmacro ,name ,parms
         `(,',f *cont* ,,@parms))
       (defun ,f (*cont* ,@parms) ,@body))))
       
(defmacro =bind (parms expr &body body)
  "The cl macro is multiple-value-bind.
  Receives multiple values from a single
  expr, binds them to parms, and executes
  body in the lexical environment of the
  bindings."
  `(let ((*cont* #'(lambda ,parms ,@body)))
     ,expr))

(defmacro =values (&rest retvals)
  "Like 'values' in that it can be used
  to return multiple values from a function.
  In addition, all functions written with
  =defun that return one or more values 
  must either end with =values or with
  a call to a function that does."
  `(funcall *cont* ,@retvals))

(defmacro =funcall (fn &rest args)
  "Call a function passed as an argument."
  `(funcall ,fn *cont* ,@args))

(defmacro =apply (fn &rest args)
  "Call a function, using the elements of 
  a list as its arguments."
  `(apply ,fn *cont* ,@args))
  
;; Finally, the required task

(=defun words ()
  (amb-bind word1 '("the" "that" "a")
    (amb-bind word2 '("frog" "elephant" "thing")
      (amb-bind word3 '("walked" "treaded" "grows")
        (amb-bind word4 '("slowly" "quickly")
          (=values word1 word2 word3 word4))))))

(=defun amb-main ()
   (=bind (w1 w2 w3 w4) (words)
      (if
       (and
        (char= (char w1 (1- (length w1)))
               (char w2 0))
        (char= (char w2 (1- (length w2)))
               (char w3 0))
        (char= (char w3 (1- (length w3)))
               (char w4 0)))
       (list w1 w2 w3 w4)
       (fail))))

;; ("that" "thing" "grows" slowly")

;;; History Variables
                    
#|

Storing the history of objects in a program 
is a common task. Maintaining the history of 
an object in a program has traditionally 
required programmers either to write 
specific code for handling the historical 
data, or to use a library which supports 
history logging.
        
History variables are variables in a 
programming language which store not only 
their current value, but also the values 
they have contained in the past. Some 
existing languages do provide support for 
history variables. However these languages 
typically have many limits and restrictions 
on use of history variables.

[http://www.bod.com/index.php?id=3435&objk_id=148050                                               
"History Variables: The Semantics, Formal 
Correctness, and Implementation of History 
Variables in an Imperative Programming 
Language" by Mallon and Takaoka]

Concept also discussed on LtU and 
Patents.com.

Task

Demonstrate History variable support:

    enable history variable support 
       (if needed)
    define a history variable
    assign three values
    non-destructively display the history
    recall the three values.


For extra points, if the language of choice 
does not support history variables, 
demonstrate how this might be implemented. 
            
|#

;; History variables are not a part of the
;; CL standard. They can be implemented
;; rather easily using macros and 
;; property lists.

;; Arbitrary combinations of letters, 
;; numbers and other glyphs, which would
;; be recognized as variables in most 
;; languages are called symbols in the list
;; family. They are most frequently used 
;; as variables, but can also be used as
;; objects in their own right, and take 
;; the place of strings in some applications.

;; Every symbol contains a property list, 
;; a very simple associative list, which
;; contains certain default properties, and
;; to which arbitrary additions can be made.
;; We can store a list of prior values 
;; keyed to some keyword, like :history.

;; A macro must be used to manipulate a 
;; symbol for binding, so we need that
;; facility.

(defmacro add-history (var)
  "Aux macro for set-hist"
  `(push
    ,var
    (get ',var :history nil)))

(defmacro set-history-var (var expr)
  "Setter for a history variable"
  (let ((gexpr (gensym "gexpr")))
    `(let ((,gexpr ,expr))
       (setf ,var ,gexpr)
       (add-history ,var)
       ,gexpr)))
       
(defun get-history (quoted-var)
  "Returns the history of a history variable."
  (if 
    (not 
      (member 
        :history 
        (symbol-plist quoted-var)))
    (format t "~A is not a history variable."
            quoted-var)
    (get quoted-var :history)))

(defmacro recall (var)
   (let ((value (gensym "value")))
    `(cond 
       ((not (member 
               :history 
               (symbol-plist ',var)))
        (error
          "~A not a history variable."
          ',var))
       ((null (get ',var :history))
        (setf ,var nil))
       (t
        (pop (get ',var :history))
        (setf ,var (car (get ',var :history)))))))
                                                              
#|
 
(set-history-var h 25)
-> 25
h
-> 25
(set-history-var h 50)
-> 50
h
-> 50
(set-history-var h 75)
-> 75
h
-> 75
(get-history 'h)
-> (75 50 25)
h
-> 75
(recall h)
-> 50
h
-> 50
(recall h)
-> 25
h 
-> 25
(recall h)
-> NIL
h
-> NIL
(recall h)
-> NIL
h
-> NIL
   
(get-history 'sam)
printout-> SAM not a history variable
-> NIL
   
(revert sam)

error-> SAM not a history variable
        
|#

;;; Parsing/Shunting-yard Algorithm

#|

Given the operator characteristics and input 
from the Shunting-yard algorithm page and 
tables, use the algorithm to show the 
changes in the operator stack and RPN output 
as each individual token is processed.

    Assume an input of a correct, space 
    separated, string of tokens representing 
    an infix expression
    
    Generate a space separated output string 
    representing the RPN
    
    Test with the input string:  
    
    3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3  
    
    print and display the output here.
    
    Operator precedence is given in this 
    table:         
    
    operator precedence associativity operation
    ^ 	    4 	      right 	   exponentiation
    * 	    3 	      left 	   multiplication
    / 	    3 	      left 	   division
    + 	    2 	      left 	   addition
    - 	    2 	      left 	   subtraction 


Extra credit

Add extra text explaining the actions and an 
optional comment for the action on receipt 
of each token. 
   
Note

The handling of functions and arguments is 
not required. |#
    
(defparameter *precedence*
  '(("^" . 4)
    ("*" . 3)
    ("/" . 3)
    ("+" . 2)
    ("-" . 2)))

(defun precedence> (op1 op2)
  (> (cdr (assoc op1 *precedence* :test #'equal))
     (cdr (assoc op2 *precedence* :test #'equal))))

(defun operatorp (token)
  (member token '("^" "*" "/" "+" "-")
          :test #'equal))
    
(defun infix-list->rpn-list (infix-list)
  (let ((res-stack nil)
        (op-stack nil))
    (dolist (token infix-list 
               (progn
                 (dolist (op op-stack)
                   (push op res-stack))
                 (nreverse res-stack)))               
      (cond
        ((equal token "(")
         (push token op-stack))
        ((equal token ")")
           (iter
            (for prev = (pop op-stack))
            (until (equal prev "("))
            (push prev res-stack)))
        ((operatorp token)
         (if (null op-stack)
             (push token op-stack)
             (let ((prev (car op-stack)))
               (cond
                ((equal prev "(")
                 (push token op-stack))
                ((equal token "^")
                 (push token op-stack))
                ((precedence> token prev)
                 (push token op-stack))
                (t
                 (iter
                  (for prev = (car op-stack))
                  (while op-stack)
                  (while 
                    (not
                     (equal prev "(")))
                  (while
                    (not
                     (precedence> token prev)))
                  (push
                   (pop op-stack)
                   res-stack)
                  (finally
                   (push token op-stack))))))))
        (t
         (push token res-stack))))))

(defun tokenize (str)
  (uiop:split-string str :separator " "))

(defparameter *shunting-data*
  "3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")

(defun shunting-main ()
  (let ((infix-list (tokenize *shunting-data*)))
    (format t "~%~{~A~^ ~}"
      (infix-list->rpn-list infix-list))))
                                   
;; 3 4 2 * 1 5 - 2 3 ^ ^ / +  

;;; Graph Colouring

#|

A Graph is a collection of nodes (or 
vertices), connected by edges (or not). 
Nodes directly connected by edges are called 
neighbours.  
            
In our representation of graphs, nodes are 
numbered and edges are represented by the 
two node numbers connected by the edge 
separated by a dash. Edges define the nodes 
being connected. Only unconnected nodes need 
a separate description. 
  
Example graph

+---+
| 3 |
+---+

  +-------------------+
  |                   |
+---+     +---+     +---+
| 0 | --- | 1 | --- | 2 |
+---+     +---+     +---+
            
A useful internal datastructure for a graph 
and for later graph algorithms is as a 
mapping between each node and the set/list 
of its neighbours.

In the above example:

0 maps-to 1 and 2
1 maps to 2 and 0
2 maps-to 1 and 0
3 maps-to <nothing>

Graph colouring task

Colour the vertices of a given graph so that 
no edge is between verticies of the same 
colour.

    Integers may be used to denote different 
    colours.
    
    Algorithm should do better than just 
    assigning each vertex a separate colour. 
    The idea is to minimise the number of 
    colours used, although no algorithm short 
    of exhaustive search for the minimum is 
    known at present, (and exhaustive search 
    is not a requirement).
    
    Show for each edge, the colours assigned 
    on each vertex. 
    
    Show the total number of nodes, edges, 
    and colours used for each graph.

Use the following graphs
    
Ex1    
      
       0-1 1-2 2-0 3

+---+
| 3 |
+---+

  +-------------------+
  |                   |
+---+     +---+     +---+
| 0 | --- | 1 | --- | 2 |
+---+     +---+     +---+
          
Ex2

The wp articles left-side graph

   1-6 1-7 1-8 2-5 2-7 2-8 3-5 3-6 3-8 4-5 4-6 4-7


  +----------------------------------+
  |                                  |
  |                      +---+       |
  |    +-----------------| 3 | ------+----+
  |    |                 +---+       |    |
  |    |                   |         |    |
  |    |                   |         |    |
  |    |                   |         |    |
  |  +---+     +---+     +---+     +---+  |
  |  | 8 | --- | 1 | --- | 6 | --- | 4 |  |
  |  +---+     +---+     +---+     +---+  |
  |    |         |                   |    |
  |    |         |                   |    |
  |    |         |                   |    |
  |    |       +---+     +---+     +---+  |
  +----+------ | 7 | --- | 2 | --- | 5 | -+
       |       +---+     +---+     +---+
       |                   |
       +-------------------+
       
        
Ex3

The wp articles right-side graph which is 
 the same graph as Ex2, but with different 
 node orderings and namings.

   1-4 1-6 1-8 3-2 3-6 3-8 5-2 5-4 5-8 7-2 7-4 7-6


  +----------------------------------+
  |                                  |
  |                      +---+       |
  |    +-----------------| 5 | ------+----+
  |    |                 +---+       |    |
  |    |                   |         |    |
  |    |                   |         |    |
  |    |                   |         |    |
  |  +---+     +---+     +---+     +---+  |
  |  | 8 | --- | 1 | --- | 4 | --- | 7 |  |
  |  +---+     +---+     +---+     +---+  |
  |    |         |                   |    |
  |    |         |                   |    |
  |    |         |                   |    |
  |    |       +---+     +---+     +---+  |
  +----+------ | 6 | --- | 3 | --- | 2 | -+
       |       +---+     +---+     +---+
       |                   |
       +-------------------+
       
  
Ex4

This is the same graph, node naming, and 
edge order as Ex2 except some of the edges 
x-y are flipped to y-x. This might alter the 
node order used in the greedy algorithm 
leading to differing numbers of colours.

   1-6 7-1 8-1 5-2 2-7 2-8 3-5 6-3 3-8 4-5 4-6 4-7


                      +-------------------------------------------------+
                      |                                                 |
                      |                                                 |
  +-------------------+---------+                                       |
  |                   |         |                                       |
+---+     +---+     +---+     +---+     +---+     +---+     +---+     +---+
| 4 | --- | 5 | --- | 2 | --- | 7 | --- | 1 | --- | 6 | --- | 3 | --- | 8 |
+---+     +---+     +---+     +---+     +---+     +---+     +---+     +---+
  |         |                             |         |         |         |
  +---------+-----------------------------+---------+         |         |
            |                             |                   |         |
            |                             |                   |         |
            +-----------------------------+-------------------+         |
                                          |                             |
                                          |                             |
                                          +-----------------------------+
                                                         
|#

;; I have chosen the DSatur heuristic
;; algorithm

;; The following is a description of the 
;; algorithm:

#| Let the "degree of saturation" of a 
vertex be the number of different colours 
being used by its neighbors. Given a simple, 
undirected graph G compromising a vertex set 
V and edge set E, the algorithm assigns 
colors to all of the vertices using color 
labels 1 , 2 , 3 , . . . The algorithm 
operates as follows:
         
1. Let v be the uncolored vertex in G with 
the highest degree of saturation. In cases 
of ties, choose the vertex among these with 
the largest degree in the subgraph induced 
by the uncolored vertices.
    
2. Assign v to the lowest color label not 
being used by any of its neighbors. 
      
3. If all vertices have been colored, then 
end; otherwise return to Step 1. |#

(defparameter *node-list* nil)
 
(defparameter *node-dict*
  (make-hash-table :test #'equal))

(defun reset-collections ()
   (setf *node-list* nil)
   (clrhash *node-dict*))
 
(defun color (node)
  (let ((entry
          (gethash node *node-dict*)))
    (if entry
        (gethash :color entry)
        (error "Node ~S not in *node-dict* ~%~
               COLOR" node))))

(defun set-color (node color)
  (let ((entry (gethash node *node-dict*)))
    (assert entry () 
      "Node ~S not in *node-dict*"
      node)
    (setf (gethash :color entry) color)))

(defsetf color set-color)

(defun neighbors (node)
  (let ((entry
          (gethash node *node-dict*)))
    (if entry
        (gethash :neighbors entry)
        (error "Node ~S not in *node-dict* ~%~
                NEIGHBORS" node))))
                
(defun add-edge (a b)
  (push b (gethash :neighbors
                   (gethash a *node-dict*)))
  (push a (gethash :neighbors
                   (gethash b *node-dict*)))
  'ok)
        

(defun degree-of-saturation (node)
  (let ((dos 0)
        (neighbors (neighbors node)))
    (when neighbors
          (mapc #'(lambda (neighbor)
                    (when (plusp 
                            (color neighbor))
                      (incf dos)))
                  neighbors))
    dos))

(defun next-node ()
  (let ((candidates
          (remove-if #'(lambda (node)
                         (plusp (color node)))
            *node-list*)))
    (when candidates
      (let ((best (first candidates))
            (max-sat
              (degree-of-saturation
                (first candidates))))
        (dolist (node (rest candidates) 
                 best)
          (let ((cur-sat
                  (degree-of-saturation node)))
            (cond
              ((> cur-sat max-sat)
               (setf max-sat cur-sat
                     best node))
              ((= cur-sat max-sat)
               (when (> 
                      (length 
                        (neighbors node))
                      (length
                        (neighbors best)))
                 (setf best node))))))))))

(defun assign-color (node)
  (let ((neighbors (neighbors node)))
    (setf (color node)
      (if neighbors
          (1+
            (apply #'max
                   (mapcar #'color neighbors)))
          1))))

(defun color-all ()
  (loop
    while (some 
            #'zerop 
            (mapcar #'color *node-list*))
    do
    (assign-color (next-node)))
  'ok)

(defun edge-string->list (edge-string)
  (mapcar #'(lambda (token)
              (split-string token 
                :separator "-"))
            (split-string edge-string
                :separator " ")))

(defun populate-collections (edge-string)
  (reset-collections)
  (let ((edges 
          (edge-string->list edge-string)))
    (mapc #'(lambda (edge)
              (let ((a (first edge))
                    (b (second edge)))
                (unless
                  (member a *node-list*
                    :test #'equal)
                  (push a *node-list*)
                  (setf
                    (gethash a *node-dict*)
                    (dict #'equal :color 0
                          :neighbors nil)))
                (when b
                  (unless 
                    (member b *node-list*
                       :test #'equal)
                    (push b *node-list*)
                    (setf
                      (gethash b *node-dict*)
                      (dict #'equal :color 0
                            :neighbors nil)))
                  (add-edge a b))))
            edges)
          (setf *node-list*
                (sort *node-list* #'string<))
          *node-list*))

(defun color-main ()
  (let 
    ((cases
      '("0-1 1-2 2-0 3"
        "1-6 1-7 1-8 2-5 2-7 2-8 3-5 3-6 3-8 4-5 4-6 4-7"
        "1-4 1-6 1-8 3-2 3-6 3-8 5-2 5-4 5-8 7-2 7-4 7-6"             
        "1-6 7-1 8-1 5-2 2-7 2-8 3-5 6-3 3-8 4-5 4-6 4-7"                
        )))
    (dolist (case cases)
      (populate-collections case)
      (color-all)
      (format t "~&~A:~%" case)
      (format t "~&~{~A~^ ~}~%"
        (mapcar #'(lambda (node)
                    (list node (color node)))
                  *node-list*)))))

#| My results. Each result shows the 
graph on one line, then (node color) 
pairs on the next.
                 
"0-1 1-2 2-0 3:
(0 1) (1 2) (2 3) (3 1)
1-6 1-7 1-8 2-5 2-7 2-8 3-5 3-6 3-8 4-5 4-6 4-7:
(1 1) (2 5) (3 3) (4 7) (5 6) (6 2) (7 8) (8 4)
1-4 1-6 1-8 3-2 3-6 3-8 5-2 5-4 5-8 7-2 7-4 7-6:
(1 1) (2 4) (3 5) (4 2) (5 3) (6 6) (7 7) (8 4)
1-6 7-1 8-1 5-2 2-7 2-8 3-5 6-3 3-8 4-5 4-6 4-7:
(1 1) (2 5) (3 3) (4 7) (5 6) (6 2) (7 8) (8 4)
" 
      
|#
 

#|

PRIME DECOMPOSITION

The prime decomposition of a number is 
defined as a list of prime numbers which 
when all multiplied together, are equal to 
that number. 
     
Example

 12 = 2 × 2 × 3,  so its prime 
decomposition is  {2, 2, 3}

Task

Write a function which returns an array or 
collection which contains the prime 
decomposition of a given number n   
greater than 1.

If your language does not have an 
isPrime-like function available, you may 
assume that you have a function which 
determines whether a number is prime 
(note its name before your code).

If you would like to test code from this 
task, you may use code from trial division 
or the Sieve of Eratosthenes.

Note: The program must not be limited by 
the word size of your computer or some 
other artificial limit; it should work for 
any number regardless of size 
(ignoring the physical limits of RAM etc).
|#

;; Common Lisp has no built-in isPrime
;; function, so we will need to build 
;; one.

;; Common Lisp has arbitrary precision 
;; integers, so we need not worry about 
;; handling large results. 

(defparameter *first-primes*
  '(2 3 5 7 11 13 17 19 23 29 31 37 41
    43 47 53 59 61 67 71 73 79 83 89 97
    101 103 107 109 113 127 131 137 139
    149 151 157 163 167 173 179 181 191
    193 197 199 211 223 227 229 233 239
    241 251 257 263 269 271 277 281 283
    293 307 311 313 317 331 337 347 349
    353 359 367 373 379 383 389 397 401
    409 419 421 431 433 439 443 449 447
    461 463 467 479 487 491 499 503 509
    521 523 541))

(defparameter *biggest-first-prime* 541)

(defun square (x)
  (* x x))

(defun primep (n)
  (cond
  ((or (not (integerp n))
       (< n 2))
   nil)
  ((<= n *biggest-first-prime*)
   (find n *first-primes*))
  ((<= n (square *biggest-first-prime*))
   (let ((limit (1+ (isqrt n))))
     (dolist (p *first-primes* t)
       (cond 
         ((>= p limit)
          (return-from primep t))
         ((zerop (mod n p))
          (return-from primep nil))))))
  (t
     (dolist (p *first-primes*)
       (when (zerop (mod n p))
         (return-from primep nil)))
     (do ((limit (1+ (isqrt n)))
          (div 547 (+ div 2)))
         ((>= div limit) t)
       (when (zerop (mod n div))
         (return-from primep nil))))))
        
(defun prime-decomposition (n)
  (assert (numberp n))
  (cond
    ((or (not (integerp n))
         (< n 2))
     nil)
    ((primep n)
     (list n))
    (t
      (build-decomposition n 2 nil))))

(defun build-decomposition (n prime results)
  (cond
    ((= n 1)
     (nreverse results))
    ((> prime n)
     (error "Something went wrong with build-decomposition"))    
    ((divides prime n)
     (build-decomposition 
       (/ n prime)
       prime
       (cons prime results)))
    (t
     (build-decomposition
       n
       (next-prime prime)
       results))))

(defun next-prime (p)
  (if (< p *biggest-first-prime*)
      (nth (1+ (position p *first-primes*))
           *first-primes*)
      (do ((new-p (+ p 2) (+ new-p 2)))
          ((primep new-p) new-p))))

(defun divides (n m)
  (zerop (mod m n)))
  
#| PASCAL'S TRIANGLE
   
Pascal's triangle is an arithmetic and 
geometric figure often associated with the 
name of Blaise Pascal, but also studied 
centuries earlier in India, Persia, China 
and elsewhere. 
    
Its first few rows look like this:

    1
   1 1
  1 2 1
 1 3 3 1 
 
where each element of each row is either 1 
or the sum of the two elements right above 
it.

For example, the next row of the triangle 
would be:

              1   (since the first element 
                  of each row doesn't have 
                  two elements above it)
              4   (1 + 3)
              6   (3 + 3)
              4   (3 + 1)
              1   (since the last element of 
                  each row doesn't have two 
                  elements above it)

So the triangle now looks like this:

    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1 
  
Each row n (starting with row 0 at the top) 
shows the coefficients of the binomial 
expansion of (x + y)^n.

Task

Write a function that prints out the first   
n rows of the triangle (with f(1) yielding 
the row consisting of only the element 1).

This can be done either by summing elements 
from the previous rows or using a binary 
coefficient or combination function.

Behavior for n ≤ 0 does not need to be 
uniform, but should be noted. 
         
|#

(defun construct-triangle (n)
  (cond
    ((< n 1)
     nil)
    (t
      (let ((limit (truncate (1+ n))))
        (labels ((crec (result m)
                   (cond
                     ((= m limit) 
                      (nreverse result))
                     ((= m 1) 
                      (crec (push #(1) result)
                            (1+ m)))
                     ((= m 2) 
                      (crec (push #(1 1) result)
                            (1+ m)))
                     (t
                       (let ((prev (first result))
                             (next-row
                               (make-array m
                                 :element-type
                                 'int)))
                         (setf (aref next-row 0) 1
                               (aref next-row (1- m)) 1)
                         (loop
                           for i from 1 below (1- m)
                           do
                           (setf
                             (aref next-row i)
                             (+
                               (aref prev (1- i))
                               (aref prev i))))
                         (crec (push next-row result)
                               (1+ m)))))))
          (crec nil 1))))))

(defun print-triangle-lines (line-list)
  (dolist (line line-list)
    (loop
      for num across line
      do
      (format t "~D " num))
    (terpri)))

(defun pascal-main ()
  (dolist (n '(-15 0 1 4 10 25 4.3))
    (format t "~&~%n: ~A~%~%" n)
    (print-triangle-lines
      (construct-triangle n))))

#|
READ A SPECIFIC LINE FROM A FILE
     
Some languages have special semantics for 
obtaining a known line number from a file.
          
Task

Demonstrate how to obtain the contents of a 
specific line within a file.

For the purpose of this task demonstrate 
how the contents of the seventh line of a 
file can be obtained,   and store it in a 
variable or in memory   (for potential 
future use within the program if the code 
were to become embedded).

If the file does not contain seven lines,   
or the seventh line is empty, or too big to 
be retrieved, output an appropriate message.

If no special semantics are available for 
obtaining the required line, it is 
permissible to read line by line.

Note that empty lines are considered and 
should still be counted.

Also note that for functional languages or 
languages without variables or storage,   
it is permissible to output the extracted 
data to standard output. |#
     
;; There is no built-in facility for 
;; going to a specific line of a text file.
;; (there is a function to access a 
;; specific file position). It is easy
;; to implement such a function, however.

;; I assume that line numbers are zero-
;; indexed;

(defun nth-line (n path)
  (with-open-file (f path)
    (loop
      for i from 0 to n
      for line = (read-line f)
      finally
      (return line))))

(defun file-line-main ()
  (let ((file
          "quicklisp/local-projects/rosetta/trial-file.lisp"))
    (format t "~&Reading the 5th line of our file:")
    (format t "~&~A" (nth-line 5 file))
    (format t "~&Supplying an illegal line number (50 with fewer than 50 ~%~
            lines in the file:~%")
    (handler-case
      (nth-line 50 file)
      (end-of-file (eof) (format t "~&~A" eof)))
    (format t "~&Supplying a non-existent file:~%")     
    (handler-case
      (nth-line 10 "no-such-file.text")
      (file-error (fe) (format t "~&~A" fe)))
    (values)))      
    
#| Minkowski Question Mark Function
   
The Minkowski question-mark function 
converts the continued fraction 
representation [a0; a1, a2, a3, ...] of a 
number into a binary decimal representation 
in which the integer part a0 is unchanged 
and the a1, a2, ... become alternating runs 
of binary zeroes and ones of those lengths. 
The decimal point takes the place of the 
first zero. 
      
Thus, ?(31/7) = 71/16 because 31/7 has the 
continued fraction representation [4;2,3] 
giving the binary expansion 4 + 0.0111[2].

Among its interesting properties is that it 
maps roots of quadratic equations, which 
have repeating continued fractions, to 
rational numbers, which have repeating 
binary digits.

The question-mark function is continuous 
and monotonically increasing, so it has an 
inverse.

    Produce a function for ?(x).   
    Be careful: rational numbers have two 
    possible continued fraction 
    representations:

                  [a0;a1,... an−1,an]     and
                  [a0;a1,... an−1,an−1,1]

    Choose one of the above that will give a 
    binary expansion ending with a 1.
    
    Produce the inverse function ?^-1(x)
    
    Verify that ?(φ) = 5/3, where φ is the 
    Greek golden ratio.
    
    Verify that ?^-1(-5/9) = (√13 - 7)/6
    
    Verify that the two functions are 
    inverses of each other by showing that 
    ?^-1(?(x))=x and ?(?-1(y))=y for x, y of 
    your choice

Don't worry about precision error in the 
      last few digits. |#
                        
(defun integral-part (nstr)
  "Given a string containing a coerced
  float, return the integral portion 
  as an integer."
  (let ((integer 
          (parse-integer 
            nstr 
            :junk-allowed t)))
    (if integer
        integer
        0)))

(defun fractional-part (nstr)
  "Given a string which contains a coerced
  float, return the fractional part as 
  a ratio."
  (let ((decimal-pt-pos
          (position #\. nstr)))
    (if (and decimal-pt-pos
             (> (length nstr) 
                (1+ decimal-pt-pos)))
        (let ((decimal-subseq
                (subseq
                  nstr
                  (1+ decimal-pt-pos)
                  (position-if
                    #'alpha-char-p
                      nstr))))
          (/ (parse-integer decimal-subseq)
             (expt 10 (length decimal-subseq))))
        0)))

(defun power-multiplier (nstr)
  "Given a string containing a coerced
  float, return the integer or fraction
  that is the power of 10 in the exponentional
  part of the float. Examples:
  (power-multiplier \"123.456e3\") => 1000
  (power-multiplier \"123.456e-3\") => 1/1000
  (power-multiplier \"123.456\") => 1."
  (let ((exp-marker-position
         (position-if #'alpha-char-p nstr)))
    (if (and exp-marker-position
             (> (length nstr) (1+ exp-marker-position)))
        (expt
          10
          (parse-integer
            (subseq
              nstr
              (1+ exp-marker-position))))
        1)))
    
(defun rationalize* (x)
  "Given a real number, or 
  a string containing one, coerce the
  written representation precisely
  to a fraction (or integer).
  Note that the initial rationale 
  for this function was to supplement
  the built-in rationalize function, 
  which is broken in this implementation."
  (assert (or (stringp x) (realp x))
    ()
    "'rationalize accepts only reals or
    strings, not ~
    ~A ~A." (type-of x) x)
  (if (rationalp x)
      x
      (let ((str (if (stringp x)
                     x
                     (format nil "~A" x)))
            (sign 1))
        (case (char str 0)
          (#\- (setf sign -1)
               (setf str (subseq str 1)))
          (#\+ (setf str (subseq str 1))))
        (* sign 
           (+ (integral-part str)
              (fractional-part str))
           (power-multiplier str)))))

(defstruct (c-frac (:conc-name nil))
  "Struct that represents a continued
  fraction."
  (integral 0 :type integer)
  (fractional nil :type (list integer))
  (repeatingp nil :type boolean)
  (repeat-start nil :type (or integer null))
  (repeat-end nil :type (or integer null)))

(defun float->c-frac (x)
  "Given a float, returns the longer
  continued fraction representation 
  as a c-frac."
  (labels ((rec (real)
             (if (integerp real)
                 (list real)
                 (multiple-value-bind
                     (integral fractional)
                     (truncate real)
                   (cons
                     integral
                     (rec (/ fractional)))))))
    (let ((ratio (rationalize* x)))
      (if (integerp ratio)
          (make-c-frac
           :integral ratio
           :fractional (list 0))
          (multiple-value-bind
              (integ frac)
              (truncate ratio)
            (if (plusp frac)
                (make-c-frac
                  :integral integ
                  :fractional
                  (rec (/ frac)))
                (make-c-frac
                  :integral (- integ 1)
                  :fractional
                  (rec (/ (+ frac 1))))))))))

(defun pos-c-frac-reciprocal (c-frac)
  "Given a positive continued fraction object, 
  return the continued fraction of the 
  reciprocal. If the c-frac is repeating,
  return an approximation. Signals an error if the
  value of the c-frac is 0."
  (cond
    ((and (zerop (integral c-frac))
          (= (length (fractional c-frac)) 1)
          (zerop (first (fractional c-frac))))
     (let ((dbz 
              (make-condition 'division-by-zero)))
       (error dbz)))
    ((minusp (integral c-frac))
     (error
      "POS-C-FRAC-RECIPROCAL takes only positive c-fracs."))
    ((zerop (integral c-frac))
     (make-c-frac
      :integral (first (fractional c-frac))
      :fractional (rest (fractional c-frac))))
    (t
     (make-c-frac
      :integral 0
      :fractional
        (cons (integral c-frac)
              (fractional c-frac))))))

(defun binary-frac (digit-list
                    &optional
                    (precision 500))
  "Given a list of decimal digits, which
  are the fractional digits of a simple
  continued fraction, return a list of 
  alternating runs of 0's and 1's such
  that the initial run is of zeros, and each
  run has a length equal to the corresponding
  digit in digit list, with the exception of
  the first run, which has one fewer 0. Examples
  (binary-frac (2 3 4)) -> (0 1 1 1 0 0 0 0)
  (binary-frac (1 1 1 1)) -> (1 0 1)"
  (let ((bits nil)
        (1st-digit (first digit-list))
        (rest-prec precision))
    (cond 
      ((>= 1st-digit rest-prec)
       (list 0))
      (t
       (dotimes (_ (1- 1st-digit))
         (push 0 bits)
         (decf rest-prec))
       (do ((bit 1)
            (rest-digits 
              (copy-seq (rest digit-list)))
            (rp rest-prec (1- rp)))
           ((or (zerop rp)
                (not rest-digits)) 
            (nreverse bits))
         (cond
           ((zerop (first rest-digits))
            (setf rest-digits (rest rest-digits))
            (setf bit (if (zerop bit)
                          1
                          0)))
          (t
           (decf (first rest-digits))
           (push bit bits))))))))
 
(defun repeating-frac 
       (c-frac
        &optional
        (prec
          (max 
            9
            (* 
              (apply
                #'+
                (fractional c-frac))
              2))))
  "Given a c-frac representing an 
  irrational number and an optional number 
  of digits of precision,
  return a ratio approximation
  of the fractional part of the c-frac."
  (let* ((fractional (fractional c-frac))
         (repeated
          (subseq
            (fractional c-frac)
            (repeat-start c-frac)
            (repeat-end c-frac)))
        (bits nil)
        (rest-prec prec))
    (do ((zero-count (1- (first fractional))
                     (1- zero-count)))
        ((or (zerop zero-count)
             (zerop rest-prec)))
      (push 0 bits)
      (decf rest-prec))
    (do ((rest-digits (copy-seq (rest fractional)))
         (bit 1))
        ((zerop rest-prec) (nreverse bits))
      (cond
        ((null rest-digits)
         (setf rest-digits (copy-seq repeated)))
        ((zerop (first rest-digits))
         (setf rest-digits (rest rest-digits)
               bit (if (zerop bit)
                       1
                       0)))
        (t
         (push bit bits)
         (decf rest-prec)
         (decf (first rest-digits)))))))

(defun binary-frac->decimal-ratio (bit-list)
  "Given a list of bits representing the 
  fractional part of the ? function, return
  the equivalent decimal ratio."
  (do ((power 1/2 (/ power 2))
       (ratio 0)
       (rest-bits (copy-list bit-list)
                  (rest rest-bits)))
      ((null rest-bits) ratio)
    (unless (zerop (first rest-bits))
      (incf ratio power))))

(defun binary-transform (c-frac)
  "Transform the fractional digits of 
  a continued fraction to a ratio that is
  transform specified for these digits by
  the Minkowski ? function."
  (let ((fractional (fractional c-frac)))
    (if (repeatingp c-frac)
        (binary-frac->decimal-ratio
          (repeating-frac c-frac))
        (binary-frac->decimal-ratio
          (binary-frac fractional)))))  
           
(defun ? (x)
  "Minkowski's ? function."
  (let ((c-frac (float->c-frac x)))
    (+ (integral c-frac)
       (binary-transform c-frac))))

(defun frac->bit-list (x &optional
                         (e 1d-100))
  "Given a fractional number, 
  0 <= x < 1, return a list of 
  binary digits for the number."
  (do ((ratio (rationalize* x))
       (power 1/2 (/ power 2))
       (bits nil))
      ((or (zerop ratio)
           (< power e))
       (nreverse bits))
    (cond
      ((>= ratio power)
       (decf ratio power)
       (push 1 bits))
      (t
       (push 0 bits)))))
                  
(defun bit-list->run-list (bit-list)
  "Given a list of bits, return a list
  of decimal numbers corresponding to runs
  of bits. The first number will be the 
  (number of initial 0's) + 1. The next
  number with be the number of the next
  consecutive 1's, then the third number
  with be the number of next consecutive 
  0's, until the bit-list is exhausted."
  (let ((run-list nil)
        (rest-bits (copy-list bit-list)))
    (do ((run-length+1 1 (1+ run-length+1)))
        ((or (null rest-bits)
             (= (first rest-bits) 1))
         (push run-length+1 run-list))
      (setf rest-bits (rest rest-bits)))
    (do ((run-length 0 (1+ run-length))
         (rb rest-bits (rest rb))
         (bit 1))
        ((null rb) (progn
                     (push run-length run-list)
                     (nreverse run-list)))
      (when (/= (first rb) bit)
        (push run-length run-list)
        (setf run-length 0)
        (setf bit (if (zerop bit)
                      1
                      0))))))

(defun num->?-inverse-c-frac (x)
  "Given a number, return a c-frac 
  containing a representation of the
  result of performing the inverse ? 
  function on x."
  (multiple-value-bind
      (integ frac)
      (truncate (rationalize* x))
    (if (not (minusp frac))
        (make-c-frac
          :integral integ
          :fractional 
           (bit-list->run-list
           (frac->bit-list frac)))
        (make-c-frac
          :integral (- integ 1)
          :fractional
           (bit-list->run-list
             (frac->bit-list (+ frac 1)))))))

(defun reverse-trim (bits)
  "Given a list of bits, reverse it 
  and trim all initial zeros from 
  the reversed list"
  (do ((rev (reverse bits) (rest rev)))
      ((or (null rev)
           (not (zerop (first rev))))
       rev)))

(defun c-frac->float (c-frac)
  "Coerce a c-frac to a float"
    (do* ((rt 
           (reverse-trim (fractional c-frac))
           (rest rt))
          (cur (first rt) (first rt))
          (ratio cur (+ cur (/ ratio))))
         ((null (cdr rt)) 
                    (float 
                      (+
                       (integral c-frac)
                       (/ ratio))
                      0l0))))

(defun ?^-1 (x)
  "The inverse of Minkowski's ? function."
  (c-frac->float
    (num->?-inverse-c-frac x)))
    
#| Mian-Chowla Sequence
   
The Mian–Chowla sequence is an integer 
sequence defined recursively. 
         
Mian–Chowla is an infinite instance of a 
Sidon sequence, and belongs to the class 
known as B₂ sequences.


The sequence starts with:

        a[1] = 1

then for n > 1, an is the smallest positive 
integer such that every pairwise sum

        a[i] + a[j]

is distinct, for all i and j less than or 
equal to n.

The Task

        Find and display, here, on this page 
        the first 30 terms of the Mian–Chowla 
        sequence.
        
        Find and display, here, on this page 
        the 91st through 100th terms of the 
        Mian–Chowla sequence.


Demonstrating working through the first few 
terms longhand:

        a1 = 1

        1 + 1 = 2

Speculatively try a2 = 2

        1 + 1 = 2
        1 + 2 = 3
        2 + 2 = 4

There are no repeated sums so 2 is the next 
number in the sequence.

Speculatively try a3 = 3

        1 + 1 = 2
        1 + 2 = 3
        1 + 3 = 4
        2 + 2 = 4
        2 + 3 = 5
        3 + 3 = 6

Sum of 4 is repeated so 3 is rejected.

Speculatively try a3 = 4

        1 + 1 = 2
        1 + 2 = 3
        1 + 4 = 5
        2 + 2 = 4
        2 + 4 = 6
        4 + 4 = 8

There are no repeated sums so 4 is the next 
number in the sequence.

And so on... |#
    
(declaim (inline next-mc probe-candidate))
    
(defun main-mc ()
  (let ((mc-array (mc-series 100)))
    (format t "First 30 elements:~%")
    (print-elts mc-array 0 30)
    (format t "~%Elements 90 to 100:~%")
    (print-elts mc-array 89 100)))

(defun print-elts (array start end)
  "Print elements in an array from
  a start index (inclusive) to an 
  end index (exclusive). Prints elements
  to accommodate this phone screen, so
  that, if possible, each line will not 
  run over 45 characters."
  (do ((i start (1+ i))
       (line-len 0))
      ((= i end))
    (let* ((item (format nil "~D "
                   (aref array i)))
           (i-len (length item)))
      (cond 
        ((< (+ line-len i-len) 45)
         (incf line-len i-len)
         (format t "~A" item))
        (t
         (setf line-len i-len)
         (format t "~%~A" item))))))

(defun mc-series (n)
  "Given an integer n, return an array
  containing the first n elements of the
  Mian-Chowla sequence."
  (let ((mc
           (make-array n
             :element-type 'integer
             :fill-pointer 0)))
    (vector-push 1 mc)
    (let ((sums (list 2))
          (prev 1))
      (do ((i 1 (1+ i)))
          ((= i n) mc)
        (multiple-value-bind
            (next new-sums)
            (next-mc prev mc sums)
          (vector-push next mc)
          (setf prev next)
          (setf sums (append new-sums sums)))))))
    
(defun next-mc (prev mc sums)
  (do ((candidate (1+ prev) (1+ candidate)))
      (nil)
    (let ((all-new-sums
            (probe-candidate
             candidate
             mc
             sums)))
      (when all-new-sums
        (return-from next-mc
          (values candidate all-new-sums))))))

(defun probe-candidate (candidate mc sums)
  (let ((new-sums nil))
    (loop for mian across mc
      for sum = (+ candidate mian)
      when (position sum sums)
      do
      (return-from probe-candidate nil)
      else
      do
      (push sum new-sums)
      finally
      (return 
        (push
         (* candidate 2)
         new-sums)))))

#|
"First 30 elements:
1 2 4 8 13 21 31 45 66 81 97 123 148 182 
204 252 290 361 401 475 565 593 662 775 822 
916 970 1016 1159 1312 
Elements 90 to 100:
21948 22526 23291 23564 23881 24596 24768 
25631 26037 26255 27219 " |#
      

      
             
                  
               
          
          
          
        
           

      


                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
