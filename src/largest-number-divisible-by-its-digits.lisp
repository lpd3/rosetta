;;;; Largest Number Divisible By Its Digits

(in-package :divigibles)

(import '(alexandria:set-equal))

(defparameter *hex-digits*
  "0123456789abcdef"
  "The lowercase digits of base 16, as a string")

(defparameter *base-10-7-digits-factor*
  504
  "Requisite factor for a 7-digit divigible number in base 10")

(defparameter *base-16-11-digits-factor*
  90090
  "Requisite factor (in base 10) for an 11-digit divigible number in base 16")

(defun divigibles-main ()
  "Entry-point to the Largest Number Divisible By Its Digits task."
  (let ((base-10-answer (largest-base-10-divigible))
        (base-16-answer (largest-base-16-divigible)))
    (print "Largest number divisible by all its digits in base 10:")
    (format t "~&~D" base-10-answer)
    (print "Largest number divisible by all its digits in base 16:")
    (format t "~&~X = ~D~%" base-16-answer base-16-answer)))

(defun largest-base-10-divigible ()
  "Finds and returns the largest 7-digit decimal
  integer that is divisible by all its digits and
  has no repeated digits. If there is no such number,
  returns nil."
  (do ((candidate (largest-candidate 10) (- candidate *base-10-7-digits-factor*)))
      ((< candidate (expt 10 6)) nil)
    (when (contains-all-legal-base-10-digits-p candidate)
      (return-from largest-base-10-divigible
        candidate))))

(defun largest-base-16-divigible ()
  "Finds and returns the largest decimal integer
   whose base-16 representation has the following
   attributes:
   1. 11 digits
   2. Number is divisible by all its digits.
   3. No digit appears more than once.

   If there is no such number, return nil."
  (do ((candidate (largest-candidate 16) (- candidate *base-16-11-digits-factor*)))
      ((< candidate (expt 16 10)) nil)
    (when (contains-all-legal-base-16-digits-p candidate)
      (return-from largest-base-16-divigible
        candidate))))

(defun largest-candidate (base)
  "Given 'base', return the largest 
   candidate with which to begin the search"
  (case base
    (10 (* (floor (expt 10 7) *base-10-7-digits-factor*) *base-10-7-digits-factor*))
    (16 (* (floor (expt 16 11) *base-16-11-digits-factor*) *base-16-11-digits-factor*))
    (otherwise (error "Bases 10 and 16 only, not ~A ~S" (type-of base) base))))

(defparameter *legal-base-10-digits*
  '(1 2 3 6 7 8 9)
  "The legal digits in a 7-digit decimal digible")

(defparameter *legal-base-16-digits*
  '(#\1 #\2 #\3 #\5 #\7 #\9 #\a #\b #\d #\e #\f)
  "The legal digits, as characters, of an 11-digit base 16 divigible.")

(defun contains-all-legal-base-10-digits-p (n)
  "Does this 7-digit integer divisible by *base-10-7-digits-factor*
   contain all digits except 0, 4, and 5?"
  (let ((digits (get-base-10-digits n)))
    (set-equal digits *legal-base-10-digits*)))

(defun contains-all-legal-base-16-digits-p (n)
  "Does this number, when expressed in hex, contain
   all legal digits of an 11-digit hexadecimal divigible?"
  (let ((digits (int-to-hex-digit-char-list n)))
    (set-equal digits *legal-base-16-digits*)))

(defun get-base-10-digits (n)
  "Given a decimal integer, return a list of its digits, in order."
  (cond ((zerop n)
         (list 0))
        (t (do ((digits nil)
                (remainder n (truncate remainder 10)))
               ((zerop remainder) digits)
             (push (mod remainder 10) digits)))))

(defun int-to-hex-digit-char-list (n)
  "Given (decimal) integer n, return a list containing the 
   characters comprising n's hexadecimal representation, in order."
  (cond
    ((zerop n)
     '(#\0))
    (t (do ((remainder n (truncate remainder 16))
            (digits nil))
           ((zerop remainder) digits)
         (push (char *hex-digits* (mod remainder 16)) digits)))))
