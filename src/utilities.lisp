;;;; utlilities.lisp
                   
(in-package :ros-utils)

(defmacro printout->string (&body body)
  "Given a body, returns as a string any 
  printout produced by the body."
  (let ((str (gensym "STR-STREAM")))
    `(with-output-to-string (,str)
       (let ((*standard-output* ,str))
         ,@body))))
         
(defmacro broadcast-printout 
   ((&optional 
       (file-name "quicklisp/local-projects/rosetta/printout.txt.lisp"))
    &body body)
  "Given an optional filename and body, returns any printout
  produced by the body as a string and 
  writes it to a file."
  (with-gensyms (string-stream file-stream)
    `(with-output-to-string (,string-stream)
       (with-open-file (,file-stream
                        ,file-name
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :overwrite)
         (let ((*standard-output*
                (make-broadcast-stream
                 ,string-stream
                 ,file-stream)))
           ,@body)))))

(defmacro abbrev (short long)
  "Given two symbols, the second of which
  is an interned symbol (presumably one
  with a long name) that is assigned 
  an operator, assign the same operator
  to the first (presumably shorter)
  symbol."
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  "Given a series of pairs of symbols,
  assign the first symbol in each pair
  the same operator already assigned to
  the second symbol in each pair."
  `(progn
     ,@(mapcar #'(lambda (pair)
                   `(abbrev ,@pair))
                 (batches names 2))))
                 
(abbrevs
 mbind multiple-value-bind
 dbind destructuring-bind)

(defun rationalize (x)
  "Given a real number, return
  a rational number whose 
  value is identical to the 
  displayed value of the reat 
  (rather than the binary
  approximation.) This is a 
  native CL function, but for some
  reason it does not work as advertised
  in ECL on Android."
  (assert (realp x)
    ()
    "RATIONALIZE takes a real argument, not %A %S."
    (type-of x) x)
  (if (rationalp x)
      x
      (do* ((nstr (format nil "~F" x))
            (len (length nstr))
            (mantissa 0)
            (i 0 (1+ i))
            (divider 1)
            (sign 1)
            (zero-code (char-code #\0))
            (point-seen nil))
           ((= i len)
            (* sign (/ mantissa divider)))
        (let ((c (char nstr i)))
          (case c
            (#\+ nil)
            (#\- (setf sign -1))
            (#\. (setf point-seen t))
            (otherwise ; c is a digit char
              (let ((val
                     (- (char-code c)
                        zero-code)))
                (setf mantissa
                  (+
                   (* mantissa 10)
                   (if (minusp mantissa)
                       (- val)
                       val)))
                (when point-seen
                  (setf divider
                    (* divider 10))))))))))
           
                                 
       


