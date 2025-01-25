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

(defun binary-search (item seq test comp &key (key #'identity))
  "Binary search algorith  for any kind of sequence. Searches the
  a sequence, which must be sorted, for an item.
  
  Required args:

  ITEM (type t): the item that we are looking for.
  SEQ (proper sequence) the sorted seqence in which the
    search is performed.
  TEST (function of 2 args that returns a value): Function
    that determines whether or not the item and an element
    are equal or otherwise equivalent.
  COMP (function of 2 args that returns a value): Function
    that compares the items. When this function returns 
    non-nil, then the first arg should be found farther to
    the left in the sequence then the second arg.

  Keyword (optional) arg:
  
  KEY: A function of one arg that extracts a value from
    a sequence element for comparison with ITEM. Defaults
    to #'identity.

  Finds the midpoint of  SEQ, and KEY to extract a value from
  SEQ at the midpint. Invokes TEST on ITEM and the value
  extracted. If the result is non-nil, the extracted value is returned 
  and the algorithm ends. Otherwise, invokes COMP on item and value.
  If the result is non-nil, the elements to the left of the midpoint
  are considered (and the items to the right are never considered). 
  Finds the midpoint of the items to the left and the algorithm 
  repeats. If COMP returns NIL, then the items to the right of the
  midpoint are the next focus (and those to the left will never be 
  considered). Continues until either the element is found, or 
  the endpoints of the region under consideration are adjacent in 
  the sequence, at which point NIL is returned.

  If the sequence is not in some kind of order, or the COMP 
  function is not correct for the task, the result returned will 
  be useless."
  (when (not (typep seq 'alexandria:proper-sequence))
    (error 'type-error*
           :argument seq
           :type (type-of seq)
           :expected-type 'proper-sequence
           :location "BINARY-SEARCH: seq"))
  (when (not (functionp test))
    (error 'type-error*
           :argument test
           :type (type-of test)
           :expected-type "FUNCTION of 2 args"
           :location "BINARY-SEARCH: test"))
  (when (not (functionp comp))
    (error 'type-error*
           :argument comp
           :type (type-of comp)
           :expected-type "FUNCTION of 2 args"
           :location "BINARY-SEARCH: comp"))
  (when (not (functionp key))
    (error 'type-error*
           :argument key
           :type (type-of key)
           :expected-type "FUNCTION of one arg"
           :location "BINARY-SEARCH: key"))
  (when (zerop (length seq))
    (return-from binary-search nil))
  (do* ((left 0)
        (right (1- (length seq)))
        (mid (floor (+ left right) 2)))
       ((> left right)
        ;; No more logical places to search. Item must not match
        ;; any value in seq. Return NIL.
        nil)
    (let ((value (funcall key (elt seq mid))))
      (cond
        ((funcall test item value)
         ;; item matches value. Return value
         (return-from binary-search value))
        ((funcall comp item value)
         ;; item, if in seq, must be to the left of value.
         ;; move right to mid - 1 and recalculate mid.
         (setf right (1- mid)
               mid (floor (+ left right) 2)))
        (t
         ;; item, if in seq, must be to the right of value.
         ;; set left to mid + 1 and recalculate mid
         (setf left (1+ mid)
               mid (floor (+ left right) 2)))))))


#+ecl (defun rationalize (x)
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
                  (otherwise            ; c is a digit char
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
