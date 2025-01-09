;;;; rc-002.lisp
(in-package :ros-01)

;;; Combinations With Repetitions

#|
The set of combinations with repetitions is 
computed from a set, S (of cardinality n, 
and a size of resulting selection, by 
reporting the sets of cardinality k where 
each member of those sets is chosen from S. 
In the real world, it is about choosing sets 
where there is a “large” supply of each type 
of element and where the order of choice 
does not matter. For example:
    
    Q: How many ways can a person choose two 
    doughnuts from a store selling three 
    types of doughnut: iced, jam, and plain? 
    (i.e., S is {iced, jam, plain}, |S| = 3, 
    and k = 2.)

    A: 6: {iced, iced}; {iced, jam}; 
          {iced, plain}; {jam, jam}; 
          {jam, plain}; {plain, plain}.
          
Note that both the order of items within a 
pair, and the order of the pairs given in 
the answer is not significant; the pairs 
represent multisets.
Also note that doughnut can also be spelled 
donut.


Task

    Write a function/program/routine/.. to 
    generate all the combinations with 
    repetitions of n types of things taken k 
    at a time and use it to show an answer 
    to the doughnut example above.
    For extra credit, use the function to 
    compute and show just the number of ways 
    of choosing three doughnuts from a 
    choice of ten types of doughnut. Do not 
    show the individual choices for this 
    part. |#
           
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
           
(defun combinations-with-repetition (set k)
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
  with repetition can be made with n distinct
  elements from which to choose."
  (if (or (zerop n)
          (zerop k))
      0
      (/
        (factorial (+ n (1- k)))
        (* (factorial k)
           (factorial (1- n))))))
           
;;; Associative Array/Creation

#|
The goal is to create an associative array 
(also known as a dictionary, map, or hash).|#
      
(defparameter *my-hash*
  (make-hash-table
   :test #'equal
   :size 200
   :rehash-size 2.0
   :rehash-threshold 0.75)
  "The hash-table is a basic type in 
  common lisp. All arguments 
  in the constructor are optional.
  The :test arg takes one of four equality
  functions used for the keys.
  #'eq works only with symbols. #'eql 
  (the default) works with symbols, 
  numbers or characters. #'equal works
  with symbols, numbers, characters, 
  flat lists, strings, and bitvectors.
  #'equalp works with all that #'equal does,
  and in addition, works with arrays and 
  treats strings in a case-insensitive 
  manner. The more general the equality test,
  the slower the operation of lookups.
  The other args control the 
  initial size and under what conditions
  the table is rehashed.
  The access function is GETHASH, and is
  used for lookups like so:
  (gethash <key> <table> &optional <default>).
  The function returns two values: the value
  associated with the key and either T or 
  NIL which inform about the presence of 
  the key. If the key is not found, 
  the first value will be the default, if 
  supplied, or NIL if not.
  As typical for CL, the setter is 
  a gethash wrapped in a setf.
  E.g. (setf (gethash 12 my-table) 'joe)
  will associate the value 'joe with 
  the key 12 in my-table. There are several
  other provided functions, including 
  maphash, which maps over the entries. Two
  other data structures have associative 
  properties in CL: the association list
  and the property list. Both are built
  on single linked lists, and thus,
  access is O(n) rather than O(1). Nevertheless,
  alists offer some advantages over 
  hash-tables.
  (1). They are more efficient then
       hash tables when there are 10 or 
       fewer entries.
  (2). They can be ordered.
  (3). They can contain an arbitrary number
       of entries with the same key. This
       can be useful for temporarily 
       shadowing an entry.
  (4). Being built on lisps, they take
       advantage of the plethora of nattve
       list and sequence functions.
  A few disadvantages of native CL 
  hash-tables: 1. There is no built-in
  literal or displayable form. 2. There
  are no native functions for common
  manipulations such as listing the keys 
  or values. These and other disadvantages
  are ameliorated by the ubiquitous 
  third-party utility libraries
  alexandria and serapeum.")   
  
;;; Inconsummate Numbers in Base 10

#|
A consummate number is a non-negative 
integer that can be formed by some integer 
N divided by the the digital sum of N. 
  
For instance

47 is a consummate number.

   846 / (8 + 4 + 6) = 47

On the other hand, there are integers that 
can not be formed by a ratio of any integer 
over its digital sum. These numbers are 
known as inconsummate numbers.   
      
62 is an inconsummate number. There is no 
integer ratio of an integer to its digital 
sum that will result in 62.

The base that a number is expressed in will 
affect whether it is inconsummate or not. 
This task will be restricted to base 10.


Task

    Write a routine to find inconsummate 
    numbers in base 10;
    Use that routine to find and display the 
    first fifty inconsummate numbers.


Stretch

    Use that routine to find and display the 
    one thousandth inconsummate number.
    
|#
 
;;; In most cases, I could use the external
;;; library CL-ANA to provide combinations
;;; with replacement. But the dependencies
;;; can't be loaded on this platform. So
;;; I have used my own function from
;;; the previous project above.
                              
(defun inconsummate-numbers ()
    (iter
     (with index = 1)
     (for n from 1)
     (while (<= index 1000))
     (block inner
       (iter
        (for len from 
          (max
           1
           (ceiling (log n 10))))
        (when (< (* 9 n len)
                 (expt 10 (1- len)))
          (when (<= index 50)
            (format t "~&~D: ~D"
                    index n))
          (when (>= index 1000)
            (format t "~&~D: ~D"
                    index n)
            (return-from inconsummate-numbers))
          (incf index)
          (return-from inner))
        (dolist (digits 
                  (combinations-with-repetition
                   (iota 10) len))
          (let ((sum
                 (apply #'+ digits)))
            (when
              (and 
                (> sum 0)
                (string=
                  (sort
                    (format nil "~D"
                                (* n sum))
                  #'char<)
                  (format nil "~{~D~}"
                              digits)))
              (return-from inner))))))))

#| 
Inconsummate numbers in base 10, #1-50:
             
62, 63, 65, 75, 84, 95, 161, 173, 195, 216, 
261, 266, 272, 276, 326, 371, 372, 377, 381,
383, 386, 387, 395, 411, 416, 422, 426, 431,
432, 438, 441, 443, 461, 466, 471, 476, 482,
483, 486, 488, 491, 492, 493, 494, 497, 498,
516, 521, 522, 527
     
1000th base 10 Inconsummate number:
       
6996

|#    
     
;;; Loops/Break

#|
Show a loop which prints random numbers 
(each number newly generated each loop) 
from 0 to 19 (inclusive). 
     
Task

If a number is 10, stop the loop after 
printing it, and do not generate any further 
numbers.

Otherwise, generate and print a second 
random number before restarting the loop.

If the number 10 is never generated as the 
first number in a loop, loop forever. |#
      
;; There are several ways of accomplishing
;; this.

;; a very Lispy way is with recursion

(defun loops-rec ()
  (let ((n (random 20)))
    (print n)
    (unless (= n 10)
      (loops-rec))))

;; The most general iteration construct is
;; DO

(defun loops-do ()
  (do ((n (print (random 20)) ; initial value
          (print (random 20)))) ; subsequent values
      ((= n 10)))) ; end condition

;; A perhaps more readable version with the
;; powerful but controversial LOOP. 
;; Controversial: the syntax is different
;; from the rest of CL. It can also be 
;; finicky about the order of clauses.

(defun loops-loop ()
  (loop
    for n = (print (random 20))
    when
    (= n 10)
    do
    (return))) ;; return and return-from
               ;; are Lisp's "break". 
               ;; In CL, "break" invokes
               ;; the debugger
               
;; There are several 3rd-party libraries
;; that supply alternative solutions.
;; Perhaps the most popular is 
;; ITER from the ITERATE package. It is
;; similar to LOOP, but more capable, 
;;  extensible, less finicky, and with
;;  syntax that more closely matches
;; the rest of lisp. I use it myself.

; (use-package :iterate) 
                        
(defun loops-iter ()
  (iter
   (for n = (print (random 20)))
   (when (= n 10)
     (return))))
     
;;; I'm A Software Engineer, Get Me Out of 
;;; Here.

#|

Your latest contract has hit a snag. You 
came to update the army payroll system, but 
awoke this morning to the sound of mortars 
landing not far away and panicked generals 
banging on you door. The President has 
loaded his gold on trucks and needs to find 
the shortest route to safety. You are given 
the following map. The top left hand corner 
is (0,0). You and The President are located 
at HQ in the centre of the country (11,11). 
Cells marked 0 indicate safety. Numbers other 
than 0 indicate the number of cells that his 
party will travel in a day in any direction 
up, down, left, right, or diagonally. 
    
         00000         
      00003130000      
    000321322221000    
   00231222432132200   
  0041433223233211100  
  0232231612142618530  
 003152122326114121200 
 031252235216111132210 
 022211246332311115210 
00113232262121317213200
03152118212313211411110
03231234121132221411410
03513213411311414112320
00427534125412213211400
 013322444412122123210 
 015132331312411123120 
 003333612214233913300 
  0219126511415312570  
  0021321524341325100  
   00211415413523200   
    000122111322000    
      00001120000      
         00000 
         
Part 1 Use Dijkstra's algorithm to find a 
list of the shortest routes from HQ to 
safety.
     
Part 2
Six days later and you are called to another 
briefing. The good news is The President and 
his gold are safe, so your invoice may be 
paid if you can get out of here. To do this 
a number of troop repositions will be 
required. It is concluded that you need to 
know the shortest route from each cell to 
every other cell. You decide to use Floyd's 
algorithm. Print the shortest route from 
(21,11) to (1,11) and from (1,11) to (21,11),
and the longest shortest route between any 
two points.
    
Extra Credit

Is there any cell in the country that can not 
be reached from HQ?
    
Which cells will it take longest to send 
reinforcements to from HQ?        
               
|#   
    
(defparameter *data*
   ".........00000.........
......00003130000......
....000321322221000....
...00231222432132200...
..0041433223233211100..
..0232231612142618530..
.003152122326114121200.
.031252235216111132210.
.022211246332311115210.
00113232262121317213200
03152118212313211411110
03231234121132221411410
03513213411311414112320
00427534125412213211400
.013322444412122123210.
.015132331312411123120.
.003333612214233913300.
..0219126511415312570..
..0021321524341325100..
...00211415413523200...
....000122111322000....
......00001120000......
.........00000.........")

(defun data->rows (data)
  "Separates the data, a string, 
  into a list of strings, where each string 
  is a row."
  (uiop:split-string data :separator 
                          (list
                           #\Newline)))

(defun rows->array (rows)
  "Creates a 2d array from a list of 
  rows of strings. Digit characters are
  coerced to integers. Other characters
  are unchanged."
  (iter
   (with array = (make-array
                  (list
                   (length rows)
                   (length (first rows)))))
   (for i from 0)
   (for row in rows)
   (iter
    (for j from 0)
    (for c in-string row)
    (setf (aref array i j)
          (if (digit-char-p c)
              (- (char-code c)
                 (char-code #\0))
              c)))
   (finally (return array))))
   
(defun array->vertex-dict (array)
  "Creates a vertex dictionary from 
  the array. See *vertex-dict* and the
  task description for futher details."
  (destructuring-bind
      (nrows ncols)
      (array-dimensions array)
    (let ((table (dict #'equal)))
      (iter
       (for i from 0 below nrows)
       (iter
        (for j from 0 below ncols)
        (let ((val (aref array i j))
              (neighbors nil))
          (when (and (numberp val)
                     (> val 0))
                (setf neighbors
                  (remove-if-not
                   #'(lambda (coords)
                       (and
                        (< -1 (first coords) nrows)
                        (< -1 (second coords) ncols)
                        (numberp
                         (aref array
                           (first coords)
                           (second coords)))))
                     (list
                      (list i (+ j val))
                      (list i (- j val))
                      (list (+ i val) j)
                      (list (- i val) j)
                      (list (+ i val)
                            (+ j val))
                      (list (- i val)
                            (- j val))
                      (list (+ i val)
                            (- j val))
                      (list (- i val)
                            (+ j val))))))
          (setf
           (gethash (list i j) table)
           (list
            (list* :value val)
            (list* :neighbors neighbors)))))
       (finally
        (return table))))))

(defparameter *graph*
  (rows->array
   (data->rows *data*))
  "An array of the data, with numerical
  characters coerced to integers.")

(defparameter *vertex-dict*
  (array->vertex-dict *graph*)
  "A hash table, where keys 
  are coordinate lists from the graph,
  and values are association lists
  with :value keyed to the item found
  at the coordinate in the graph and
  :neighbors to a list of valid coordinate 
  lists corresponding to cells of the
  array that contain numbers. Note that
  this is a directed graph, since 
  a cell containing a zero is a destination
  with no edges, whereas cells containing
  other integers may have one or more 
  edges which link to a 0.")

(defun init-distance-dict (vertices start)
  "Given a hash table of vertices and a starting
  coordinate pair (list), return a 
  new hash table with the same keys and 
  alists for the values. Each alist has
  entries :distance (set to 0 for start
  and infinity everywhere else) and :path
  (set to a list containing start for start
   and nil everywhere else)"
  (let ((distances
         (maphash-new
          #'(lambda (k _)
              (declare (ignore _))
              (values
               k (list (list* :distance 
                              +infinity+)
                       (list* :path
                              nil))))
            vertices
            :test #'equal)))
    (setf (gethash start distances)
          (list (list* :distance 0)
                (list* :path
                       (list start))))
    distances))
    
(defun distance (node distances)
  (cdr
   (assoc :distance
    (gethash node distances))))
    
(defun %set-distance (node distances distance)
  (rplacd
   (assoc :distance
     (gethash node distances))
   distance))

(defsetf distance %set-distance)
                 
(defun path (node distances)
  (cdr
    (assoc :path
      (gethash node distances))))
    
(defun %set-path (node distances path)
  (rplacd
   (assoc :path
     (gethash node distances))
   path))
     
(defsetf path %set-path)

(defun value (node vertices)
  (cdr
    (assoc :value
      (gethash node vertices))))

(defun neighbors (node vertices)
  (cdr
   (assoc :neighbors
     (gethash node vertices))))

(defun shortest-paths-to-safety (start
                                 &optional
                                 (graph
                                  *graph*)
                                 (vertices
                                  *vertex-dict*))
  "Given a starting coordinate (list)
  and optional graph (array, defaulting to
  *graph*) and vertices (hash table, 
  defaulting to *vertices), return a 
  list of the shortest paths to 
  a cell having a 0 value."
  (declare (ignore graph))
  (iter
    (with not-seen = (hash-table-keys vertices))
    (with distances = (init-distance-dict
                       vertices start))
    (with node-queue = (queue start))
    (with path-list = nil)
    (until (queue-empty-p node-queue))
    (let ((cur (deq node-queue)))
      (if (zerop (value cur vertices))
          (push (list*
                 (path cur distances)
                 (1- (length (path cur distances))))
                path-list)
          (dolist (neighbor (neighbors cur vertices))
            (when 
                (member neighbor not-seen :test #'equal)
              (when
                  (< (1+ (distance cur distances))
                     (distance neighbor distances))
                (setf
                 (distance neighbor distances)
                 (1+ (distance cur distances))
                 (path neighbor distances)
                 (append
                  (path cur distances)
                  (list neighbor)))
                (enq neighbor node-queue)))))
      (setf not-seen
            (remove 
             cur 
             not-seen 
             :test #'equal)))
    (finally
     (return 
       (sort path-list #'< :key #'cdr)))))

(defun shortest-of-short (paths-to-safety)
  "Given an alist with paths as keys and
  lengths as values, and 
  sorted by increasing path length, strip the 
  table of 
  any entries that do not tie for shortest."
  (remove-if-not
   #'(lambda (entry)
       (equal (cdr entry) (cdar paths-to-safety)))
     paths-to-safety))
       
(defun init-floyd-warshaw-array (&optional
                                 (graph
                                  *graph*)
                                 (vertices
                                  *vertex-dict*))
  (iter
   (with (nrows ncols) = (array-dimensions graph))
   (with fw-array =
         (make-array
          (list
           nrows
           ncols
           nrows
           ncols)
          :initial-element +infinity+))
   (for i from 0 below nrows)
   (iter
    (for j from 0 below ncols)
    (setf
      (aref fw-array i j i j)
      0)
    (dolist (neighbor (neighbors (list i j) vertices))
      (setf
       (aref fw-array i j (car neighbor) (cadr neighbor))
       1)))
   (finally
    (return fw-array))))

(defun floyd-warshaw (&optional
                      (graph *graph*)
                      (vertices
                       *vertex-dict*))
  "Given a graph (array) and a hash table
  containing information about each 
  vertex, return an array that lists
  the shortest paths from every vertex
  to every other vertex."
  (iter
    (with fw = (init-floyd-warshaw-array
                 graph vertices))
    (with (nrows ncols) = (array-dimensions graph))
    (for inter-row from 0 below nrows)
    (iter
     (for inter-col from 0 below ncols)
     (iter
      (for start-row from 0 below nrows)
      (iter
       (for start-col from 0 below ncols)
       (iter
        (for end-row from 0 below nrows)
        (iter
         (for end-col from 0 below ncols)
         (setf
          (aref fw start-row start-col end-row end-col)
          (min
           (aref fw start-row start-col end-row end-col)
           (+
            (aref fw start-row start-col
                     inter-row inter-col)
            (aref fw inter-row inter-col
                     end-row end-col)))))))))
    (finally
     (return fw))))

(defun find-path (source destination array 
                   &optional
                   (vertices *vertex-dict*))
  (let ((srow (car source))
        (scol (cadr source))
        (drow (car destination))
        (dcol (cadr destination)))
    (when 
      (< (aref array srow scol drow dcol)
         +infinity+)
      (let ((search-queue
             (apply
              #'queue
              (mapcar 
                #'(lambda (neighbor)
                    (list source neighbor))
                  (neighbors source vertices)))))
        (iter
         (until (queue-empty-p search-queue))
         (for cur-path = (deq search-queue))
         (for next-node = (car (last cur-path)))
         (when
           (equal next-node destination)
           (return-from find-path
             cur-path))
         (dolist (neighbor
                  (neighbors next-node vertices))
           (when 
             (not
              (member neighbor cur-path
                :test #'equal))
             (enq
              (append
               (copy-list cur-path)
               (list neighbor))
              search-queue))))))))
              
(defun longest-shortest-path (fw)
  (iter 
    (with source = nil)
    (with destination = nil)
    (with longest = -1)
    (with (nrows ncols) = (array-dimensions fw))
    (for source-row from 0 below nrows)
    (iter
     (for source-col from 0 below ncols)
     (iter
      (for dest-row from 0 below nrows)
      (iter
       (for dest-col from 0 below ncols)
       (let ((cur-length
              (aref 
                fw 
                source-row
                source-col
                dest-row
                dest-col)))
         (when
           (and (< cur-length +infinity+)
                (> cur-length longest))
           (setf
             source (list source-row source-col)
             destination (list dest-row dest-col)
             longest cur-length))))))
    (finally
     (return
      (values
       longest
       source
       destination)))))

(defun im-an-engineer-primary ()
  (format t "~&~&The shortest paths to safety are ~%~
          ~A"
    (shortest-of-short
     (shortest-paths-to-safety '(11 11))))
  (let ((fw (floyd-warshaw)))
    (format t
      "~&~&The shortest path from (21 11) to (1 11) ~%~
      is ~D days long. One path is ~%~
      ~A ~%~
      The shortest path from (1 11) to (21 11) ~%~
      is ~D days long. One path is ~%~
      ~A"
      (aref fw 21 11 1 11)
      (find-path
       '(21 11)
       '(1 11)
        fw)
      (aref fw 1 11 21 11)
      (find-path
       '(1 11)
       '(21 11)
        fw))
    (multiple-value-bind
        (dist source dest)
        (longest-shortest-path fw)
      (format t
        "~&~&Besides the unconnected nodes, ~%~
        the longest shortest path takes ~D days. ~%~
        One example is the path between ~%~
        ~A and ~A"
        dist
        source
        dest))))

#|
"The shortest paths to safety are 
((((11 11) (12 10) (13 10) (18 15) (21 15)) . 4)
 (((11 11) (12 10) (13 10) (13 5) (13 0)) . 4)
 (((11 11) (10 10) (8 10) (5 13) (1 13)) . 4)
 (((11 11) (10 10) (8 10) (5 7) (2 4)) . 4)
 (((11 11) (10 11) (13 8) (14 7) (18 3)) . 4)
 (((11 11) (10 11) (7 8) (4 5) (3 4)) . 4)
 (((11 11) (10 11) (7 8) (7 5) (12 0)) . 4)
 (((11 11) (10 11) (7 8) (7 5) (2 5)) . 4)
 (((11 11) (10 11) (7 11) (7 12) (1 6)) . 4)
 (((11 11) (10 11) (13 11) (17 15) (20 18)) . 4)
 (((11 11) (10 11) (10 14) (12 16) (16 20)) . 4)
 (((11 11) (12 11) (9 14) (6 17) (4 19)) . 4)
 (((11 11) (12 11) (15 8) (18 11) (22 11)) . 4)
 (((11 11) (12 11) (15 8) (15 5) (18 2)) . 4)
 (((11 11) (12 11) (12 8) (16 4) (13 1)) . 4)
 (((11 11) (12 11) (12 8) (16 4) (16 1)) . 4)
 (((11 11) (12 11) (12 8) (8 4) (6 2)) . 4)
 (((11 11) (12 11) (12 8) (12 4) (15 1)) . 4)
 (((11 11) (12 11) (12 8) (12 4) (9 1)) . 4)
 (((11 11) (12 11) (12 14) (8 18) (3 18)) . 4)
 (((11 11) (12 11) (12 14) (16 18) (13 21)) . 4)
 (((11 11) (12 11) (12 14) (16 18) (16 21)) . 4)
 (((11 11) (11 12) (14 9) (18 5) (19 4)) . 4)
 (((11 11) (11 12) (14 9) (18 13) (22 9)) . 4)
 (((11 11) (11 12) (14 9) (18 13) (22 13)) . 4)
 (((11 11) (11 12) (8 9) (2 15) (1 16)) . 4)
 (((11 11) (11 12) (8 9) (2 15) (1 14)) . 4)
 (((11 11) (11 12) (8 9) (2 15) (1 15)) . 4)
 (((11 11) (11 12) (8 9) (2 15) (2 16)) . 4)
 (((11 11) (11 12) (8 9) (14 3) (11 0)) . 4)
 (((11 11) (11 12) (8 9) (2 9) (1 8)) . 4)
 (((11 11) (11 12) (8 9) (2 9) (1 9)) . 4)
 (((11 11) (11 12) (8 9) (8 3) (6 1)) . 4)
 (((11 11) (11 12) (8 9) (8 3) (8 1)) . 4)
 (((11 11) (11 12) (14 15) (16 15) (19 18)) . 4)
 (((11 11) (11 12) (8 12) (6 12) (0 12)) . 4)
 (((11 11) (11 12) (14 12) (16 12) (20 16)) . 4)
 (((11 11) (11 12) (11 9) (9 9) (3 3)) . 4)
 (((11 11) (11 12) (11 15) (11 17) (7 21)) . 4)
 (((11 11) (11 12) (11 15) (11 17) (15 21)) . 4))
The shortest path from (21 11) to (1 11) 
is 7 days long. One path is 
((21 11) (21 12) (19 10) (14 10) (10 10) (8 8) (4 8) (1 11)) 
The shortest path from (1 11) to (21 11) 
is 7 days long. One path is 
((1 11) (2 10) (5 13) (9 9) (15 3) (20 8) (20 10) (21 11))
Besides the unconnected nodes, 
the longest shortest path takes 9 days. 
One example is the path between 
(1 11) and (20 14)"   
 
|# 
  
;;;; Sort A List of Object Identifiers

#|
Object identifiers (OID) are strings used to 
identify objects in network data. 
         
Task

Show how to sort a list of OIDs, in their 
natural sort order.
        
Details

    An OID consists of one or more non-
    negative integers in base 10, separated 
    by dots. It starts and ends with a 
    number.
    Their natural sort order is 
    lexicographical with regard to the 
    dot-separated fields, using numeric 
    comparison between fields.
    
Test case

Input (list of strings)

1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11150.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11150.3.4.0   
                         
Output (list of strings)

1.3.6.1.4.1.11.2.17.5.2.0.79
1.3.6.1.4.1.11.2.17.19.3.4.0.1
1.3.6.1.4.1.11.2.17.19.3.4.0.4
1.3.6.1.4.1.11.2.17.19.3.4.0.10
1.3.6.1.4.1.11150.3.4.0
1.3.6.1.4.1.11150.3.4.0.1     
                             
|#

(defun oid->int-list (oid)
  (mapcar #'parse-integer
    (uiop:split-string oid :separator ".")))

(defun oid< (oid1 oid2)
  (let ((lst1 (oid->int-list oid1))
        (lst2 (oid->int-list oid2)))
    (labels ((rec (rest1 rest2)
               (cond
                ((null rest2)
                 nil)
                ((null rest1)
                 t)
                ((< (car rest1)
                    (car rest2))
                 t)
                ((> (car rest1)
                    (car rest2))
                 nil)
                (t
                 (rec (cdr rest1)
                      (cdr rest2))))))
      (rec lst1 lst2)))) 
                        
(defun sort-oid (oid-list)
  (sort oid-list #'oid<)) 
                         
(defparameter *oid-test-case*
  (list
   "1.3.6.1.4.1.11.2.17.19.3.4.0.10"
   "1.3.6.1.4.1.11.2.17.5.2.0.79"
   "1.3.6.1.4.1.11.2.17.19.3.4.0.4"
   "1.3.6.1.4.1.11150.3.4.0.1"
   "1.3.6.1.4.1.11.2.17.19.3.4.0.1"
   "1.3.6.1.4.1.11150.3.4.0"))   
                                
(defun oid-main ()
  (sort-oid *oid-test-case*))

;; Works as advertised.

;;;; Juggler Sequence

#|

Juggler sequences were publicized by an
American mathematician and author Clifford 
A. Pickover. The name of the sequence gets 
it's name from the similarity of the rising 
and falling nature of the numbers in the 
sequences, much like balls in the hands of a 
juggler.

Description

A juggler sequence is an integer sequence 
that starts with a positive integer a[0], 
with each subsequent term in the sequence 
being defined by the recurrence relation:

   a[k + 1]  =  floor(a[k] ^ 0.5)    
                if a[k] is even    
                
   or       
            
   a[k + 1]  =  floor(a[k] ^ 1.5)    
                if a[k] is odd                                   

If a juggler sequence reaches 1, then all 
subsequent terms are equal to 1. This is 
known to be the case for initial terms up to 
1,000,000 but it is not known whether all 
juggler sequences after that will eventually 
reach 1.

Task

Compute and show here the following 
statistics for juggler sequences with an 
initial term of a[n] where n is between 20 
and 39 inclusive:

    l[n] - the number of terms needed to 
           reach 1.
    h[n] - the maximum value reached in that 
           sequence.
    i[n] - the index of the term (starting 
           from 0) at which the maximum is 
           (first) reached.

If your language supports big integers with 
an integer square root function, also 
compute and show here the same statistics 
for as many as you reasonably can of the 
following values for n:

113, 173, 193, 2183, 11229, 15065, 15845, 
30817, 48443, 275485, 1267909, 2264915, 
5812827

Those with fast languages and fast machines 
may also like to try their luck at 
n = 7110201.

However, as h[n] for most of these numbers 
is thousands or millions of digits long, 
show instead of h[n]:

    d[n] - the number of digits in h[n]
    
|#

(defun next-juggler (n)
  (if (evenp n)
      (isqrt n)
      (isqrt (* n n n))))

;; Strangely, I don't get the right results
;; when I use (expt n 3/2). Smaller numbers
;; give correct results. Larger numbers 
;; finish quickly with incorrect results.

(defun juggler-stats (n)
  (iter
   (with h = -1)
   (with i = -1)
   (for l from 1)
   (for jug initially n then (next-juggler jug))
   (when (= jug 1)
     (return
      (values
       l h i)))
   (when (> jug h)
     (setf h jug
           i l))))

(defun juggler-main ()
  (format t "~&Number, terms to 1, max, terms to max.~%")
  (iter
   (for n from 20 to 39)
   (multiple-value-bind
       (l h i)
       (juggler-stats n)
     (format t "~%~D ~D ~D ~D"
       n l h i)))
  (format t "~&~%Number, terms to 1, number of digits in max,~%~
               terms to max.~%")
  (iter
   (for n in 
     '(113 173 193 2183 11229 15065 15845 
       30817 48443 275485 1267909 2264915 
       5812827 7110201))
   (multiple-value-bind
       (l h i)
       (juggler-stats n)
     (format t "~%~D ~D ~D ~D"
       n
       l
       (ceiling (log h 10))
       i))))
       
#|

Number, terms to 1, max, terms to max.

20 4 20 1
21 10 140 5
22 4 22 1
23 10 110 2
24 4 24 1
25 12 52214 4
26 7 36 4
27 7 140 2
28 7 36 4
29 10 156 2
30 7 36 4
31 7 172 2
32 7 36 4
33 9 2598 3
34 7 36 4
35 9 2978 3
36 4 36 1
37 18 24906114455136 9
38 4 38 1
39 15 233046 4

Number, terms to 1, number of digits in max,
terms to max.

113 17 27 10
173 33 82 18
193 74 271 48
2183 73 5929 33
11229 102 8201 55
15065 67 11723 26
15845 140 23889 44
30817 94 45391 40
48443 158 972463 61
275485 226 1909410 149
1267909 152 1952329 100
2264915 150 2855583 90
5812827 136 7996275 68        
        
The last one took too long, so I didnt wait 
for it. |#
    
;;; 2048

#|

Implement a 2D sliding block puzzle game 
where blocks with numbers are combined to 
add their values. 
    
Task


Rules of the game

  The rules are that on each turn the player 
  must choose a direction   
  (up, down, left or right).
          
  All tiles move as far as possible in that 
  direction, some move more than others.
          
  Two adjacent tiles (in that direction only) 
  with matching numbers combine into one 
  bearing the sum of those numbers.
          
  A move is valid when at least one tile can 
  be moved, including by combination.
  
  A new tile is spawned at the end of each 
  turn at a randomly chosen empty square   
  (if there is one).
  Most of the time, a new 2 is to be added, 
  but occasionally (10% of the time), a 4.
  
  To win, the player must create a tile with 
  the number 2048.
          
  The player loses if no valid moves are 
  possible.


The name comes from the popular open-source 
implementation of this game mechanic, 2048.


Requirements

      "Non-greedy" movement.
      The tiles that were created by 
      combining other tiles should not be 
      combined again during the same turn 
      (move).
      That is to say, that moving the tile 
      row of:

               [2][2][2][2] 

        to the right should result in:

               ......[4][4] 

        and not:

               .........[8] 

      "Move direction priority".
      If more than one variant of combining 
      is possible, move direction shall 
      indicate which combination will take 
      effect.
      For example, moving the tile row of:

               ...[2][2][2] 

        to the right should result in:

               ......[2][4] 

        and not:

               ......[4][2] 


      Check for valid moves. The player 
      shouldn't be able to gain new tile by 
      trying a move that doesn't change the 
      board.
      
      Check for a win condition.
       
      Check for a lose condition |#  
                                    
(defun list-empties (board-tableau)
  (do ((empties nil)
       (row 0 (1+ row)))
      ((> row 3) empties)
    (dotimes (col 4)
      (unless (aref board-tableau row col)
        (push (list row col) empties)))))

(defun add-random (board-tableau)
  (let ((addition
         (if (> (random 100) 89)
             4
             2))
         (choice
          (random-elt
           (list-empties board-tableau))))
    (setf
     (aref board-tableau 
           (car choice) 
           (cadr choice))
     addition))
  board-tableau)
     
      
(defclass 2048-board ()
  ((tableau
    :initarg :tab
    :accessor tab
    :initform
    (add-random
      (add-random
       (make-array '(4 4)
         :initial-element nil)))
    :type array)))

(defmethod print-object ((obj 2048-board) stream)
  (let ((tableau (tab obj)))
    (dotimes (row 4 (values))
      (dotimes (col 4)
        (format stream "~4A "
          (if-let ((entry (aref tableau row col)))
            entry
            #\_)))
      (terpri stream))))

(defun shift-up (board)
  (let ((tableau (tab board))
        (movep nil))
    (dotimes (col 4 (values board movep))
      (unless (aref tableau 0 col)
        (cond
         ((aref tableau 1 col)
          (setf (aref tableau 0 col)
                (aref tableau 1 col)
                (aref tableau 1 col)
                nil
                movep
                t))
         ((aref tableau 2 col)
          (setf (aref tableau 0 col)
                (aref tableau 2 col)
                (aref tableau 2 col)
                nil
                movep
                t))
         ((aref tableau 3 col)
          (setf (aref tableau 0 col)
                (aref tableau 3 col)
                (aref tableau 3 col)
                nil
                movep
                t))))
      (unless (aref tableau 1 col)
        (cond
         ((aref tableau 2 col)
          (setf (aref tableau 1 col)
                (aref tableau 2 col)
                (aref tableau 2 col)
                nil
                movep t))
         ((aref tableau 3 col)
          (setf (aref tableau 1 col)
                (aref tableau 3 col)
                (aref tableau 3 col)
                nil
                movep
                t))))
      (unless (aref tableau 2 col)
        (when (aref tableau 3 col)
          (setf (aref tableau 2 col)
                (aref tableau 3 col)
                (aref tableau 3 col)
                nil
                movep
                t))))))

(defun shift-down (board)
  (let ((tableau (tab board))
        (movep nil))
    (dotimes (col 4 (values board movep))
      (unless (aref tableau 3 col)
        (cond
         ((aref tableau 2 col)
          (setf (aref tableau 3 col)
                (aref tableau 2 col)
                (aref tableau 2 col)
                nil
                movep
                t))
         ((aref tableau 1 col)
          (setf (aref tableau 3 col)
                (aref tableau 1 col)
                (aref tableau 1 col)
                nil
                movep
                t))
         ((aref tableau 0 col)
          (setf (aref tableau 3 col)
                (aref tableau 0 col)
                (aref tableau 0 col)
                nil
                movep
                t))))
      (unless (aref tableau 2 col)
        (cond
         ((aref tableau 1 col)
          (setf (aref tableau 2 col)
                (aref tableau 1 col)
                (aref tableau 1 col)
                nil
                movep
                t))
         ((aref tableau 0 col)
          (setf (aref tableau 2 col)
                (aref tableau 0 col)
                (aref tableau 0 col)
                nil
                movep
                t))))
      (unless (aref tableau 1 col)
        (when (aref tableau 0 col)
          (setf (aref tableau 1 col)
                (aref tableau 0 col)
                (aref tableau 0 col)
                nil
                movep
                t))))))
                
(defun shift-left (board)
  (let ((tableau (tab board))
        (movep nil))
    (dotimes (row 4 (values board movep))
      (unless (aref tableau row 0)
        (cond
         ((aref tableau row 1)
          (setf (aref tableau row 0)
                (aref tableau row 1)
                (aref tableau row 1)
                nil
                movep
                t))
         ((aref tableau row 2)
          (setf (aref tableau row 0)
                (aref tableau row 2)
                (aref tableau row 2)
                nil
                movep
                t))
         ((aref tableau row 3)
          (setf (aref tableau row 0)
                (aref tableau row 3)
                (aref tableau row 3)
                nil
                movep
                t))))
      (unless (aref tableau row 1)
        (cond
         ((aref tableau row 2)
          (setf (aref tableau row 1)
                (aref tableau row 2)
                (aref tableau row 2)
                nil
                movep
                t))
         ((aref tableau row 3)
          (setf (aref tableau row 1)
                (aref tableau row 3)
                (aref tableau row 3)
                nil
                movep
                t))))
      (unless (aref tableau row 2)
        (when (aref tableau row 3)
          (setf (aref tableau row 2)
                (aref tableau row 3)
                (aref tableau row 3)
                nil
                movep
                t))))))
                
(defun shift-right (board)
  (let ((tableau (tab board))
        (movep nil))
    (dotimes (row 4 (values board movep))
      (unless (aref tableau row 3)
        (cond
         ((aref tableau row 2)
          (setf (aref tableau row 3)
                (aref tableau row 2)
                (aref tableau row 2)
                nil
                movep
                t))
         ((aref tableau row 1)
          (setf (aref tableau row 3)
                (aref tableau row 1)
                (aref tableau row 1)
                nil
                movep
                t))
         ((aref tableau row 0)
          (setf (aref tableau row 3)
                (aref tableau row 0)
                (aref tableau row 0)
                nil
                movep
                t))))
      (unless (aref tableau row 2)
        (cond
         ((aref tableau row 1)
          (setf (aref tableau row 2) (aref tableau row 1)
                (aref tableau row 1) nil
                movep t))
         ((aref tableau row 0)
          (setf (aref tableau row 2) (aref tableau row 0)
                (aref tableau row 0) nil
                movep t))))
      (unless (aref tableau row 1)
        (when (aref tableau row 0)
          (setf (aref tableau row 1) (aref tableau row 0)
                (aref tableau row 0) nil
                movep t))))))           
      
(defun combine-up (board)
  (let ((tableau (tab board))
        (combinedp nil))
    (dotimes (col 4 (values board combinedp))
      (when (aref tableau 0 col)
        (cond
         ((eql (aref tableau 0 col)
               (aref tableau 1 col))
          (setf (aref tableau 0 col)
                (* 2 (aref tableau 0 col))
                (aref tableau 1 col)
                (aref tableau 2 col)
                (aref tableau 2 col)
                (aref tableau 3 col)
                (aref tableau 3 col)
                nil
                combinedp t)
          (when (and (aref tableau 1 col)
                     (eql (aref tableau 1 col)
                          (aref tableau 2 col)))
            (setf (aref tableau 1 col)
                  (* 2 (aref tableau 1 col))
                  (aref tableau 2 col)
                  nil)))
         ((and (aref tableau 1 col)
               (eq (aref tableau 1 col)
                   (aref tableau 2 col)))
          (setf (aref tableau 1 col)
                (* 2 (aref tableau 1 col))
                (aref tableau 2 col)
                (aref tableau 3 col)
                (aref tableau 3 col)
                nil
                combinedp
                t))
         ((and (aref tableau 2 col)
               (eq (aref tableau 2 col)
                   (aref tableau 3 col)))
          (setf (aref tableau 2 col)
                (* 2 (aref tableau 2 col))
                (aref tableau 3 col)
                nil
                combinedp
                t)))))))

(defun combine-down (board)
  (let ((tableau (tab board))
        (combinedp nil))
    (dotimes (col 4 (values board combinedp))
      (when (aref tableau 3 col)
        (cond
         ((eql (aref tableau 3 col)
               (aref tableau 2 col))
          (setf (aref tableau 3 col)
                (* 2 (aref tableau 3 col))
                (aref tableau 2 col)
                (aref tableau 1 col)
                (aref tableau 1 col)
                (aref tableau 0 col)
                (aref tableau 0 col)
                nil
                combinedp t)
          (when (and (aref tableau 2 col)
                     (eql (aref tableau 2 col)
                          (aref tableau 1 col)))
            (setf (aref tableau 2 col)
                  (* 2 (aref tableau 2 col))
                  (aref tableau 1 col)
                  nil)))
         ((and (aref tableau 2 col)
               (eq (aref tableau 2 col)
                   (aref tableau 1 col)))
          (setf (aref tableau 2 col)
                (* 2 (aref tableau 2 col))
                (aref tableau 1 col)
                (aref tableau 0 col)
                (aref tableau 0 col)
                nil
                combinedp
                t))
         ((and (aref tableau 1 col)
               (eq (aref tableau 1 col)
                   (aref tableau 0 col)))
          (setf (aref tableau 1 col)
                (* 2 (aref tableau 1 col))
                (aref tableau 0 col)
                nil
                combinedp
                t)))))))

(defun combine-left (board)
  (let ((tableau (tab board))
        (combinedp nil))
    (dotimes (row 4 (values board combinedp))
      (when (aref tableau row 0)
        (cond
         ((eql (aref tableau row 0)
               (aref tableau row 1))
          (setf (aref tableau row 0)
                (* 2 (aref tableau row 0))
                (aref tableau row 1)
                (aref tableau row 2)
                (aref tableau row 2)
                (aref tableau row 3)
                (aref tableau row 3)
                nil
                combinedp t)
          (when (and (aref tableau row 1)
                     (eql (aref tableau row 1)
                          (aref tableau row 2)))
            (setf (aref tableau row 1)
                  (* 2 (aref tableau row 1))
                  (aref tableau row 2)
                  nil)))
         ((and (aref tableau row 1)
               (eq (aref tableau row 1)
                   (aref tableau row 2)))
          (setf (aref tableau row 1)
                (* 2 (aref tableau row 1))
                (aref tableau row 2)
                (aref tableau row 3)
                (aref tableau row 3)
                nil
                combinedp
                t))
         ((and (aref tableau row 2)
               (eq (aref tableau row 2)
                   (aref tableau row 3)))
          (setf (aref tableau row 2)
                (* 2 (aref tableau row 2))
                (aref tableau row 3)
                nil
                combinedp
                t)))))))

(defun combine-right (board)
  (let ((tableau (tab board))
        (combinedp nil))
    (dotimes (row 4 (values board combinedp))
      (when (aref tableau row 3)
        (cond
         ((eql (aref tableau row 3)
               (aref tableau row 2))
          (setf (aref tableau row 3)
                (* 2 (aref tableau row 3))
                (aref tableau row 2)
                (aref tableau row 1)
                (aref tableau row 1)
                (aref tableau row 0)
                (aref tableau row 0)
                nil
                combinedp t)
          (when (and (aref tableau row 2)
                     (eql (aref tableau row 2)
                          (aref tableau row 1)))
            (setf (aref tableau row 2)
                  (* 2 (aref tableau row 2))
                  (aref tableau row 1)
                  nil)))
         ((and (aref tableau row 2)
               (eq (aref tableau row 2)
                   (aref tableau row 1)))
          (setf (aref tableau row 2)
                (* 2 (aref tableau row 2))
                (aref tableau row 1)
                (aref tableau row 0)
                (aref tableau row 0)
                nil
                combinedp
                t))
         ((and (aref tableau row 1)
               (eq (aref tableau row 1)
                   (aref tableau row 0)))
          (setf (aref tableau row 1)
                (* 2 (aref tableau row 1))
                (aref tableau row 0)
                nil
                combinedp
                t)))))))
                
(defun move (direction board)
  (let* ((functions
           (ecase direction
             ((#\L #\l) (list #'shift-left #'combine-left))
             ((#\R #\r) (list #'shift-right #'combine-right))
             ((#\U #\u) (list #'shift-up #'combine-up))
             ((#\D #\d) (list #'shift-down #'combine-down))))
         (mover (car functions))
         (combiner (cadr functions)))
    (multiple-value-bind
        (moved movep)
        (funcall mover board)
      (when movep
            (setf board moved))
      (multiple-value-bind
          (combined combinedp)
          (funcall combiner board)
        (when combinedp
              (setf board combined))
        (values board (or movep combinedp))))))
    

(defun losep (board)
  (let ((tableau (tab board)))
    (dotimes (row 4)
      (dotimes (col 4)
        (unless (aref tableau row col)
          (return-from losep nil))))
    (dotimes (row 4)
      (if (or (= (aref tableau row 0)
                 (aref tableau row 1))
              (= (aref tableau row 1)
                 (aref tableau row 2))
              (= (aref tableau row 2)
                 (aref tableau row 3)))
          (return-from losep nil)))
    (dotimes (col 4 t)
      (if (or (= (aref tableau 0 col)
                 (aref tableau 1 col))
              (= (aref tableau 1 col)
                 (aref tableau 2 col))
              (= (aref tableau 2 col)
                 (aref tableau 3 col)))
          (return-from losep nil)))))

(defun winp (board)
  (let ((tableau (tab board)))
    (dotimes (row 4 nil)
      (dotimes (col 4)
        (when (eql (aref tableau row col)
                 2048)
          (return-from winp t))))))

(defun play-2048 ()
  (let ((board (make-instance '2048-board))
        (move nil))
    (format t"~%~45:@<2048~>~2%")
    (loop
      (format t"~A~%" board)
      (block choice
        (loop
          (format t "(L)eft, (R)ight, (U)p, (D)own, (Q)uit")
          (setq move (char (read-line) 0))
          (cond
           ((find move "LRUDQlrudq")
            (return-from choice))
           (t
            (format t "~%~A" board)))))
      (case move
        ((#\Q #\q)
         (format t "~%~46:@<BYE!~>")
         (return-from play-2048 (values)))
        ((#\L #\R #\U #\D
          #\l #\r #\u #\d)
         (multiple-value-bind
             (altered changedp)
             (move move board)
           (block win-eval
             (when changedp
               (when (winp board)
                 (format t"~2%YOU WON!~2%")
                 (let ((againp
                        (y-or-n-p "Play again?")))
                   (cond
                    (againp
                     (setf board
                       (make-instance '2048-board))
                     (return-from win-eval))
                    (t
                     (format t "~%~46:@<BYE!~>")
                     (return-from play-2048 (values))))))))
               (setf (tab board)
                     (add-random
                      (tab altered)))
               (block lose-eval
                 (when (losep board)
                   (format t"~2%GAME OVER~2%")
                   (cond
                     ((y-or-n-p "Play again?")
                      (setf board 
                            (make-instance '2048-board))
                      (return-from lose-eval))
                     (t
                      (format t "~%~46:@<BYE!~>")
                      (return-from play-2048 (values))))))
           (terpri)))))))

;; My only qualm is I can't get the columns
;; right.

;;; Call a Function

#|
             
Demonstrate the different syntax and 
semantics provided for calling a function.
          
Task


This may include:

  Calling a function that requires no 
     arguments
  Calling a function with a fixed number of 
    arguments
  Calling a function with optional arguments
  Calling a function with a variable number 
     of arguments
  Calling a function with named arguments
  Using a function in statement context
  Using a function in first-class context 
     within an expression
  Obtaining the return value of a function
  Distinguishing built-in functions and 
     user-defined functions
  Distinguishing subroutines and functions
  Stating whether arguments are passed by 
     value or by reference
  Is partial application possible and how


This task is not about defining functions.
     
|#

#|
In lisps, a group of items surrounded by
parentheses is a list. Bare lists are 
interpreted as an invocation in which 
the first element is the operator
(function, macro or special operator), 
and the other elements positional arguments.
If we wish a non-empty list to just be a
container, we can
1. Call the LIST function on the elements
   of the list
   (list 1 2 3) -> (1 2 3)
2. Call the QUOTE function on the list
   (quote (1 2 3)) -> (1 2 3)
3. Use a single quote ' as syntactic sugar
   for QUOTE:
   '(1 2 3) -> (1 2 3)
               
When we are speaking of positional 
arguments, the mechanics are the same,
regardless if the arguments are 
required, optional or of variable length.
          
+ takes a variable number of args
  
(+) -> 0
(+ 5) -> 5
(+ 5 6) -> 11
(+ 5 6 7) -> 18
              
The difference is found in the function
or macro signature, called a lambda list.
   
(defun myfun1 ()) accepts no args
                                
(defun myfun2 (a)) one required arg
                                  
(defun myfun3 (a b)) two required positional
       args.
       
(defun myfun4 (a &optional b))
    two positional args, the first required
    
(defun myfun5 (a &rest bs))
    one required arg followed by 
    0 or more optional positional
    args.
    
In CL, we usually use a special symbol
called a keyword to indicate named
args. Keywords always begin with a colon:
      
(myfun6 5 :lemons 3 :sauce 12)

This invocation begins with a positional
arg followed by two named args, called
keyword args. :lemons has the value 3
and :sauce has 12.
    
When a lambda list contains named arguments,
called keyword arguments, supplying the 
arguments during invocation is always 
optional. In CL, all required args are
positional.
    
Back to myfun6. Our lambda list:
   
(myfun6 (a &key lemons sauce))

Note that the lambda list uses plain
symbols eather than keywords. 
        
There is also
&allow-other-keys
Which allows for arbitrary, unspecified
keyword arguments.
        
We can pass functions as first class arguments.
Common Lisp stores symbols referencing
functions and symbols referencing variables
in separate namespaces. Thus, we can 
have a single symbol that references both
a function and a value.
  
Generally, when a symbol appears at the 
beginning of a list, it is interpreted
as an operator. Otherwise, it is interpreted
as a value.
   
To get at the function when the symbol
does not appear at the beginning of a 
list, use FUNCTION or its sugar #'
      
Example

(function myfunc6)

or

#'myfunc6.

To invoke a function passed as an arg, we use
FUNCALL

(funcall function-passed-as-arg)

To obtain the value of a function, 
simply call it. All functions return a 
value, which may be NIL.
       
Generally the value of the last statement
in the function definition is the one 
returned. We need no "return x". Simply
"x" will do the job.
    
Lisp recognizes no difference between 
native and user-created functions. There are
a handful of special operators that form the
backbone of the language, and users cannot
create new ones. Otherwise, users may 
create arbitrary functions and macros
which are not distinguished from the native
 functions and macros.
 
There is no such thing as a subroutine in
CL. However, anonymous functions can be 
written with LAMBDA and lexically 
scoped local functions, macros, 
and symbol macros can be defined with
FLET, LABELS, MACROLET and 
SYMBOL-MACROLET
 
CL passes most args (except for some 
types such as numbers) by reference, but
this fact is hidden from the user.
     
Partial application is quite possible, 
but there is no built-in operator for
this task. We can use LAMBDA, import
a function like alexandria:curry or 
serapeum:partial. Or we can just define 
our own:
    
(defun partial (func &rest args)
  (lambda (&rest new-args)
    (apply func (append args new-args))))

(defun add-five ()
  (lambda (&rest args)
    (partial #'+ 5)))

(funcall #'add-five 6)

-> 11
   
|#

;;; Knapsack Problem 0/1

#|

A tourist wants to make a good trip at the 
weekend with his friends. 
        
They will go to the mountains to see the 
wonders of nature, so he needs to pack well 
for the trip.

He has a good knapsack for carrying things, 
but knows that he can carry a maximum of 
only 4kg in it, and it will have to last the 
whole day.

He creates a list of what he wants to bring 
for the trip but the total weight of all 
items is too much.

He then decides to add columns to his 
initial list detailing their weights and a 
numerical value representing how important 
the item is for the trip.


Here is the list: 
     
Table of potential knapsack items
item 	     weight (dag) value
map 	     9 	         150
compass 	     13 	         35
water 	     153 	200
sandwich      50 	         160
glucose 	     15 	         60
tin 	     68 	         45
banana 	     27 	         60
apple 	     39 	         40
cheese 	     23 	         30
beer 	     52 	         10
suntan cream  11 	         70
camera 	     32 	         30
T-shirt 	     24 	         15
trousers      48 	         10
umbrella      73 	         40
waterproof tr 42 	         70
waterproof oc 43 	         75
note-case     22 	         80
sunglasses    7 	         20
towel 	     18 	         12
socks 	     4 	         50
book 	     30 	         10
     
knapsack      ≤400 dag 	?   
              
The tourist can choose to take any 
combination of items from the list, but 
only one of each item is available.

He may not cut or diminish the items, so he 
can only take whole units of any item.


Task

Show which items the tourist can carry in 
his knapsack so that their total weight does 
not exceed 400 dag [4 kg], and their total 
value is maximized.

[dag = decagram = 10 grams]    
     
|#

(defparameter *knapsack-01-data*
  '((:map 9 150)
    (:compass 13 35)
    (:water 153 200)
    (:sandwich 50 160)
    (:glucose 15 60)
    (:tin 68 45)
    (:banana 27 60)
    (:apple 39 40)
    (:cheese 23 30)
    (:beer 52 10)
    (:suntan-cream 11 70)
    (:camera 32 30)
    (:t-shirt 24 15)
    (:trousers 48 10)
    (:umbrella 73 40)
    (:waterproof-trousers 42 70)
    (:waterproof-overclothes 43 75)
    (:note-case 22 80)
    (:sunglasses 7 20)
    (:towel 18 12)
    (:socks 4 50)
    (:book 30 10)))

(defun get-arrays (&optional
                   (data-alist
                    *knapsack-01-data*))
  (let* ((len (length data-alist))
         (items (make-array (1+ len)
                  :element-type 'symbol
                  :initial-element :null))
         (v (make-array (1+ len)
                  :element-type 'fixnum
                  :initial-element -1))
         (w (make-array (1+ len)
                  :element-type 'fixnum
                  :initial-element -1)))
    (iter
     (for entry in data-alist)
     (for i from 1)
     (setf (aref items i)
           (car entry)
           (aref w i)
           (cadr entry)
           (aref v i)
           (caddr entry)))
    (values items w v)))
 
(defun find-value-maxes (values weights
                          &optional
                          (max-weight 400))
  (let ((value-array
         (make-array
          (list (length values)
                (1+ max-weight))
          :element-type 'fixnum
          :initial-element -1)))
    (labels ((local-max (i j)
               (cond
                ((or (zerop i)
                     (zerop j))
                 (setf
                  (aref value-array i j)
                  0))
                ((> (aref weights i) j)
                 (let ((prev
                        (aref 
                          value-array
                          (1- i)
                          j)))
                   (setf
                    (aref value-array i j)
                    (if (> prev -1)
                        prev
                        (local-max
                         (1- i) j)))))
                (t
                 (let ((prev
                        (aref
                         value-array
                         (1- i)
                         j))
                       (minus-weight
                        (aref
                         value-array
                         (1- i)
                         (- j
                            (aref weights i)))))
                   (setf
                    (aref value-array i j)
                    (max
                     (if (> prev -1)
                         prev
                         (local-max
                          (1- i)
                          j))
                     (+
                      (aref values i)
                      (if (> minus-weight -1)
                          minus-weight
                          (local-max
                           (1- i)
                           (- j
                              (aref weights i))))))))))))
            (local-max
             (1- (length values))
             max-weight)
            value-array)))

(defun get-items (value-array items weights
                   &optional
                   (max-weight 400))
  (labels ((rec (i j)
             (cond
              ((or
                 (zerop i)
                 (<= j 0))
               nil)
              ((> (aref value-array i j)
                  (aref value-array (1- i) j))
               (cons 
                 i 
                 (rec (1- i)
                      (- j 
                         (aref weights i)))))
              (t
               (rec (1- i) j)))))
    (let ((indices (rec (1- (length items))
                        max-weight)))
      (mapcar
        #'(lambda (index)
            (aref items index))
          indices))))

(defun knapsack-0/1-main ()
  (multiple-value-bind
      (items weights values)
      (get-arrays)
    (let ((value-maxes
           (find-value-maxes
            values
            weights)))
      (get-items value-maxes items weights))))
  
#| Very fast!
   
(:SOCKS :SUNGLASSES :NOTE-CASE 
 :WATERPROOF-OVERCLOTHES 
 :WATERPROOF-TROUSERS
 :SUNTAN-CREAM :BANANA :GLUCOSE :SANDWICH 
 :WATER :COMPASS :MAP)
 
It looks like a reasonable list. |#
                                  
   
            
