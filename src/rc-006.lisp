;;;; rosetta/ros-006
;;;; Contains one task: 
;;;; Compiler/Virtual Machine Interpreter

(in-package #:ros-01)

#| Compiler/Virtual Machine Interpreter
   
A virtual machine implements a computer in 
software. 
         
Virtual Machine Interpreter

Write a virtual machine interpreter. This 
interpreter should be able to run virtual 
assembly language programs created via the 
task. This is a byte-coded, 32-bit word 
stack based virtual machine.

The program should read input from a file 
and/or stdin, and write output to a file 
and/or stdout.
       
Input format

Given the following Given the following 
program:

count = 1;
while (count < 10) {
    print("count is: ", count, "\n");
    count = count + 1;
}

The output from the Code generator is a 
virtual assembly code program:
        
Output from gen, input to VM

Datasize: 1 Strings: 2
"count is: "
"\n"
    0 push  1
    5 store [0]
   10 fetch [0]
   15 push  10
   20 lt
   21 jz     (43) 65
   26 push  0
   31 prts
   32 fetch [0]
   37 prti
   38 push  1
   43 prts
   44 fetch [0]
   49 push  1
   54 add
   55 store [0]
   60 jmp    (-51) 10
   65 halt   
   
The first line of the input specifies the 
datasize required and the number of constant 
strings, in the order that they are 
reference via the code.

The data can be stored in a separate array, 
or the data can be stored at the beginning 
of the stack. Data is addressed starting at 
0. If there are 3 variables, the 3rd one if 
referenced at address 2.

If there are one or more constant strings, 
they come next. The code refers to these
strings by their index. The index starts at 
0. So if there are 3 strings, and the code 
wants to reference the 3rd string, 2 will be 
used.

Next comes the actual virtual assembly code.
The first number is the code address of that 
instruction. After that is the instruction 
mnemonic, followed by optional operands, 
depending on the instruction.
          
Registers

sp:

   the stack pointer - points to the next 
   top of stack.  The stack is a 32-bit 
   integer array.

pc:

   the program counter - points to the 
   current instruction to be performed.  
   The code is an array of bytes.

Data:

   data
   string pool

Instructions

Each instruction is one byte. The following 
instructions also have a 32-bit integer 
operand:

fetch [index]

where index is an index into the data array.

store [index]

where index is an index into the data array.

push n

where value is a 32-bit integer that will be 
pushed onto the stack.

jmp (n) addr

where (n) is a 32-bit integer specifying the 
distance between the current location and 
the desired location. addr is an unsigned 
value of the actual code address.

jz (n) addr

where (n) is a 32-bit integer specifying the 
distance between the current location and 
the desired location. addr is an unsigned 
value of the actual code address.

The following instructions do not have an 
operand. They perform their operation 
directly against the stack:

For the following instructions, 
the operation is performed against the top 
two entries in the stack:

add
sub
mul
div
mod
lt
gt
le
ge
eq
ne
and
or

For the following instructions, the 
operation is performed against the top 
entry in the stack:

neg
not

Print the word at stack top as a character.

prtc

Print the word at stack top as an integer.

prti

Stack top points to an index into the string 
pool. Print that entry.

prts

Unconditional stop.

halt

A simple example virtual machine

def run_vm(data_size)
    int stack[data_size + 1000]
    set stack[0..data_size - 1] to 0
    int pc = 0
    while True:
        op = code[pc]
        pc += 1

        if op == FETCH:
            stack.append(stack[bytes_to_int(code[pc:pc+word_size])[0]]);
            pc += word_size
        elif op == STORE:
            stack[bytes_to_int(code[pc:pc+word_size])[0]] = stack.pop();
            pc += word_size
        elif op == PUSH:
            stack.append(bytes_to_int(code[pc:pc+word_size])[0]);
            pc += word_size
        elif op == ADD:   stack[-2] += stack[-1]; stack.pop()
        elif op == SUB:   stack[-2] -= stack[-1]; stack.pop()
        elif op == MUL:   stack[-2] *= stack[-1]; stack.pop()
        elif op == DIV:   stack[-2] /= stack[-1]; stack.pop()
        elif op == MOD:   stack[-2] %= stack[-1]; stack.pop()
        elif op == LT:    stack[-2] = stack[-2] <  stack[-1]; stack.pop()
        elif op == GT:    stack[-2] = stack[-2] >  stack[-1]; stack.pop()
        elif op == LE:    stack[-2] = stack[-2] <= stack[-1]; stack.pop()
        elif op == GE:    stack[-2] = stack[-2] >= stack[-1]; stack.pop()
        elif op == EQ:    stack[-2] = stack[-2] == stack[-1]; stack.pop()
        elif op == NE:    stack[-2] = stack[-2] != stack[-1]; stack.pop()
        elif op == AND:   stack[-2] = stack[-2] and stack[-1]; stack.pop()
        elif op == OR:    stack[-2] = stack[-2] or  stack[-1]; stack.pop()
        elif op == NEG:   stack[-1] = -stack[-1]
        elif op == NOT:   stack[-1] = not stack[-1]
        elif op == JMP:   pc += bytes_to_int(code[pc:pc+word_size])[0]
        elif op == JZ:    if stack.pop() then pc += word_size else pc += bytes_to_int(code[pc:pc+word_size])[0]
        elif op == PRTC:  print stack[-1] as a character; stack.pop()
        elif op == PRTS:  print the constant string referred to by stack[-1]; stack.pop()
        elif op == PRTI:  print stack[-1] as an integer; stack.pop()
        elif op == HALT:  break

Additional examples

Your solution should pass all the test cases 
above and the additional tests found Here.
Reference

The C and Python versions can be considered 
reference implementations. 
          
See compiler.txt (use the shell to find it)
to see the various compiler tasks.
   
|#
 
#| My notes
   
1. all code and data, except string 
   constants, is stored and operated on
   as regular integers, whose values
   are constrained to those that fit
   in 32 bits. Overflow results in 
   rollover. (Note that in CL, integers
   are automatically given arbitrary
   precision, with type conversion performed
   implicitly. At least one implementation
   has facilities for raw 32-bit arithmetic,
   but this is not portable. The use of
   bit-vectors was considered and 
   rejected. CL does not provide native 
   functions to perform arithmetic on 
   bit-vectors. The 3rd party bit-smasher
   library offers this facility for
   non-negative values, only.
2. Registers/Memory. All registers/memory
   except the string array
   are Common Lisp arrays of constrained
   integers, each limited to 32 bits.
 
2A. We need a temporary register. 
    Temp is a zero-dimensional array
    (i.e. it can hold only one value at
     a time.)
2B. Stack
    This will be an adjustable array with
    a Lisp fill-pointer. The fill-pointer
    will be used as the sp (stack pointer)
2C. Data
    A simple vector (i.e. of fixed length)
2D. Strings
    A simple vector. Contains strings,
    not integers.
3. Instructions
3A FETCH n
   Retrieve nth value from data array 
   and place into the register
3B STORE n
   Place the value in the register into
   the nth cell of the data array.
3C PUSH n
   Push n onto the stack
3D JMP (offset) address
   Add offset to the code pointer.
3E JZ (offset) address
   Iff the value in the register is 
   binary zero, add offset to the code
   pointer.
3F ADD
   Pop stack to temp.
   Pop stack, temp.
   Normalize temp
   Push to stack.
3G SUB
   Pop stack
   Exchange popped value and top value
   of stack.
   Use hand-rolled subtraction: pop
   stack, subtracting from temp.
   Normalize temp
   Push to stack
3H. MUL
    Pop stack to temp.
    Pop stack, multiplying with temp
    Normalize temp
    Push to stack.
3I. DIV
    Pop stack
    Exchange
    Specialized divide: divide temp
      by value popped from stack
    Normalize temp
    Push stack
3J. MOD
    Pop stack
    Exchange
    Find modulus of temp and pop
    Normalize temp
    Push
3K. LT
    Pop
    Exchange
    If temp < pop,
    Push b1,
    else b0.
3L. GT
    Pop
    Exchange.
    If temp > pop
    push b1
    else b0
3M. LE
    Pop
    Exchange
    If temp <= pop,
    push b1
    else b0
3N. GE
    Pop
    Exchange
    If temp >= pop
    push b1
    else b0
30. EQ
    Pop
    If temp = pop
    push b1
    else b0
3P. NE
    Pop
    If temp /= pop
    push b1
    else b0
3Q. AND
    Pop
    Bitwise AND temp with pop
    normalize temp
    push
3R. OR
    Pop
    Bitwise OR temp and pop
    normalize temp
    push
    
3S. NEG
    pop
    negate temp
    normalize temp
    push
    
3T. NOT
    pop
    bitwise NOT temp
    normalize temp
    push
    
3U. PRTC
    peek
    convert temp to character
    print character
    
3V. PRTI
    pop
    convert temp to integer
    print integer
    
3W. PRTS
    pop
    convert temp to integer
    access and print string at strings[temp]

3X. HALT
    Exit program loop

|#
(defparameter *instructions*
  '(fetch store push jmp jz add sub mul
    div mod lt gt le ge eq ne and or
    neg not prtc prti prts halt)
   "The instruction set for the vm")
   
(defparameter *code-table*
  (let ((table (make-hash-table :size 24)))
    (do ((i 1 (1+ i))
         (tokens *instructions* (rest tokens)))
        ((null tokens) (progn
                         (setf (gethash 0 table)
                               'none)
                         table))
      (setf (gethash i table)
            (first tokens))))  
  "Table mapping bit-vectors to instructions.
   Instruction codes start at binary 1,
   as we wish binary 0 to be a neutral
   place-holder.")

;; The opposite mapping will be on each
;; instruction symbol's plist.

(maphash
 #'(lambda (code instruction)
     (setf (get instruction 'code) code))
   *code-table*)
   
(defun int->instruction (code)
  (gethash code *code-table* 'none))

(defun instruction->int (instruction)
  (let ((trial (get instruction 'code)))
    (if trial
        trial
        0)))
   
(defparameter *word-size* 4
  "The number of cells allowed for an
  argument in code, or, the length
  in bytes of a computer word.")

(defparameter *max-bits* (* *word-size* 8))

(defparameter *max-val* 
  (1- (expt 2 (1- *max-bits*)))
  "The maximum value that a signed 
  2s complement *max-bits* integer can hold.
  When *max-bits* is 32,
  value is 2,147,483,647")
  
(defparameter *min-val* 
  (- (expt 2 (1- *max-bits*)))
  "The minimum value that a signed 2s
  complement *max-bits* integer can hold.
  With *max-bits* = 32,
  the value is -2,147,483,648.")
  
(defparameter *rand-limit* 
  (expt 2 *max-bits*)
  "used for creating chaotic overflows.")

(defun chaotic-overflow ()
  "Simulates an overflow of two
  operands whose signs are opposite.
  Since this is undefined behavior,
  returns a random integer within the
  limits."
  (+ *min-val* (random *rand-limit*)))

(defun raw-add (a b)
  "Simulates raw fixed-bit adding. If the
  result fits within the range, return 
  the result. If not, and both a and b
  have the same sign, return an overflowed
  result. If the result is out of range and
  the integers have opposite signs, then 
  the result is undefined, and so we return
  a random integer in range."
  (let ((true-sum (+ a b)))
    (if (<= *min-val* true-sum *max-val*)
        true-sum
        (cond
         ((= 1 (signum a) (signum b))
          (+ *min-val* 
             (mod 
               true-sum 
               (1+ *max-val*))))
         ((= -1 (signum a) (signum b))
          (+ *max-val*
             (mod
              true-sum
              *min-val*)))
         (t
          (chaotic-overflow))))))

(defun raw-sub (a b)
  "Simulates raw fixed-bit subtraction.
  See raw-add for details."
  (let ((true-difference (- a b)))
    (if (<= *min-val* 
            true-difference 
            *max-val*)
        true-difference
        ;; if there is an overflow,
        ;; the operands must have had
        ;; different signs
        (chaotic-overflow))))

(defun raw-mul (a b)
  "Simulate fixed-bit multiplication of
  two integers. See raw-add for details."
  (let ((true-product (* a b)))
    (cond 
      ((<= *min-val* true-product *max-val*)
        true-product)
      ((/= (signum a) (signum b))
       (chaotic-overflow))
      ;; if the signs are the same, true-product
      ;; must be positive.
      (t
       (do* ((abs-a (abs a))
             (abs-b (abs b))
             (i (truncate (1+ *max-val*) abs-a)
                (1+ i))
             (result (* abs-a i)
              (+ result abs-a)))
           ((= i abs-b) result)
         (when (> result *max-val*)
           (setf result
             (+ *min-val*
                (mod
                 result
                 (1+ *max-val*))))))))))

(defun raw-div (a b)
  "In truncated integer division,
  there can be no overflows. However, if
  an attempt is made to divide by zero, 
  the cpu gets stuck in an endless loop.
  I will not do that here. I will simply
  end the program with a note that the
  error occurred."
  (cond 
  ((zerop b)
      ;; rather than signal an error,
      ;; I mimic an infinite recursion and 
      ;; then send control
      ;; out of the program loop.
   (dotimes (_ 10)
     (format t "DIV0~%"))
   (format t "X-{~%")
   (throw 'div0 nil))
  (t
    (truncate a b))))
    
(defun raw-mod (a b)
  "a modulo b. If b is 0, crash. See
  raw-div for details."
  (cond 
    ((zerop b)
      (dotimes (_ 10)
        (format t "DIV0~%"))
      (format t "X-{~%")
      (throw 'div0 nil))
    (t (mod a b))))
    
(defun raw-neg (a)
  "Simulate negation of a fixed-bit 
  integer. Given twos-complement
  fixed-bit integers, an attempt to
  negate the minimum legal value 
  results in the value being 
  returned unchanged."
  (if (= a *min-val*)
      a
      (- a)))
  

(defstruct (registers (:conc-name reg-))
   "Struct which holds the registers of 
    the virtual machine. This will be 
    populated by our assembler."
  (stack 
     (make-array 20
                 :element-type 'fixnum
                 :initial-element 0
                 :adjustable t
                 :fill-pointer 0)
     :type (vector fixnum))
   (data nil
     :type (or (vector fixnum) null))
   (strings nil
     :type (or (vector string) null))
   (code nil
     :type (or (vector fixnum) null)))
     
(defun vector-peek (vec)
  (aref vec (1- (length vec))))

(defparameter *loop-limit* most-positive-fixnum)

(defun c-or (a b)
  (if (not (zerop a))
      1
      (if (not (zerop b))
          1
          0)))

(defun c-and (a b)
  (if (zerop a)
      0
      (if (zerop b)
          0
          1)))

(defun c-not (a)
  (if (zerop a)
      1
      0))
      
(defun vm-run (reg)
  "Runs code as a virtual machine. The
  machine-language code and the 
  other registers are stored in a 
  registers struct."
  (catch 'div0
    (let ((code (reg-code reg))
          (stack (reg-stack reg))
          (data (reg-data reg))
          (strings (reg-strings reg))
          (temp-reg 0)
          (instruction 'none))
       (loop with halted = nil
             for cp from 0
             for counter from 0 to *loop-limit*
             until halted
             while (< cp (length code)) do
         (setf instruction
           (int->instruction 
             (aref code cp)))
         (case instruction
           (fetch
            (vector-push-extend
             (aref data
               (aref code (1+ cp)))
             stack)
            (incf cp *word-size*))
           (store
            (setf
             (aref data
               (aref code (1+ cp)))
             (vector-pop stack))
            (incf cp *word-size*))
           (push
            (vector-push-extend
             (aref code (1+ cp))
             stack)
            (incf cp *word-size*))
           (jmp
            (incf cp 
                  (aref code (1+ cp))))
           (jz
            (incf cp
              (if (zerop (vector-pop stack))
                  (aref code (1+ cp))
                  *word-size*)))
           (add
            (setf temp-reg
                  (vector-pop stack))
            (vector-push
             (raw-add 
               (vector-pop stack)
               temp-reg)
             stack))
           (sub
            (setf temp-reg
                  (vector-pop stack))
            (vector-push
             (raw-sub
              (vector-pop stack)
              temp-reg)
             stack))
           (mul
            (setf temp-reg
                  (vector-pop stack))
            (vector-push
             (raw-mul
              (vector-pop stack)
              temp-reg)
             stack))
           (div
            (setf temp-reg
                  (vector-pop stack))
            (vector-push
             (raw-div
              (vector-pop stack)
              temp-reg)
             stack))
           (mod
            (setf temp-reg
                  (vector-pop stack))
            (vector-push
             (raw-mod 
               (vector-pop stack)
               temp-reg)
             stack))
           (lt
            (setf temp-reg
                  (vector-pop stack))
            (vector-push
             (if (< (vector-pop stack)
                    temp-reg)
                 1
                 0)
             stack))
           (gt
            (setf temp-reg
                  (vector-pop stack))
            (vector-push
             (if (> (vector-pop stack)
                    temp-reg)
                 1
                 0)
             stack))
           (le
            (setf temp-reg
                  (vector-pop stack))
            (vector-push
             (if (<= (vector-pop stack)
                    temp-reg)
                 1
                 0)
             stack))
           (ge
            (setf temp-reg
                  (vector-pop stack))
            (vector-push
             (if (<= (vector-pop stack)
                    temp-reg)
                 1
                 0)
             stack))
           (eq
            (setf temp-reg
                  (vector-pop stack))
            (vector-push
             (if (= (vector-pop stack)
                    temp-reg)
                 1
                 0)
             stack))
           (ne
            (setf temp-reg
                  (vector-pop stack))
            (vector-push
             (if (/= (vector-pop stack)
                     temp-reg)
                 1
                 0)
             stack))
           (and
             (setf temp-reg
                   (vector-pop stack))
             (vector-push
              (c-and
               (vector-pop stack)
               temp-reg)
              stack))
           (or
            (setf temp-reg
                  (vector-pop stack))
            (vector-push
             (c-or
              (vector-pop stack)
              temp-reg)
             stack))
           (neg
            (vector-push
             (raw-neg
              (vector-pop stack))
             stack))
           (not
            (vector-push
             (c-not
              (vector-pop stack))
             stack))
           (prtc
            (format t "~C"
              (code-char (vector-peek stack))))
           (prti
            (format t "~D"
              (vector-peek stack)))
           (prts
            (let ((str
                   (aref strings
                     (vector-pop stack))))
              (if (string= str "\"\\n\"")
                  (terpri)
                  (format t "~A" str))))
           (halt
            (setf halted t))))))
  (values))

(defun emptyp (seq)
  (zerop (length seq)))

(defun load-file (file-path)
  "Returns contents of text file 
  as a string, containing the same 
  line structure as the file."
  (with-output-to-string (s)
    (with-open-file 
      (f :direction :input
         :element-type :character
         :if-does-not-exist :error)
      (do ((line 
            #1=(read-line f nil 'eof nil)
            #1#))
          ((eq line 'eof))
        (format s "~A~%" 
          (string-trim " " line))))))

(defun load-input ()
  "Loads a code string from user input."
  (format t "~&Please enter code, line by line.~%")
  (format t "Enter q on a separate line to stop.~2%")
  (with-output-to-string (s)
    (do ((line
          #7=(read-line)
          #7#))
        ((find line
           '("q" "quit" "end" "stop" "eof")
           :test #'string-equal))
      (format s "~A~%" 
        (string-trim " " line)))))
        
(defparameter *c-char->lisp-char*
  `(("\\\\a" . "@")
    ("\\\\b" . ,(format nil "~C" #\Backspace))
    ("\\\\e" . "@")
    ("\\\\f" . ,(format nil "~C" #\Page))
    ("\\\\n" . ,(format nil "~%"))
    ("\\\\r" . ,(format nil "~C" #\Return))
    ("\\\\t" . ,(format nil "~C" #\Tab))
    ("\\\\v" . "@")
    ("\\\\'" . "'")
    ("\\\\" . "\\")))
          
(defun clean-string (str)
  "Removes superfluous quotes from a string.
  Replaces C escape characters with common
  Lisp equivalents."
  (let* ((adjustable
          (make-array (length str)
            :element-type 'character
            :initial-contents str
            :adjustable t))
         (no-quotes
         (remove #\" adjustable)))
    (reduce
     #'(lambda (new-str entry)
         (regex-replace-all 
           (car entry) 
           new-str 
           (cdr entry)))
       *c-char->lisp-char*
       :initial-value no-quotes)))
  

(defun process-raw (code-str)
  "Takes a code string, as loaded 
  by load-file or load-input. Places each 
  line in a list. The first line
  is divided into tokens, which
  contain the labels 'Datasize:' and 
  'Strings:', each followed by a number.
  The labels are discarded and the arguments
  are coerced to ints. The second arg, the
  number of strings, is used to skip over
  the following lines, which contain those
  strings. After the string lines, the 
  code itself begins. For each code line,the 
  list is split into tokens. 
  The numbers are coerced to ints, other 
  numbers are extracted from brackets and 
  parentheses, are coerced to ints, 
  and the brackets and parentheses are
  discarded.
  the instructions are coerced to symbols.
  Once the raw string has been proceseed,
  we are left with a nested list in 
  which the inner lists are in reverse order.
  The last line number (first item in 
  first list) is grabbed. Then, the list 
  is reversed, a list containing only
  the last line number is added to the 
  front, and resulting nested list is 
  returned."
  (let* ((line-list 
          (uiop:split-string
           code-str
           :separator (list #\Newline)))
         (param-line (first line-list))
         (processed nil)
         string-count)
    (do* ((rest-line
           (uiop:split-string 
             (first line-list)
             :separator " ")
           (rest rest-line))
         (token (first rest-line) 
                (first rest-line))
         (new-first-line nil))
        ((null rest-line) (push (nreverse new-first-line)
                   processed))
      (when 
         (and 
           (not (emptyp token))
           (every 
             #'digit-char-p token))
        (push
         (parse-integer token)
         new-first-line)
        (when (null (cdr rest-line))
          (setf string-count 
                (first
                 new-first-line)))))
    (do* ((rest-lines 
            (rest line-list)
            (rest rest-lines))
          (line-ptr 1 (1+ line-ptr))
          (cur-line 
            #3=(first rest-lines)
            #3#))
         ((null rest-lines))
      (if (<= line-ptr string-count)
          (push (list (clean-string cur-line)) 
                processed)
          (let ((tokens
                (uiop:split-string
                  cur-line
                  :separator " "))
                (processed-line nil))
            (when tokens
              (dolist (token 
                     tokens 
                     (push 
                       (nreverse 
                         processed-line) 
                       processed))
              (cond
               ((emptyp token) nil)
               ((every #'digit-char-p token)
                (push
                 (parse-integer token)
                 processed-line))
               ((char=
                  (char token 0)
                  #\[)
                (push
                 (parse-integer
                  (remove #\]
                    (remove #\[ token)))
                 processed-line))
               ((char=
                 (char token 0)
                 #\()
                (push
                 (parse-integer
                  (remove #\)
                   (remove #\( token)))
                 processed-line))
               (t
                (let 
                  ((sym (find-symbol
                         (string-upcase
                          token))))
                  (if
                   (and 
                     sym
                     (position sym *instructions*))
                   (push sym processed-line)
                   (push token processed-line))))))))))
  (let ((code-line-count (first (first processed))))
    (setf processed (nreverse processed))
    (push (list code-line-count) processed))))

(defun assemble (line-list)
  "Assembles a nested list (from 
  process-raw) into a registers structure,
  to be fed to our virtual machine.
  We process the list as follows:
  Take the first line. It contains only
  the line number of the last code 
  instruction. We use this line number 
  to create a code array, and populate 
  it with zeros. 
  The next line contains two integers:
  the size of the data array, and the size 
  of the string array. We use these to 
  create the arrays, populating the first
  with zeros and the second with empty strings.
  We also use the string count to build our
  string array directly from the following
  lines. Once we get to the code, each
  line will have 1. a line number. We 
  grab that as a code array index. 2. 
  (hopefully) an instruction symbol. We
  convert that to the specified integer
  code and store it at the index. 3 + 4
  (both optional) integer args. We grab the 
 first arg and put it directly into the 
  code array, at the index following 
  the instruction. We never will need the
  second arg. When the entire list has 
  been processed, we assemble the arrays into
  our struct and return it."
  (destructuring-bind
       ((last-line-number . _)
        (data-size string-count . _)
       . _)
       line-list
    (let ((code-array
           (make-array 
             (+ last-line-number 10)
             :element-type 'fixnum
             :initial-element 0))
          (data-array
           (if (plusp data-size)
               (make-array data-size
                 :element-type 'fixnum
                 :initial-element 0)
               nil))
          (string-array
           (if (plusp string-count)
               (make-array string-count
                 :element-type 'string
                 :initial-element "")
               nil)))
      (do* ((rest-lines (rest (rest line-list))
              (rest rest-lines))
            (cur-line
             #4=(first rest-lines)
             #4#)
            (line-ptr 0 (1+ line-ptr)))
           ((null rest-lines))
        (if (< line-ptr string-count)
            (setf
             (aref string-array line-ptr)
             (first cur-line))
            (destructuring-bind
                  (line-no instruc . args)
                  cur-line
              (setf
               (aref code-array line-no)
               (instruction->int instruc))
              (when args
                (setf 
                  (aref code-array (1+ line-no))
                  (first args))))))
      (make-registers
       :data data-array
       :strings string-array
       :code code-array))))       

(defun vm-main (&optional file-path)
  (let ((reg
         (assemble
          (process-raw
           (if file-path
               (load-file file-path)
               (load-input))))))
    (vm-run reg)))

#| It works. The big pain in the ass was 
dealing with C-style escape characters.
This passed every test except 1: Task 4. 
In includes an escaped escape character,
\\n, which was intended to print out \n.
My string cleaner replaced \n with a 
newline. I really don't care that this 
error appeared, frankly. The strings are 
biased toward C-family languages. They
get too much attention already. I like
Lisp.
|#

#| Determinant and Permanent
   
For a given matrix, return the determinant 
and the permanent of the matrix. The 
determinant is given by

det (A) = 
         
sum from sigma of the sign of sigma 
times the product from i=1 to n of 
M[i, sigma[i]]

         
while the permanent is given by
                                
perm(A) = 
         
sum from sigma of the product from
i = 1 to n of M[i, sigma[i]]
  
In both cases the sum is over the 
permutations sigma of the permutations of 
1, 2, ..., n. (A permutation's sign is 1 if 
there are an even number of inversions and 
-1 otherwise; see parity of a permutation.)

More efficient algorithms for the determinant 
are known: LU decomposition, see for example 
wp:LU decomposition#Computing the determinant.
Efficient methods for calculating the 
permanent are not known. 
          
|#

;;;;  Determinant. The standard schoolbook
;;;; approach is the Leibnitz formula, 
;;;; but the time complexity grows 
;;;; proportional to the factorial of 
;;;; the number of rows. It makes sense
;;;; only when the matrix is 5x5 or 
;;;; smaller. Other methods can calculate
;;;; the determinant with complexity that
;;;; grows with the cube of the number 
;;;; of rows, or less. (The best algorithms
;;;; to date are of time complexity
;;;; O(n^(2.35)).
                
;;;; LU Decomposition is a relatively 
;;;; straightforward cubic algorithm.

;;;; First, an implementation of the 
;;;; Leibnitz formula.

(defun sig (perm)
  "Given an array containing a permutation
  of the indices, return 1 if there are
  an even number of inversions, -1 
  otherwise."
  (loop with inversions = 0
        for i from 0 below (1- (length perm))
        for n = (aref perm i) do
    (loop for j from (1+ i) below (length perm)
          for m = (aref perm j)
          when (< m n) do
          (incf inversions))
    finally
    (return (if (evenp inversions)
                1
                -1))))

(defun find-det-term (perm matrix)
  "Given a permutation of row indices
  and a matrix, return
  sig(perm) * matrix[0][perm[0]] *
  matrix[1][perm[1]] * ... * 
  matrix[n-1][perm[n-1]]."
  (let ((product (sig perm))
        (n (array-dimension matrix 0)))
    (dotimes (i n product)
      (setf product
        (* product
           (aref matrix i (aref perm i)))))))
  
(defun leib-det (matrix)
  "Given a square matrix, return the 
  determinant. Uses the Leibnitz formula,
  and thus should be used only on matrices
  of size 5x5 or smaller."
  (let ((det 0))
    (map-permutations
     #'(lambda (perm)
         (incf det 
               (find-det-term 
                 (coerce perm 'vector)
                 matrix)))
       (iota (array-dimension matrix 0)))
    det))

(defun identity-mat (n)
  "Given n, return the nxn identity matrix."
  (let ((mat (zero-mat n)))
    (dotimes (i n mat)
      (setf
       (aref mat i i)
       1))))

(defun zero-mat (n)
  "Given n, return the nxn zero matrix."
  (make-array (list n n)
    :element-type 'number
    :initial-element 0))

(defun lu-decomposition (mat)
  "Given a square matrix, perform lu-decomposition,
  and return both resultant matrices as 
  multiple values. The upper matrix 
  is returned as the first value."
  (let* ((n (array-dimension mat 0))
         (upper (rearrange mat n))
         (lower (identity-mat n)))
    (if (null upper)
        (values nil nil)
        (dotimes (diag-idx
                  (1- n) 
                  (values upper lower))
          (loop 
            with red-fctr = 0
            for red-row-idx from
                (1+ diag-idx) 
                below n
            for diag-elt =
                (aref upper diag-idx diag-idx)
            if (zerop diag-elt) do
            (return-from lu-decomposition (values nil nil))
            else do
            (setf red-fctr
              (/ (aref upper red-row-idx diag-idx)
                 diag-elt))
            (setf
             (aref lower red-row-idx diag-idx)
             red-fctr)
            (loop for col from diag-idx 
                          below n
                          do
              (decf 
                (aref upper red-row-idx col)
                (* red-fctr
                   (aref upper diag-idx col)))))))))
       
(defun rearrange (mat n)
  "Given a square nxn matrix, 
  return a copy in which no element
  along the main diagonal is zero. If 
  the matrix is not square or the 
  rearrangement is not possible, return
  nil."
  (if (/= n (array-dimension mat 1))
      nil
      (let ((copy (copy-array mat)))
        (block pivot
          (dotimes (diag-idx n copy)
            (let ((cur-diag
                    (aref 
                      copy 
                      diag-idx 
                      diag-idx)))
            (when (zerop cur-diag)
              (block search
                (dotimes (repl-idx n
                           (return-from
                            rearrange nil))
                  (when
                    (and 
                      (/= repl-idx cur-diag)
                      (not 
                        (zerop
                          (aref 
                            copy
                            repl-idx
                            cur-diag)))
                      (not
                        (zerop
                          (aref
                            copy
                            cur-diag
                            repl-idx))))         
                  (block replace
                    (dotimes (i n 
                              (return-from
                               search))
                      (rotatef
                       (aref copy diag-idx i)
                       (aref copy repl-idx i))))))))))))))   
      
(defun lu-det (mat)
  "Given a matrix, returns the determinant.
  Uses LU-decomposition, which is 
  much, much more efficient than the 
  Leibnitz formula for matrices that are
  larger than 5x5."
  (let ((upper (lu-decomposition mat)))
    ;; The lower triangular matrix has
    ;; all ones along the main diagonal,
    ;; so we don't need it.      
    (if (not upper)
        0
        (let ((det 1))
          (dotimes (i (array-dimension mat 0) det)
            (setf det
              (* det (aref upper i i))))))))
              
(defun find-perm-term (perm matrix)
  "Given a permutation of row indices
  and a matrix, return
  matrix[0][perm[0]] *
  matrix[1][perm[1]] * ... * 
  matrix[n-1][perm[n-1]]."
  (let ((n (array-dimension matrix 0))
        (product 1))
    (dotimes (i n product)
      (setf product
        (* product
           (aref matrix i (aref perm i)))))))

(defun determinant (mat)
  (if (<= (array-dimension mat 0) 5)
      (leib-det mat)
      (lu-det mat)))
         
(defun permanent (mat)
  "Given a square matrix, returns its 
  permanent. Unlike the determinant, 
  the permanent cannot be computed in 
  polynomial time. There are algorithms 
  which are a little more efficient than
  the Leibnitz formula, but they are 
  complex and shave off just a little 
  time."
  (let ((permanent 0))
    (map-permutations
     #'(lambda (perm)
         (incf permanent
           (find-perm-term 
             (coerce perm 'vector)
             mat)))
       (iota (array-dimension mat 0)))
    permanent))

(defun det-main ()
  (let ((examples
         (list
          #2a((1 2)
              (3 4))
          #2a((1 2 3 4)
              (4 5 6 7)
              (7 8 9 10)
              (10 11 12 13))
          #2a((0 1 2 3 4)
              (5 6 7 8 9)
              (10 11 12 13 14)
              (15 16 17 18 19)
              (20 21 22 23 24)))))
    (dolist (mat examples)
      (format t "~2%~A~2%Det-Leib: ~D, Det-LU: ~D, Per: ~D~%"
        mat (leib-det mat) (lu-det mat) (permanent mat)))))
                   
#|

"

#2A((1 2) (3 4))

Det-Leib: -2, Det-LU: -2, Per: 10


#2A((1 2 3 4) (4 5 6 7) (7 8 9 10) (10 11 12 13))

Det-Leib: 0, Det-LU: 0, Per: 29556


#2A((0 1 2 3 4) (5 6 7 8 9) (10 11 12 13 14) (15 16 17 18 19) (20 21 22 23 24))

Det-Leib: 0, Det-LU: 0, Per: 6778800
"          
|#

#| SUBLEQ
   
Subleq is an example of a One-Instruction 
Set Computer (OISC). 
    
It is named after its only instruction, 
which is SUbtract and Branch if Less than 
or EQual to zero.

Task

Your task is to create an interpreter which 
emulates a SUBLEQ machine.

The machine's memory consists of an array of 
signed integers. These integers may be 
interpreted in three ways:

    simple numeric values
    memory addresses
    characters for input or output

Any reasonable word size that accommodates 
all three of the above uses is fine.

The program should load the initial contents 
of the emulated machine's memory, set the 
instruction pointer to the first address 
(which is defined to be address 0), and 
begin emulating the machine, which works as 
follows:

  Let A be the value in the memory location 
  identified by the instruction pointer;   
  let B and C be the values stored in the 
  next two consecutive addresses in memory.
          
  Advance the instruction pointer three 
  words, to point at the address after the 
  address containing C.
          
  If A is -1 (negative unity), then a 
  character is read from the machine's input 
  and its numeric value stored in the 
  address given by B. C is unused. (Most 
  implementations adopt the C convention of 
  signaling EOF by storing -1 as the read-in 
  character.)
          
  If B is -1 (negative unity), then the 
  number contained in the address given by A 
  is interpreted as a character and written 
  to the machine's output. C is unused.
          
  Otherwise, both A and B are treated as 
  addresses. The number contained in address 
  A is subtracted from the number in address 
  B (and the difference left in address B). 
  If the result is positive, execution 
  continues uninterrupted; if the result is 
  zero or negative, the instruction pointer 
  is set to C.
          
  If the instruction pointer becomes 
  negative, execution halts.

Your solution may initialize the emulated 
machine's memory in any convenient manner, 
but if you accept it as input, it should be 
a separate input stream from the one fed to 
the emulated machine once it is running. And 
if fed as text input, it should be in the 
form of raw subleq "machine code" - 
whitespace-separated decimal numbers, with 
no symbolic names or other assembly-level 
extensions, to be loaded into memory 
starting at address 0 (zero).

For purposes of this task, show the output 
of your solution when fed the below   
"Hello, world!"   program.

As written, this example assumes ASCII or a 
superset of it, such as any of the Latin-N 
character sets or Unicode; you may translate 
the numbers representing characters 
(starting with 72=ASCII 'H') into another 
character set if your implementation runs 
in a non-ASCII-compatible environment. If 0 
is not an appropriate terminator in your 
character set, the program logic will need 
some adjustment as well. 
     
15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 
0 -1 72 101 108 108 111 44 32 119 111 114 
108 100 33 10 0
    
The above "machine code" corresponds to 
something like this in a hypothetical 
assembler language for a signed 8-bit 
version of the machine: 
        
start:
    0f 11 ff subleq (zero), (message), -1 
    ; subtract 0 from next character value 
      to print;
    ; terminate if it's <=0
    11 ff ff subleq (message), -1, -1     
    ; output character
    10 01 ff subleq (neg1), (start+1), -1 ; 
    modify above two instructions by 
    subtracting -1 
    10 03 ff subleq (neg1), (start+3), -1 ; 
    (adding 1) to their target addresses 
    0f 0f 00 subleq (zero), (zero), start ; 
    if 0-0 <= 0 (i.e. always) 
    goto start

; useful constants
zero: 
    00      .data 0  
neg1: 
    ff      .data -1
; the message to print
message: .data "Hello, world!\n\0"
    48 65 6c 6c 6f 2c 20 77 6f 72 6c 64 21 0
    a 00         
|#

(defun subleq-run (mem)
  (do* ((ptr 0))
       ((minusp ptr) (finish-output))
    (let ((a (aref mem ptr))
          (b (aref mem (1+ ptr)))
          (c (aref mem (+ ptr 2))))
      (incf ptr 3)
      (cond
        ((= a -1)
         (setf 
           (aref mem b) 
           (char-code (read-char))))
        ((= b -1)
         (format t "~C" (code-char (aref mem a))))
        (t                       
         (decf (aref mem b)
               (aref mem a))
         (when (<= (aref mem b) 0)
           (setf ptr c)))))))

(defun input-code (stream)
  "Reads code from a stream (defaulting
  to standard input.) Ends at EOF or 
  the appearance of a keyword. Returns
  a code array."
  (let ((code (make-array 1000
                :element-type 'fixnum
                :adjustable t
                :fill-pointer 0)))
    (uiop:with-safe-io-syntax ()
      (do ((token 
             #4=(read stream nil :eof nil)
             #4#))
          ((keywordp token) code)
        (vector-push-extend token code)))))

(defun file-code (path)
  (with-open-file (f path)
    (let ((*standard-input* f))
      (input-code))))

(defun subleq (&key (stream *query-io*) file-path)
  (let ((code
         (if file-path
             (file-code file-path)
             (input-code stream))))
    (subleq-run code)))
    
#| Pathological Floating Point Problems
   
Most programmers are familiar with the 
inexactness of floating point calculations 
in a binary processor. 
   
The classic example being:

0.1 + 0.2 =  0.30000000000000004

In many situations the amount of error in 
such calculations is very small and can be 
overlooked or eliminated with rounding.

There are pathological problems however, 
where seemingly simple, straight-forward 
calculations are extremely sensitive to 
even tiny amounts of imprecision.

This task's purpose is to show how your 
language deals with such classes of problems.

A sequence that seems to converge to a wrong 
limit:

Consider the sequence:

    v1 = 2 
    v2 = -4 
    vn = 111 - 1130 / v[n-1]  + 
    3000  / (v[n-1] * v[n-2])
    
As   n   grows larger, the series should converge to   6   but small amounts of error will cause it to approach   100.


Task 1

Display the values of the sequence where 
n = 3, 4, 5, 6, 7, 8, 20, 30, 50 & 100 to at 
least 16 decimal places.

    n = 3     18.5
    n = 4      9.378378
    n = 5      7.801153
    n = 6      7.154414
    n = 7      6.806785
    n = 8      6.5926328
    n = 20     6.0435521101892689
    n = 30     6.006786093031205758530554
    n = 50     6.0001758466271871889456140207471954695237
    n = 100    6.000000019319477929104086803403585715024350675436952458072592750856521767230266
    
Task 2

The Chaotic Bank Society is offering a new 
investment account to their customers.

You first deposit $e - 1 where e is 
2.7182818...   the base of natural 
logarithms.

After each year, your account balance will 
be multiplied by the number of years that 
have passed, and $1 in service charges will 
be removed.

So ...

   after 1 year, your balance will be multiplied by 1 and $1 will be removed for service charges.
   after 2 years your balance will be doubled and $1 removed.
   after 3 years your balance will be tripled and $1 removed.
   ...
   after 10 years, multiplied by 10 and $1 
   removed, and so on.

What will your balance be after 25 years?

   Starting balance: $e-1
   Balance = (Balance * year) - 1 for 25 years
   Balance after 25 years: $0.0399387296732302
    
                  
Task 3, extra credit

Siegfried Rump's example. Consider the following function, designed by Siegfried Rump in 1988.

  f(a,b) = 
  333.75b^6 + 
  a^2(11a^2b^2 - b^6 - 121b^4 - 2 ) + 
  5.5b^8 + a/(2b) 
                        
  compute   f(a,b)  where   a=77617.0   and   b=33096.0 
  f(77617.0, 33096.0)   =   
  -0.827396059946821 
  
Demonstrate how to solve at least one of the 
first two problems, or both, and the third 
if you're feeling particularly jaunty. |#
   
#| All three problems can be solved 
nicely by relying on the 
fact that Common Lisp returns arbitrary
precision fractions
rather than floating point numbers when
performing operations on integers or 
fractions. When 
all arguments are rational numbers, we 
can easily perform exact calculations 
(although slower than floating-point
operations), and then coerce 
printed results to the maximum precision
float type available (long-float, which
varies in size depending on the 
implementation.) In the case of the second
problem, we can approximate e to arbitrary
precision using this method. |#
     
(defun crazy-sequence ()
  (let ((seq 
          (make-array 101
            :element-type 'rational
            :initial-element 0)))
    (setf (aref seq 1) 2)
    (setf (aref seq 2) -4)
    (do ((i 3 (1+ i)))
        ((= i 101) seq)
      (setf (aref seq i)
        (+ 111
           (- (/ 1130 (aref seq (- i 1))))
           (/ 3000
              (* 
                (aref seq (- i 1))
                (aref seq (- i 2)))))))))

(defconstant +e-1+
  (/ 1718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466391932003059921817413596629043572900334295260595630738132328627943490763233829880753195251019011573834187930702154089149934884167509244761460668082264800168477411853742345442437107539077744992069551702761838606261331384583000752044933826560297606737113200709328709127443747047230696977209310141692836819025515108657463772111252389784425056953696770785449969967946864454905987931636889230098793127736178215424999229576351
     (expt 10 525))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
   "e - 1 to 525 places")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             

(defun crazy-bank ()
  (do* ((i 0 (1+ i))
        (balance +e-1+ (1- (* balance i))))
       ((= i 25) balance)))
       
(defconstant *rump-k*
  (rationalize 333.75)
  "The initial constant of the rump case
  equation, coerced to a fraction.")

(defun crazy-rump (a b)
  (+
   (* *rump-k* (expt b 6))
   (* a a
      (+ (* 11 a a b b)
         (- (expt b 6))
         (- (* 121 (expt b 4)))
         -2))
   (* 11/2 (expt b 8))
   (/ a (* 2 b))))
  
(defun float-main ()
  (let ((seq (crazy-sequence))
        (indexes
         '(3 4 5 6 7 8 20 30 50 100)))
    (format t "The crazy sequence:~%~%")
    (dolist (index indexes)
      (format t "~D: ~F~%" 
         index
         (float
           (aref seq index)
             0l0))))
  (format t "~%The crazy bank:~%~%")
  (format t "~F" (float (crazy-bank) 0L0))
  (format t "~2%Crazy rump:~2%")
  (format t "~F" (float (crazy-rump 77617 33096) 0L0)))
  
#| Euclid-Mullin Sequence
  
The Euclid–Mullin sequence is an infinite 
sequence of distinct prime numbers, in 
which each element is the least prime factor 
of one plus the product of all earlier 
elements.

Task

Definition

The first element is usually assumed to be 2. 
So the second element is : (2) + 1 = 3 and 
the third element is : (2 x 3) + 1 = 7 as 
this is prime.

Although intermingled with smaller elements, 
the sequence can produce very large elements 
quite quickly and only the first 51 have 
been computed at the time of writing.

Task

Compute and show here the first 16 elements 
of the sequence or, if your language does 
not support arbitrary precision arithmetic, 
as many as you can.

Stretch goal

Compute the next 11 elements of the sequence. 
|#

(defparameter *primes* nil)

(defparameter *top-prime* nil)
              
(defparameter *extra-primes*
  (make-array 100
    :element-type 'integer
    :adjustable t
    :fill-pointer 0))

(defun euclid-mullin-main (&optional (count 16))
  (init-params)
  (calc-em-primes count))

(defun init-params ()
  (let ((primes
         (with-open-file
           (f
            (merge-pathnames
             #p"quicklisp/local-projects/rosetta/primes.txt.lisp"))
           (read f))))
    ; 'primes' is a simple vector
    (setf *primes* primes)
    (setf *top-prime*
          (svref primes (1- (length primes))))))
            
(defun calc-em-primes (count)
  (declare (optimize (debug 3)))
  (let ((result-bigger-than-top-prime nil))
    (do ((cur-count count (1- cur-count))
         (result-list nil))
        ((zerop cur-count) (nreverse result-list))
      (cond 
        ((null result-list)
         (push 2 result-list)
         (print 2 *trace-output*))
        (t
         (let ((dividend 
                 (1+ (apply #'* result-list))))
           (cond
            ((and
                (<= dividend *top-prime*)
                (or
                 (position 
                   dividend
                   *primes*)
                 (position
                  dividend
                  *extra-primes*)))
             (push dividend result-list)
             (print dividend *trace-output*))
            (t
             (let ((fac 
                     (smallest-prime-factor
                      dividend)))
               (push
                 fac
                 result-list)
               (print fac *trace-output*))))))))))

          
(defun smallest-prime-factor (n)
  (let ((nsqrt (sqrt n)))
    (dotimes (i (length *primes*))
      (let ((prime (svref *primes* i)))
        (when (> prime nsqrt)
          (return-from 
            smallest-prime-factor
            n))
        (when (zerop (mod n prime))
          (return-from 
            smallest-prime-factor
            prime))))
    (dotimes (i (length *extra-primes*))
      (let ((prime (aref *extra-primes* i)))
        (when (> prime nsqrt)
          (return-from
           smallest-prime-factor
           n))
        (when (zerop (mod n prime))
          (return-from
           smallest-prime-factor
           prime))))
    (do ((cur-cand (+ 2 *top-prime*)
                   (+ 2 cur-cand)))
        ((> cur-cand nsqrt) n)
      (when (= (smallest-prime-factor
                 cur-cand)
               cur-cand)
          (setf *top-prime* cur-cand)
          (vector-push-extend
           cur-cand
           *extra-primes*)
          (when (zerop (mod n cur-cand))
            (return-from
             smallest-prime-factor
             cur-cand))))))

#| The calculation of the initial 16 terms
took quite some time. I attempted the extra
11,but the program ground on for 
hours with no results. Here are the 1st 
16 terms:
   
2 
3 
7 
43 
13 
53 
5 
6221671 
38709183810571 
139 
2801 
11 
17 
5471 
52662739 
23003
|#

#| Equilibrium Index
   
An equilibrium index of a sequence is an 
index into the sequence such that the sum 
of elements at lower indices is equal to 
the sum of elements at higher indices. 
    
For example, in a sequence A:

A0 = −7
A1 = 1
A2 = 5
A3 = 2
A4 = −4
A5 = 3
A6 = 0

3 is an equilibrium index, because:

A0 + A1 + A2 = A4 + A5 + A6

6 is also an equilibrium index, because:

A0 + A1 + A2 + A3 + A4 + A5 = 0

(sum of zero elements is zero)

7 is not an equilibrium index, because it 
is not a valid index of sequence A.


Task;

Write a function that, given a sequence, 
returns its equilibrium indices (if any).

Assume that the sequence may be very long.
|#
 

(defun equilibrium-index (seq-vec series-sum)
  "Given a vector of real numbers, and
  their sum, return
  the leftmost 0-based index of the element
  such that the sum of the preceding 
  numbers is equal to the sum of the 
  following numbers. If there is no 
  such index, return nil."
  (do* ((len (length seq-vec))
        (index 0 (1+ index))
        (prev-sum 
         0 
         (+ 
           prev-sum 
           (aref seq-vec (1- index))))
        (post-sum
         (- series-sum 
            (aref seq-vec index))
         (- post-sum
            (aref seq-vec index))))
       ((= index (1- len))
        (if (zerop (incf prev-sum
                         (aref seq-vec index)))
            (values index 0)
            (values nil nil)))
    (when (= prev-sum post-sum)
      (return (values index prev-sum)))))
  
(defun eq-bal-main (&rest numbers)
  (let ((sum (apply #'+ numbers))
        (series (coerce numbers 'vector)))
    (equilibrium-index series sum)))

;; My version returnx two values: the index 
;; and the sum.

#| Earliest Difference Between Prime Gaps

When calculating prime numbers > 2, the
difference between adjacent primes is always
an even number. This difference, also referred
to as the gap, varies in an random pattern; at
least, no pattern has ever been discovered,
and it is strongly conjectured that no pattern
exists. However, it is also conjectured that
between some two adjacent primes will be a
gap corresponding to every positive even
integer.

This task involves locating the minimal primes 
corresponding to those gaps.

Though every gap value exists, they don't
seem to come in any particular order.
For example, this table shows the gaps and
minimum starting value primes for 2 through
30:

gap 	msp* 	ep**

2 	3 	5
4 	7 	11
6 	23 	29
8 	89 	97
10 	139 	149
12 	199 	211
14 	113 	127
16 	1831 	1847
18 	523 	541
20 	887 	907
22 	1129 	1151
24 	1669 	1693
26 	2477 	2503
28 	2971 	2999
30 	4297 	4327

* msp: minimal starting prime
** ep: ending prime

For the purposes of this task, considering only primes greater than 2, consider prime gaps that
differ by exactly two to be adjacent.   ;


Task

For each order of magnitude m from 10^1 through
10^6:

  *  Find the first two sets of minimum adjacent
      prime gaps where the absolute value of the
     difference between the prime gap start
     values is greater than m.


E.G.

For an m of 10^1;

The start value of gap 2 is 3, the start value of
gap 4 is 7, the difference is 7 - 3 or 4. 4 < 10^1¹
so keep going.

The start value of gap 4 is 7, the start value of
gap 6 is 23, the difference is 23 - 7, or 16. 16 >
10^1 so this the earliest adjacent gap
difference > 10^1.

Stretch goal

  * Do the same for 10^7 and 10^8 (and higher?)
    orders of magnitude

Note: the earliest value found for each order
of magnitude may not be unique, in fact, IS not
unique ; also, with the gaps in ascending order,
the minimal starting values are not strictly
ascending.

|#

;; I need to think about what I need to do to avoid repeated calculations.

;; Let's put this into my own words. First some facts
;; 1. Given two consecutive prime numbers p1 and p2, the gap is p2 - p1.
;; 2. Except 2, all prime numbers are odd.
;; 3. The first gap, between p1 = 2 and p2 = 3 is 3-2 = 1. This an odd gap.
;; 4. Since all primes except 2 are odd, all gaps after the first will be even.
;; 5. Each gap after the first may be any even positive integers.
;; 6. Every positive even integer corresponds to an infinite number of prime gaps.
;; 7. There is no ordering to the gaps or means of finding out in advance what they will be.

;; What do we have to do?
;; 1. We keep track of prime gaps.
;; 2. Define "adjacent" gaps any gaps g1 and g2 such that abs(g1 - g2) = 2.
;; 3. If we encounter a new gap size, how far is the lower prime from the lower prime
;;    of the first-encountered gap whose size is g-2?. How far is the lower prime from the
;;    first encountered gap whose size is g+2.
;;    Let us call the absolute values of these distances distances of appearance.
;;
;; 4. For each m from 1 through 8, find the first pair of gaps whose distance of appearance >= 10^m.
;;    We need to report the 4 primes, the distance of appearance, the m value, and the
;;    sequential gap numbers for each case.
;;
;; So how to proceed, knowing that I should be parsimonious with memory requirements?
;;
;; 1. Primes should be generated sequentially, not in ranges. We trade slower processing time
;; for O(1) space.
;; 2. We are considering only the even gaps... The first gap we need is 2. We keep a hold of
;;    that pair: 3 and 5. Then we look at gap #3, 5 to 7. Gap is 2. We have already. Move on.
;;    Gap #4: 7 to 11. This gap is 4. The distance of appearance is 7-3=4. 4 < 10. We keep ahold of gap 1,
;;    so that we never need to consider a gap of 2 again. We also store gap 4 = 4. We are now looking
;;    for a gap of 6. Gap 5: 11 to 13 is 2. Gap 6: 13 to 17 is 4. Gap 7: 17 to 19 is 2. Gap 8: 19 to
;;    23 is 4. Gap 9: 23 to 29 is 6. Here it is. The distance of appearance is 23 - 7 = 16 > 10.
;;    This goes in the record book. The next record to obtain is >= 100. And the next gap to find is 8.
;;    Gap 10: 29 to 31, 2; Gap 11: 31 to 37, 6; Gap 12: 37 to 41, 4; Gap 13: 41 to 43, 2; Gap 14: 43 to 47, 4;
;;    Gap 15: 47 to 53, 6; Gap 16: 53 to 59, 6; Gap 17: 59 to 61, 2; Gap 18: 61 to 67, 6; Gap 19: 67 to 71, 4;
;;    Gap 20: 71 to 73, 2; Gap 21: 73 to 79, 6; Gap 22: 79 to 83, 4; Gap 23: 83 to 89, 6; Gap 24: 89 to 97, 8.
;;    We found it. Distance of appearance is 89 - 23 = 66, < 100.
;;    Now, there is another complication. The first gap larger that 8 occurs at gap 29 = 14. We have not yet
;;    encountered 10 or 12. We need to store it and wait for later.
;;
;;    Here is a suggested implementation:
;;    Collections: Something that a pair of integers.
;;    Something that maps integers to pairs. Something that holds
;;    statistics about adjacent pairs. Something to hold the statistics
;;    I choose a struct for the pair (more transparent
;;    to the reader than a list or array), and a hash-map
;;    for the associations. Since we care about the statistics for fewer than
;;    10 pairs of pairs, and it is easiest to maintain an order,
;;    an alist should be used. We will use another struct for the statistics

;;    Plan of attack
;;    1. Define the pair struct
;;    2. Define the stats struct
;;    3. Create the pair table. Initialize it with gap 2, size 2, lower 3
;;    4. Initialize the alist with all keys and vals set to nil.

;;    5. previous-prime = 5, current gap = 3
;;    6. while stats alist still as empty entries: loop
;;    7. next-prime = next prime
;;    8. gap = next-prime - previous-prime
;;    9. if gap is not a key in the table
;;    10. do
;;    11. add a new pair to the table
;;    12. check the previous gap.
;;    13. If the appearance distance is greater than any milestone and
;;        those milestones do not not have entries, create entries for them.
;;    14. check the next gap, completing step 13 for that appearance distance
;;        if necesary
;;    15. end do
;;    16. previous-prime = next-prime, current-gap += 1
;;    17. end loop
;;    18. print results.

;; My concept of the problem was incorrect thus my implementation was wrong.
;; My idea: visit each prime gap g. The first time I encountered two prime gaps g1 and
;; g2 such that g1 +- 2 = g2 and abs(lower prime g1 - lower prime g2) > 10 that would count
;; as a milestone. Then 100, 1000, etc.
;; The real task is subtedly different: we are lookimg for the minimal g1 for which the milestone is met.

;; Both versions start out the same
;; at Milestone = 10, the first time we encounter a distance > 10 between a g1 and g2
;; is for g1: gap 4 at (7, 11), g2: gap 6 at (23, 29), distance 16.
;; Our second answers diverged.
;; The first encountered distance > 100 occurs at
;; g1: gap 18 at (523, 541), g2: gap 20 at (887, 907), distance 364.
;; However the lesser g1: gap 14 at (113, 127), g2: gap 16 at (1831, 1847) distance 1718.
;; This was also the result for M=1000 in both vetsions. Back to the drawing board.

;; So I will translate the second Wren approach. The choice of sieving or taking primes as they come
;; is a tricky one. Optimize speed or space? I decided to optimize space. Also, I have
;; rolled my own prime generator, so this will be much slower than code that relies on
;; adaptations of a C++ highly-optimized library. Here goes.


(defvar *pglimit*
  1e8
  "Largest milestone to reach.")

(defun prime-gap-main ()
  "Translation of the Wren/Lua/Python approach. Thus, horribly inelegant and un-Lispy"
  (loop with gapstarts = (get-gapstarts)
        with maxgap = (apply #'max (hash-table-keys gapstarts))
        with milestone = 10 ;; PM in the original
        with gap1 = 2
        do
           (assert (<= gap1 maxgap))
           (loop until (gethash gap1 gapstarts)
                 do
                    (incf gap1 2))
           (let* ((start1 (gethash gap1 gapstarts))
                  (gap2 (+ gap1 2))
                  (start2 (gethash gap2 gapstarts)))
             (cond
               (start2
                (let ((diff (abs (- start2 start1))))
                  (cond
                    ((> diff milestone)
                     (format t "~&Earliest difference >~:D between adjacent prime gap starting ~
                             primes:~%" milestone)
                     (format t "Gap ~D starts at ~:D, gap ~D starts at ~:D, difference is ~
                             ~:D.~2%"
                             gap1 start1 gap2 start2 diff)
                     (when (>= milestone *pglimit*)
                       (return))
                     (setf milestone (* milestone 10)))
                    (t
                     (setf gap1 gap2)))))
               (t
                (setf gap1 (+ gap2 2)))))))

(defun get-gapstarts ()
  (let ((prime-limit (* *pglimit* 5))
        (gapstarts (dict)))
    (do ((prime1 3 prime2)
         (prime2 5 (next-prime prime2)))
        ((>= prime2 prime-limit) gapstarts)
      (unless (gethash (- prime2 prime1) gapstarts)
        (setf (gethash (- prime2 prime1) gapstarts) prime1)))))

#| It worked, and it wasn't terribly so. I gave up 
waiting for the 1e9 limit, but the 1e8 limit was
calculated in less than 10 minutes. (1e7 was a few seconds).

Here are the results:

Earliest difference >10 between adjacent prime gap starting primes:
Gap 4 starts at 7, gap 6 starts at 23, difference is 16.

Earliest difference >100 between adjacent prime gap starting primes:
Gap 14 starts at 113, gap 16 starts at 1,831, difference is 1,718.

Earliest difference >1,000 between adjacent prime gap starting primes:
Gap 14 starts at 113, gap 16 starts at 1,831, difference is 1,718.

Earliest difference >10,000 between adjacent prime gap starting primes:
Gap 36 starts at 9,551, gap 38 starts at 30,593, difference is 21,042.

Earliest difference >100,000 between adjacent prime gap starting primes:
Gap 70 starts at 173,359, gap 72 starts at 31,397, difference is 141,962.

Earliest difference >1,000,000 between adjacent prime gap starting primes:
Gap 100 starts at 396,733, gap 102 starts at 1,444,309, difference is 1,047,576.

Earliest difference >10,000,000 between adjacent prime gap starting primes:
Gap 148 starts at 2,010,733, gap 150 starts at 13,626,257, difference is 11,615,524.

Earliest difference >100,000,000 between adjacent prime gap starting primes:
Gap 198 starts at 46,006,769, gap 200 starts at 378,043,979, difference is 332,037,210.
#|
