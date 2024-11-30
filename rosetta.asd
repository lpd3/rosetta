(eval-when (:load-toplevel)
  (qml:quicklisp))
  
(let ((dependencies
       '(:alexandria
         :serapeum
         :iterate
         :series
         :cl-ppcre
         :repl-utilities
         :computable-reals
	 :fiveam)))
  (dolist (dep dependencies)
    (when (not (find-package dep))
      (quicklisp:quickload dep))))

(in-package :asdf-user)

(defsystem "rosetta"
  :description "Solutions to tasks on 
  rosettacode.org"
  :author "Larry Devlin"
  :mailto "larrydevlin1770@gmail.com"
  :license "Copyright © 2024 Larry Devlin.
  All rights reserved."
  :depends-on ("alexandria" "serapeum"
               "iterate" "series" "cl-ppcre"
               "repl-utilities" "computable-reals" "fiveam")
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "prime-utils")
	       (:file "prime-test")
	       (:file "rc-001")
               (:file "rc-002")
               (:file "rc-003")
               (:file "rc-004")
               (:file "rc-005")
               (:file "rc-006")))
                
