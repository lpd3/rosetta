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
  :components ((:file "src/packages")
               (:file "src/ros-conditions")
               (:file "src/utilities")
               (:file "src/prime-utils")
	       (:file "src/rc-001")
               (:file "src/rc-002")
               (:file "src/rc-003")
               (:file "src/rc-004")
               (:file "src/rc-005")
               (:file "src/rc-006"))
  :in-order-to ((test-op (test-op "rosetta/test"))))

(defsystem "rosetta/test"
  :depends-on ("rosetta" "parachute" "alexandria" "serapeum")
  :serial t
  :components ((:file "test/packages")
               (:file "test/prime-utils-test"))
  :perform (test-op (o s)
                    (uiop:symbol-call :parachute '#:test
                      (uiop:find-symbol* '#:prime-utils-test-suite :prime-utils-test))))
  
