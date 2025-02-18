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
  :components ((:file "src/packages")
               (:file "src/ros-conditions" :depends-on ("src/packages"))
               (:file "src/utilities" :depends-on ("src/ros-conditions"))
               (:file "src/prime-utils" :depends-on ("src/ros-conditions"))
	       (:file "src/rc-001" :depends-on ("src/packages"))
               (:file "src/rc-002" :depends-on ("src/packages"))
               (:file "src/rc-003" :depends-on ("src/packages"))
               (:file "src/rc-004" :depends-on ("src/packages"))
               (:file "src/rc-005" :depends-on ("src/packages"))
               (:file "src/rc-006" :depends-on ("src/prime-utils"))
               (:file "src/pierpoint-primes" :depends-on ("src/prime-utils")))
  :in-order-to ((test-op (test-op "rosetta/tests"))))

(defsystem "rosetta/tests"
  :depends-on ("rosetta" "parachute" "alexandria" "serapeum")
  :serial t
  :components ((:file "tests/packages")
               (:file "tests/prime-utils-test"))
  :perform (test-op (o s)
                    (uiop:symbol-call :parachute '#:test
                      (uiop:find-symbol* '#:prime-utils-test-suite :prime-utils-test))))
  
