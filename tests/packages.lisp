(in-package :cl-user)

(defpackage :prime-utils-test
  (:use :cl :prime-utils :ros-conditions :parachute)
  (:import-from :alexandria)
  (:import-from :serapeum))

(in-package :prime-utils-test)

(defparameter *source-code-system*
  :rosetta)

(defparameter *test-system*
  :rosetta/tests)
