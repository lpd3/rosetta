(in-package :cl-user)

(defpackage :prime-utils-test
  (:use :cl :prime-utils :ros-conditions :parachute)
  (:import-from :ros-utils
     :binary-search)
  (:import-from :alexandria)
  (:import-from :serapeum))

(in-package :prime-utils-test)

(defparameter *source-code-system*
  :rosetta)

(defparameter *test-system*
  :rosetta/tests)
