(in-package :cl-user)

(when (not (find-package "ALEXANDRIA"))
  (ql:quickload :alexandria))

(defpackage :divigibles
  (:use :cl))
