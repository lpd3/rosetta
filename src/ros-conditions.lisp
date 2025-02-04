;;;; ros-conditions: conditions defined for the rosetta system

(in-package :ros-conditions)

(define-condition ros-error (error)
  ()
  (:documentation "Base error type for errors defined for the rosetta system"))

(define-condition type-error* (ros-error)
  ((argument
    :reader type-error*-argument
    :initform (error "type-error requires the offending argument")
    :initarg :argument
    :documentation "The argument that caused the error")
   (type
    :reader type-error*-type
    :initarg :type
    :documentation "The type of the offending argument")
   (expected-type
    :reader type-error*-expected-type
    :initarg :expected-type)
   (location
    :reader type-error*-location
    :initarg :location
    :initform (error "type-error requires a location")
    :documentation "The callable in which this error occurred"))
  (:documentation "Error signaled when argument is of the wrong type"
   :report (lambda (e stream)
             (with-slots
                 (argument type expected-type location)
                 e
               (format stream "~A expects type ~A, not ~A ~S."
                       location expected-type type argument)))))

(define-condition domain-error (ros-error)
  ((argument
    :reader domain-error-argument
    :initarg :argument
    :documentation "The argument that was out of domain")
   (domain
    :reader domain-error-domain
    :initarg :domain
    :documentation "The domain constraint of the argument")
   (location
    :reader domain-error-location
    :initarg :location
    :initform (error "domain-error requires a location")
    :documentation "The callable and arg position in which this error occurred"))
  (:documentation "Error signaled when a numerical argument is outside the domain of the function."
   :report (lambda (e stream)
             (with-slots
                   (argument domain location)
                   e
               (format stream "Argument ~S is out of domain for ~A, which is ~A."
                       argument location domain)))))

(define-condition large-range-error (ros-error)
  ((range
    :reader large-range-error-range
    :initarg :range
    :initform (error () "large-range-error requires a range")
    :documentation "The range in question"))
  (:documentation "Error signaled when a function that returns a sequence is given arguments that could exhaust memory if the function is executed"
   :report (lambda (e stream)
             (let ((range (large-range-error-range e)))
               (format stream "Range is ~D, which is risky." range)))))
