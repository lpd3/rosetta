(in-package :prime-utils-test)

;; Import all symbols internal to prime-utils

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (with-package-iterator (generator '(:prime-utils) :internal)
    (do () ()
      (multiple-value-bind
            (morep symbol)
          (generator)
        (unless morep
          return)
        (import symbol)))))

(define-test prime-utils-test-suite)
