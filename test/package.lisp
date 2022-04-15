(in-package :asdf-user)

(define-package cl-cudd.test
    (:mix
     :cl-cudd :cl-cudd.baseapi
     :cl-cudd.internal-utils
     :fiveam :iterate :trivia :arrow-macros
     :asdf :uiop
     :cl)
  (:reexport :fiveam)
  (:shadow :next :<>)
  (:export
   #:parse-bdd
   #:bdd

   #:run!
   #:test-with-autoreorder-enabled))

(in-package cl-cudd.test)
