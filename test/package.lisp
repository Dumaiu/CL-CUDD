(in-package :asdf-user)

(define-package cl-cudd.test.common
    (:mix-reexport
     ;; :cl-cudd
     ;; :cl-cudd.baseapi
     :cl-cudd.thread-safe
     :cl-cudd.internal-utils
     :fiveam :iterate :trivia :arrow-macros
     :asdf :uiop
     :cl))

(define-package cl-cudd.test
    (:mix cl-cudd.test.common
     :cl)
  (:reexport :fiveam)
  (:shadow :next :<>)
  (:export
   #:parse-bdd
   #:bdd

   #:run!
   #:test-with-autoreorder-enabled))

(in-package cl-cudd.test)
