
(in-package :cl-cudd.baseapi)

(define-foreign-library libcudd
  (t (:default "/usr/local/lib/libcudd" #|"libcudd"|#)))

(use-foreign-library libcudd)

