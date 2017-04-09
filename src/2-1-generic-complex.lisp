(in-package :cudd)

(def-cudd-call exist-abstract ((:add cudd-add-exist-abstract :bdd cudd-bdd-exist-abstract)
                               (f :node) (cube :node))
  :generic "Abstracts all the variables in CUBE from the DD F by
  taking the sum over all possible values taken by the variables.
  Returns the abstracted DD.")

(def-cudd-call or-abstract ((:add cudd-add-or-abstract :bdd cudd-bdd-exist-abstract)
                            (f :node) (cube :node))
  :generic "Abstracts all the variables in CUBE from the DD F by
taking the disjunction over all possible values taken by the variables.
Returns the abstracted DD.

If abstracting an ADD, we assume that it is an 0-1-ADD.")

(def-cudd-call univ-abstract ((:add cudd-add-univ-abstract :bdd cudd-bdd-univ-abstract)
                              (f :node) (cube :node))
  :generic "Abstracts all the variables in CUBE from the DD F by
taking the conjunction over all possible values taken by the variables.
Returns the abstracted DD.

If abstracting an ADD, we assume that it is an 0-1-ADD")


(defun cofactor (f g)
  "Computes the cofactor of f with respect to g; g must be the BDD or the ADD of a cube."
  (let* ((p
          (with-pointers ((f f) (g g))
            (cudd-cofactor %mp% f g))))
    (wrap-and-finalize p (type-of f))))


