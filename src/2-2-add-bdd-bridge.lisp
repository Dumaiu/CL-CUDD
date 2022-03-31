(in-package :cudd)

(defun bdd->add (bdd &key (manager (node-manager bdd)))
  "Converts a BDD to a 0-1 ADD"
  (declare (bdd-node bdd)
           (manager manager))
  (wrap-and-finalize
   (cudd-bdd-to-add (manager-pointer manager) (node-pointer bdd))
   'add-node
   :manager manager))

(defun add->bdd (add &key (manager (node-manager add)))
  "Converts an ADD to a BDD by replacing all discriminants different from 0 with 1."
  (declare (add-node add))
  (declare (manager manager))
  (wrap-and-finalize
   (cudd-add-bdd-pattern (manager-pointer manager) (node-pointer add))
   'bdd-node
   :manager manager))

(defun add->bdd-interval (add lower upper &key ((:manager m) (node-manager add)))
  "Converts an ADD to a BDD by replacing all discriminants greater than or equal to lower and less
  than or equal to upper with 1, and all other discriminants with 0."
  (declare (add-node add))
  (declare (manager m))
  (wrap-and-finalize (cudd-add-bdd-interval
                      (manager-pointer m)
                      (node-pointer add)
                      lower upper)
                     'bdd-node
                     :manager m))

(defun add->bdd-strict-threshold (add threshold  &key ((:manager m) (node-manager add)))
  "Converts an ADD to a BDD by replacing all discriminants STRICTLY greater than value with 1, and
  all other discriminants with 0."
  (declare (add-node add))
  (declare (manager m))
  (wrap-and-finalize (cudd-add-bdd-strict-threshold
                      (manager-pointer m)
                      (node-pointer add)
                      threshold)
                     'bdd-node
                     :manager m))

(defun add->bdd-threshold (add threshold  &key ((:manager m) (node-manager add)))
  "Converts an ADD to a BDD by replacing all discriminants greater than or equal to value with 1, and
  all other discriminants with 0."
  (declare (add-node add))
  (declare (manager m))
  (wrap-and-finalize (cudd-add-bdd-threshold
                      (manager-pointer m)
                      (node-pointer add)
                      threshold)
                     'bdd-node
                     :manager m))

