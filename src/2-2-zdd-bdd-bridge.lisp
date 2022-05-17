(in-package :cudd)

;; CUDD-ZDD-VARS-FROM-BDD-VARS
;; CUDD-ZDD-PORT-TO-BDD
;; CUDD-ZDD-PORT-FROM-BDD

;; From the definition of DdManager, it seems like the BDD variables and the ZDD
;; variables are managed separately. In other words, they have sort-of
;; separate namespaces.

(declaim (reentrant bdd->zdd-simple))
(defun bdd->zdd-simple (bdd)
  "Converts a BDD to a ZDD via simple 1-to-1 variable conversion.
This function internally calls cudd-zdd-vars-from-bdd-vars and increases the ZDD variable table size as necessary."
  (declare (bdd-node bdd))
  (let-1 m (node-manager bdd)
    (declare (manager m))
    (with-cudd-critical-section (:manager m)
      (let-1 mp (manager-pointer m)
        (declare (manager-pointer mp))
        (assert* (not (null-pointer-p mp)))
        ;; TODO check if there are zdd vars?
        (assert (= 1 (cudd-zdd-vars-from-bdd-vars mp 1)))
        (wrap-and-finalize
            (cudd-zdd-port-from-bdd mp (node-pointer bdd))
            'zdd-node
          :manager m)))))

(declaim (reentrant bdd->zdd-simple))
(defun zdd->bdd-simple (zdd)
  "Converts a ZDD to a BDD via simple 1-to-1 variable conversion."
  (declare (zdd-node zdd))
  (let-1 m (node-manager zdd)
    (declare (manager m))
    (with-cudd-critical-section (:manager m)
      (let-1 mp (manager-pointer m)
        (declare (manager-pointer mp))
        (assert* (not (null-pointer-p mp)))
        (wrap-and-finalize
            (cudd-zdd-port-to-bdd mp (node-pointer zdd))
            'bdd-node
          :manager m)))))


(declaim (reentrant bdd->zdd-cover))
(defun bdd->zdd-cover (bdd)
  "Converts a BDD to a ZDD via 1-to-2 conversion for cover representation.
This function internally calls cudd-zdd-vars-from-bdd-vars and increases the ZDD variable table size as necessary."
  (declare (bdd-node bdd))
  (let-1 m (node-manager bdd)
    (declare (manager m))
    (with-cudd-critical-section (:manager m)
      (let-1 mp (manager-pointer m)
        (declare (manager-pointer mp))
        (assert* (not (null-pointer-p mp)))
        (assert (= 1 (cudd-zdd-vars-from-bdd-vars mp 2)))
        (wrap-and-finalize
            (cudd-zdd-port-from-bdd mp (node-pointer bdd))
            'zdd-node
          :manager m)))))

(declaim (reentrant zdd->bdd-cover))
(defun zdd->bdd-cover (zdd)
  "Converts a BDD to a ZDD via 1-to-2 conversion for cover representation."
  (declare (zdd-node zdd))
  (let-1 m (node-manager zdd)
    (declare (manager m))
    (with-cudd-critical-section (:manager m)
      (let-1 mp (manager-pointer m)
        (declare (manager-pointer mp))
        (assert* (not (null-pointer-p mp)))
    (wrap-and-finalize
        (cudd-zdd-port-to-bdd mp (node-pointer zdd))
        'bdd-node
      :manager m)))))
