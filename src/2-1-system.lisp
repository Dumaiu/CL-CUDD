(in-package :cudd)

(defmacro define-simple-managed-function (name interface &body (&whole doc-maybe &optional doc))
  " NOTE: INTERFACE should be a unary function from `:cl-cudd.baseapi', taking only a `manager-pointer'.
  - [2021-11-04 Thu]: The generated function will have an &optional 'manager' parameter.
  * TODO: Accept &key *or* &optional arg. "
  (declare (symbol name interface)
           (type (or null string) doc)
           (ignorable doc))
  `(defun ,name (&optional (manager *manager*))
     ,@doc-maybe
     (declare (type manager manager))
     (with-cudd-critical-section (:manager manager)
       (,interface
        (manager-pointer manager)))))

(define-simple-managed-function disable-gc cudd-disable-garbage-collection
  "Disables garbage collection. Garbage
collection is initially enabled. This function may be called to
disable it. However, garbage collection will still occur when a new
node must be created and no memory is left, or when garbage collection
is required for correctness. (E.g., before reordering.)")

(define-simple-managed-function enable-gc cudd-enable-garbage-collection
  "Enables garbage collection. Garbage collection is
initially enabled. Therefore it is necessary to call this function
only if garbage collection has been explicitly disabled.")

(define-simple-managed-function peak-node-count cudd-read-peak-node-count
  "Reports the peak number of nodes.

  This number includes node on the free list. At the peak,
  the number of nodes on the free list is guaranteed to be less than
  DD_MEM_CHUNK. ")

(define-simple-managed-function peak-live-node-count cudd-read-peak-live-node-count
  "Reports the peak number of live nodes.")

(define-simple-managed-function node-count cudd-read-node-count
  "Reports the number of nodes in BDDs and ADDs.

  This number does not include the isolated projection
  functions and the unused constants. These nodes that are not counted
  are not part of the DDs manipulated by the application. ")

(define-simple-managed-function zdd-node-count cudd-zdd-read-node-count
  "Reports the number of nodes in ZDDs.

  This number always includes the two constants 1 and 0. ")

(declaim (inline set-background))
(defun set-background (bck &key (manager *manager*))
  "Sets the background constant of the manager. It assumes
that the DdNode pointer bck is already referenced."
  (declare (node-pointer bck) (manager manager))
  (cudd-set-background manager bck))

(declaim (maybe-inline count-leaves))
(defun count-leaves (node)
  "Counts the number of leaves in a DD."
  (cudd-count-leaves (node-pointer node)))

(declaim (maybe-inline count-leaves))
(defun dag-size (node)
  "Counts the number of nodes in a DD.
  NOTE: Does not require knowledge of NODE's `manager'."
  (declare (type node node))
  (etypecase node
    (zdd-node (cudd-zdd-dag-size (node-pointer node)))
    (add-node (cudd-dag-size (node-pointer node)))
    (bdd-node (cudd-dag-size (node-pointer node)))))

(define-simple-managed-function bdd-variables cudd-bdd-variables
  "Return the number of BDD variables.")

(define-simple-managed-function zdd-variables cudd-zdd-variables
  "Return the number of ZDD variables")
(define-simple-managed-function bdd-max-variables cudd-bdd-max-variables
  "Return the maximum number of BDD variables")
(define-simple-managed-function zdd-max-variables cudd-zdd-max-variables
  "Return the maximum number of ZDD variables")
