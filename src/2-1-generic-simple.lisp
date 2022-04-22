(in-package :cudd)

(export '(*bdd-zero*
          *bdd-one*
          *bdd-false*
          *bdd-true*))

;; high-level APIs that require CLOS-based dispatching for bdds and adds
;; This file contains only the simple operators

(def-cudd-call node-or ((:add (lambda (mgr f g) (cudd-add-apply mgr +or+ f g))
                         :bdd cudd-bdd-or) (f :node) (g :node))
               :generic "Disjunction of two 0-1 ADDs or two BDDs."
               :add     "Disjunction of two 0-1 ADDs."
               :bdd     "Disjunction of two BDDs.")

(def-cudd-call node-and ((:add (lambda (mgr f g) (cudd-add-apply mgr +times+ f g))
                          :bdd cudd-bdd-and) (f :node) (g :node))
               :generic "Conjunction of two 0-1 ADDs or two BDDs."
               :add     "Conjunction of two 0-1 ADDs."
               :bdd     "Conjunction of two BDDs.")

(def-cudd-call node-xor ((#|FIXME :add (lambda (mgr f g) (cudd-add-apply mgr +times+ f g))|#
                          :bdd cudd-bdd-xor) (f :node) (g :node))
               :generic "Symmetric difference of two 0-1 ADDs or two BDDs."
               :add     "Symmetric difference of two 0-1 ADDs.  (Not yet implemented!)"
               :bdd     "Symmetric difference of two BDDs.")

(def-cudd-call node-complement ((:add cudd-add-cmpl :bdd cudd-bdd-not
                                 :zdd cudd-zdd-complement)
                                (node :node))
               :generic "Computes the complement of a node a la C language:
The complement of 0 is 1 and the complement of everything else is 0."
               :add "Computes the complement of an ADD a la C language:
The complement of 0 is 1 and the complement of everything else is 0."
               :bdd "Complements a DD by flipping the complement attribute of the
pointer (the least significant bit)."
               :zdd "Complements a unate ZDD.")

(def-cudd-call if-then-else ((:add cudd-add-ite :bdd cudd-bdd-ite :zdd cudd-zdd-ite)
                             (f :node) (g :node) (h :node))
               :generic "Return a new DD-node for with F being the top-node, G being the then-branch
and H being the else branch"
               :add "Implements ITE(f,g,h). This procedure assumes that f is a 0-1 ADD."
               :bdd "Implements ITE(f,g,h)."
               :zdd "Implements ITE(f,g,h).")

(defun cube (nodes type &key ((:manager m) *manager*))
  "
A cube, or product, is a boolean product of literals.
Build a cube from a list of nodes. TYPE defines which nodes we have
in the list of nodes: ADD-NODE or BDD-NODE
"
  (declare (manager m))
  (assert* (every (lambda (n) (eq (node-manager n) m))
                  nodes))
  (let-1 mp (manager-pointer m)
    (declare (manager-pointer mp))
    (wrap-and-finalize
        (let-1 vars (map 'list #'node-pointer nodes)
          (ecase type
            (bdd-node (cudd-bdd-cube mp vars))
            (add-node (cudd-add-cube mp vars))))
        (or type (type-of (first-elt nodes)))
      :manager m)))

(defun make-var (type &key level index (manager *manager*))
  "Creates a new DD variable (projection function). At most one of index and level may be given.

If neither index nor level are given, then the new variable has an index equal
to the largest previous index plus 1.

If index is given, then retrieves the DD variable with the index if it already exists,
or creates a new DD variable.

If level is given, then the new variable has an index equal to the largest
previous index plus 1 and is positioned at the specified level in the order.

Returns a node to the new variable if successful; invokes a signal otherwise.
The returned node has the following properties depending on the type:

type = BDD-NODE: The returned node is an internal node with both outgoing arcs
pointing to the constant 1. The else arc is complemented. Constant 1 node is shared by BDD,ADD,ZDD.

 (index)-----[1]
        \.../
          (complemented arc)

type = ADD-NODE: The returned node is an internal node with THEN arc pointing to the constant 1
and ELSE arc pointing to the arithmetic zero.

 (index)--[1]
        \
         +[arithmetic 0]

type = ZDD-NODE: The returned node is the root of N+1 nodes,
where N is the maximum number of variables currently recognized by the manager.
This is because that's the way ZDD represents a projection function of a single variable.
When index = 2 and N = 4, the resulting ZDD looks as follows:

                then branch
 (root)-(0)=(1)=(2)-(3)=(4)=[1]
                |
                +----------[arithmetic 0]
                else branch"
  (declare (type ; NOTE: /Not/ '*-constant-node'
            (member bdd-node bdd-variable-node
                    add-node add-variable-node
                    zdd-node zdd-variable-node)
            type)
           (manager manager))
  (let-1 mp (manager-pointer manager)
    (declare (manager-pointer mp))

    (ecase type
      ((bdd-node bdd-variable-node)
       ;; (break "~&Constant? ~A" (cudd-node-is-constant (bdd-var mp :index index :level level)))
       (wrap-and-finalize (bdd-var mp :index index :level level) 'bdd-variable-node
         ;; var is a projection function, and its reference count is always greater
         :manager manager
         ;; than 0. Therefore, there is no call to Cudd Ref.
         :ref nil
         :var-id index))
      ((add-node add-variable-node)
       (wrap-and-finalize (add-var mp :index index :level level) 'add-variable-node
         :manager manager
         ;; The ADD projection function are not maintained by the manager. It is
         ;; therefore necessary to reference and dereference them.
         :var-id index
         :ref t))
      ((zdd-node zdd-variable-node)
       (wrap-and-finalize (zdd-var mp :index index :level level) 'zdd-variable-node
         :manager manager
         ;; The projection functions are referenced, because they are not maintained by the manager.
         :var-id index
         :ref t)))))

(defun node-then (type node &aux (manager (manager-pointer node)))
  "Return the then child of an inner node"
  (declare (node-type type)
           (manager manager))
  (assert* (not (node-constant-p node)))
  (wrap-and-finalize (cudd-node-then (node-pointer node))
      type
    :manager manager))

(defun node-else (type node &aux (manager (manager-pointer node)))
  "Return the else child of an inner node"
  (declare (node-type type)
           (manager manager))
  (assert* (not (node-constant-p node)))
  (wrap-and-finalize (cudd-node-else (node-pointer node))
      type
    :manager manager))

#|

Although the manual says that the constant 1 is same across A/B/ZDDs and
constant 0 is obtained by Cudd_ReadZero,
somehow, CUDD-READ-ZERO and CUDD-READ-ONE did not work for ADDs.

|#

;; FIXME:
(setf (find-class 'zdd-constant-node) (find-class 'zdd-node))

(declaim (maybe-inline one-node zero-node))
(defun zero-node (type &key (manager *manager*))
  "Return the zero node for the correspoinding type.
BDD: the logical zero node (boolean 0).
ADD: the arithmetic zero node (0.0d0).
ZDD: the arithmetic zero node (0.0d0). (Same as ADD)"
  (declare (node-type type)
           (manager manager))
  (let* ((mp (manager-pointer manager))
         (pointer (ecase type
                    ((bdd-node bdd-constant-node) (cudd-read-logic-zero mp))
                    ((add-node add-constant-node) (cudd-read-zero mp))
                    ((zdd-node zdd-constant-node) (cudd-read-zero mp))))
         (subtype
           (ecase type
             ((bdd-node bdd-constant-node) 'bdd-constant-node)
             ((add-node add-constant-node) 'add-constant-node)
             ((zdd-node zdd-constant-node) 'zdd-constant-node))))
    (declare (manager-pointer mp))
    (declare (node-pointer pointer))
    (wrap-and-finalize pointer subtype
      :ref t ; NOTE: Changed [2022-04-07 Thu]
      :manager manager
      :constant nil)))

(defun one-node (type &key (manager *manager*))
  "return the constant one node."
  (declare (node-type type)
           (manager manager))
  (let ((mp (manager-pointer manager)))
    (declare (manager-pointer mp))
    (wrap-and-finalize (cudd-read-one mp)
        ;; TODO: ugly:
        (ecase type
          ((bdd-node bdd-constant-node) 'bdd-constant-node)
          ((add-node add-constant-node) 'add-constant-node)
          ((zdd-node zdd-constant-node) 'zdd-constant-node))
      :ref t ; NOTE: Changed [2022-04-07 Thu]
      :manager manager
      :constant t)))

;;; Synsugar:
(define-symbol-macro *bdd-zero* (zero-node 'bdd-constant-node))
(define-symbol-macro *bdd-one* (one-node 'bdd-constant-node))

(define-symbol-macro *bdd-false* *bdd-zero*)
(define-symbol-macro *bdd-true* *bdd-one*)
