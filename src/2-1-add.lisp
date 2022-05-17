;;;  Algebraic Decision Diagrams specific codes

(in-package :cudd)

(declaim (reentrant add-negate))
(defun add-negate (node)
  "Computes the additive inverse of an ADD."
  (let-1 manager (node-manager node)
      (declare (manager manager))
    (wrap-and-finalize
     (etypecase node
       (add-node (cudd-add-negate (manager-pointer manager) (node-pointer node))))
     'add-node
     :manager manager)))

(declaim (reentrant add-constant))
(defun add-constant (value &key (manager *manager*))
  "Retrieves the ADD for constant c if it already exists, or creates a new ADD."
  (declare (manager manager))
  (wrap-and-finalize
   (cudd-add-const (manager-pointer manager) value)
   'add-constant-node
   :manager manager))

(declaim (reentrant plus-infinity)
         (reentrant minus-infinity))
(defun plus-infinity (&key (manager *manager*))
  "Returns a node with value infinity."
  (declare (manager manager))
  ;; (let ((node (add-node (cudd-read-plus-infinity (manager-pointer manager)) :manager manager)))
  ;;   (declare (type add-constant-node node))
  ;;   node)
  (wrap-and-finalize (cudd-read-plus-infinity (manager-pointer manager)) 'add-constant-node
                     :manager manager
                     :ref nil ; because these nodes are predefined constants.
                     ))
(defun minus-infinity (&key (manager *manager*))
  "Returns a  node with value -infinity."
  (declare (manager manager))
  ;; (let ((node (add-node (cudd-read-minus-infinity (manager-pointer manager)) :manager manager)))
  ;;   (declare (type add-constant-node node))
  ;;   node)
  (wrap-and-finalize (cudd-read-minus-infinity (manager-pointer manager)) 'add-constant-node
                     :manager manager
                     :ref nil ; because these nodes are predefined constants.
                     ))

(declaim (reentrant epsilon))
(defun epsilon (&key (manager *manager*))
  "Returns a  node with value infinity."
  (declare (manager manager))
  ;; (let ((node (add-node (cudd-read-epsilon (manager-pointer manager)) :manager manager)))
  ;;   (declare (type add-constant-node node))
  ;;   node)
  (wrap-and-finalize (cudd-read-epsilon (manager-pointer manager)) 'add-constant-node
                     :manager manager
                     :ref nil ; because these nodes are predefined constants.
                     ))


;;; Functions for add-apply
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *add-operators* nil)
  (defun probe-symbol (name sym)
    (let ((fsym (cffi:foreign-symbol-pointer name)))
      (if fsym
          (pushnew (cons sym name) *add-operators* :test #'equalp)
          (format *error-output* "~&C Symbol ~a for ~a was not found~&" name sym))
      fsym))

  ;; some of these symbols are obsoleted; see *add-operators* and the docstring of add-apply
  (defparameter +plus+ (probe-symbol "Cudd_addPlus" '+plus+)
    "Integer and floating point addition")
  (defparameter +times+ (probe-symbol "Cudd_addTimes" '+times+)
    "Integer and floating point multiplication.")
  (defparameter +threshold+ (probe-symbol "Cudd_addThreshold" '+threshold+)
    "Threshold operator for Apply (f if f >=g; 0 if f<g)")
  (defparameter +set-NZ+ (probe-symbol "Cudd_addSetNZ" '+set-NZ+)
    "This operator sets f to the value of g wherever g != 0.")
  (defparameter +divide+ (probe-symbol "Cudd_addDivide" '+divide+)
    "Integer and floating point division.")
  (defparameter +minus+ (probe-symbol "Cudd_addMinus" '+minus+)
    "Integer and floating point substraction.")
  (defparameter +minimum+ (probe-symbol "Cudd_addMinimum" '+minimum+)
    "Integer and floating point minimum.")
  (defparameter +maximum+ (probe-symbol "Cudd_addMaximum" '+maximum+)
    "Integer and floating point maximum.")
  (defparameter +one-zero-maximum+ (probe-symbol "Cudd_addOneZeroMaximum" '+one-zero-maximum+)
    "1 if f > g and 0 otherwise.")
  (defparameter +diff+ (probe-symbol "Cudd_addDiff" '+diff+)
    "f op g , where f op g is plusinfinity if f=g; min(f,g) if f!=g.")
  (defparameter +agreement+ (probe-symbol "Cudd_addAgreement" '+agreement+)
    "f op g,  where f op g is f if f==g; background if f!=g.")
  (defparameter +or+ (probe-symbol "Cudd_addOr" '+or+)
    "Disjunction of two 0-1 ADDs.")
  (defparameter +and+ (probe-symbol "Cudd_addAnd" '+and+)
    "Conjunction of two 0-1 ADDs.")
  (defparameter +nand+ (probe-symbol "Cudd_addNand" '+nand+)
    "NAND of two 0-1 ADDs.")
  (defparameter +nor+ (probe-symbol "Cudd_addNor" '+nor+)
    "NOR of two 0-1 ADDs.")
  (defparameter +xor+ (probe-symbol "Cudd_addXor" '+xor+)
    "XOR of two 0-1 ADDs.")
  (defparameter +xnor+ (probe-symbol "Cudd_addXnor" '+xnor+)
    "XNOR of two 0-1 ADDs.")
  (defparameter +equals+ (probe-symbol "Cudd_addEquals" '+equals+)
    "f op g, where f op g is 1 if f==g, 0 otherwise")
  (defparameter +not-equals+ (probe-symbol "Cudd_addNotEquals" '+not-equals+)
    "f op g, where f op g is 1 if f!=g; 0 otherwise")
  (defparameter +greater-than+ (probe-symbol "Cudd_addGreaterThan" '+greater-than+)
    "f > g, where f op g is 1 if f!=g; 0 otherwise")
  (defparameter +greater-than-equals+ (probe-symbol "Cudd_addGreaterThanEquals" '+greater-than-equals+)
    "1 if f >= g, 0 otherwise")
  (defparameter +less-than+ (probe-symbol "Cudd_addLessThan" '+less-than+)
    "f < g, where f op g is 1 if f!=g; 0 otherwise")
  (defparameter +less-than-equals+ (probe-symbol "Cudd_addLessThanEquals" '+less-than-equals+)
    "f <= g, where f op g is 1 if f!=g; 0 otherwise")
  (defparameter +pow+ (probe-symbol "Cudd_addPow" '+pow+)
    "f to the power of g")
  (defparameter +mod+ (probe-symbol "Cudd_addMod" '+mod+)
    "f modulo g")
  (defparameter +log-x-y+ (probe-symbol "Cudd_addLogXY" '+log-x-y+)
    "log f base g"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *add-apply-doc*
    (format nil "Applies op to the corresponding discriminants of f and g.

The following operations are supported:

~{~A~%~}" (mapcar (lambda (op)
                    (destructuring-bind (lisp-name . c_name) op
                      (assert (boundp lisp-name))
                      (format nil "~a (originally ~a) -- ~a"
                              lisp-name c_name
                              (documentation lisp-name 'variable))))
                  *add-operators*))))

(declaim (reentrant add-apply))
(defun add-apply (op f g)
  #.*add-apply-doc*
  (let-1 manager (node-manager f)
    (declare (manager manager))
    (assert* (eq manager (node-manager g)))
    (wrap-and-finalize
     (cudd-add-apply (manager-pointer manager)
                     op
                     (node-pointer f)
                     (node-pointer g))
     'add-node
     :manager manager)))
