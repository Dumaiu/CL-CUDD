;;; base class definitions and macros for defining APIs
(in-package :cudd)

(defvar config/enable-gc t
  "When true, new nodes are equipped with finalizers.")

(export 'config/enable-gc)

(assert (fboundp 'with-cudd-critical-section))

(defun required ()
  (error "Required slot"))
;;; Wrapped CUDD node
(defstruct node
  "A boxed CUDD node class. Top class of all CUDD nodes."
  (pointer (required) :type cffi:foreign-pointer))

(declaim (inline wrap-and-finalize))
(defun wrap-and-finalize (pointer type &optional (ref t))
  "Wrap the given pointer in a node of type TYPE.
If a node for the same pointer is already in the lisp image, it is reused.
Otherwise, a new node object is instantiated.

When a new lisp node is created, we call cudd-ref on the given pointer.
We also set a finalizer for the node
which calls cudd-recursive-deref on the pointer when the lisp node is garbage collected.
"
  (declare (foreign-pointer pointer)
		   ((member bdd-node add-node zdd-node) type))
  (with-cudd-critical-section
	(ensure-gethash
	 (cffi:pointer-address pointer)
	 (manager-node-hash *manager*)
	 (progn
	   (when ref
		 (cudd-ref pointer))
	   (let ((node (ecase type
					 (bdd-node (make-bdd-node :pointer pointer))
					 (add-node (make-add-node :pointer pointer))
					 (zdd-node (make-zdd-node :pointer pointer)))))
		 (when config/enable-gc
		   (when ref
			 (trivial-garbage:finalize
			  node
			  (let ((manager *manager*))
				;; NOTE: ^^^ This holds the reference from the __finalizer function__ to
				;; the manager (along with avoiding problems related to dynamic binding).
				;; Without it, the finalizer may be called after the manager is finalized
				;; (i.e. cudd-quit is called), invalidating the pointer to the node.
				;; It is insufficient to reference a manager from a node, since the order
				;; to call finalizers is unspecified. If a manager and a node is freed in
				;; the same gc, it could be possible that cudd-quit is called
				;; first. Manager object should be referenced until the node finalizer
				;; is called.
				(lambda ()
				  (let ((mp (manager-pointer manager)))
					;; (log:info "Finalizing node.")
					(with-cudd-critical-section
					  (when (zerop (cudd-node-ref-count pointer))
						;; TODO: Hopefully releases the mutex?:
						(error "Tried to decrease reference count of node that already has refcount zero"))
					  (ecase type
						(bdd-node (cudd-recursive-deref mp pointer))
						(add-node (cudd-recursive-deref mp pointer))
						(zdd-node (cudd-recursive-deref-zdd mp pointer))))))))))
		 node)))))

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type (type-of object) :identity nil)
	(format stream "INDEX ~A " (cudd-node-read-index (node-pointer object)))
	(if (node-constant-p object)
		(format stream "LEAF (VALUE ~A)" (node-value object))
		(format stream "INNER 0x~x" (pointer-address (node-pointer object))))
	(format stream " REF ~d"
			(cudd-node-ref-count (node-pointer object)))))

(declaim (inline node-index
				 node-equal
				 node-constant-p
				 node-value))

(defun node-index (node)
  (cudd-node-read-index (node-pointer node)))

(defun node-equal (a b)
  "Return true iff A and B are the same graph.

Because CUDD caches all diagrams, this is true if and
only if their pointers are the same."
  (check-type a node)
  (check-type b node)
  (cffi:pointer-eq (node-pointer a) (node-pointer b)))

(defun node-constant-p (node)
  "return t if the node is constant, nil otherwise"
  (cudd-node-is-constant (node-pointer node)))

(defun node-value (node)
  "Return the node value of a constant node"
  ;; Make sure that we only try to read the value of a constant node
  (assert (node-constant-p node))
  (cudd-node-value (node-pointer node)))

(defstruct (bdd-node (:include node))
  "Node of a binary decision diagram (BDD)")

(defstruct (add-node (:include node))
  "Node of an algebraic decision diagram (ADD)")

(defstruct (zdd-node (:include node))
  "Node of an zero-suppressed decision diagram (ZDD)")

(deftype node-type ()
  `(member bdd-node add-node zdd-node))


(assert (not (eq 'cudd-T 'cl-cudd.baseapi:cudd-T)))
(defun cudd-T (node)
  "Evaluate the 'then' branch.  Undefined if NODE is not a branch node!"
  (check-type node bdd-node)
  (let ((res (wrap-and-finalize (cl-cudd.baseapi:cudd-T (node-pointer node)) 'bdd-node)))
	(declare (bdd-node res))
	res))


(assert (not (eq 'cudd-E 'cl-cudd.baseapi:cudd-E)))
(defun cudd-E (node)
  "Evaluate the 'else' branch.  Undefined if NODE is not a branch node!"
  (check-type node bdd-node)
  (let ((res (wrap-and-finalize (cl-cudd.baseapi:cudd-E (node-pointer node)) 'bdd-node)))
	(declare (bdd-node res))
	res))
