;;; base class definitions and macros for defining APIs
(in-package :cudd)

(defvar config/enable-gc t
  "When true, new nodes get equipped with finalizers.")

(defvar config/debug-memory-errors nil)

(defvar config/debug-consistency-checks nil
  "TODO: Disable by default if assertions are turned off.")

(export '(config/enable-gc
          cudd-logger
          config/debug-memory-errors
          config/debug-consistency-checks
          ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +finalizer-log-level+ :debu6
    "FIXME: Unused.")
  (with-package-log-hierarchy
    (defvar cudd-logger (make-logger))))

#|
(progn
(shadowing-import 'cudd:cudd-logger)
(assert (bound? 'cudd:cudd-logger))
(log:config cudd:cudd-logger :debu6))
|#

(assert (fboundp 'with-cudd-critical-section))
(assert (fboundp 'log-error))
(assert (boundp '*stderr*))

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

  * TODO: Support kwargs
  * TODO: Kwarg to disable (with-cudd-critical-section)
"
  ;; TODO:
  (declare (optimize debug))
  (declare (foreign-pointer pointer)
           ((member bdd-node add-node zdd-node) type))
  (with-cudd-critical-section
    (let ((address (cffi:pointer-address pointer)))
      (ensure-gethash
       address
       (manager-node-hash *manager*)
       (progn
         (log:debu6 :logger cudd-logger "- Constructing wrapper node for ~A.  REFs: ~D." pointer
                    (cudd-node-ref-count pointer))

         (when ref
           (cudd-ref pointer))
         (let ((node (ecase type
                       (bdd-node (make-bdd-node :pointer pointer))
                       (add-node (make-add-node :pointer pointer))
                       (zdd-node (make-zdd-node :pointer pointer)))))
           (when config/enable-gc
             (when ref
               (finalize
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
                    (let ((cur-address (pointer-address pointer)))
                      (assert (eql address cur-address))

                      (log:debu6 :logger cudd-logger "- Finalizing node for ~A.  REFs: ~D" pointer ;;cur-address
                                 (cudd-node-ref-count pointer)
                                 ))

                    (handler-case
                        (let ((mp (manager-pointer manager)))

                          (with-cudd-critical-section
                            (when config/debug-consistency-checks
                              (unless (zerop (cudd-check-keys mp))
                                (log-error :logger cudd-logger "Assert 1 failed: (zerop (cudd-check-keys mp)) at start of finalizer"))
                              (unless (zerop (cudd-debug-check mp))
                                (log-error :logger cudd-logger "Assert 2 failed: (zerop (cudd-debug-check mp)) at start of finalizer")))

                            (when (zerop (cudd-node-ref-count pointer))
                              ;; TODO: Hopefully releases the mutex?:
                              (error "Tried to decrease reference count of node that already has refcount zero"))
                            (ecase type
                              (bdd-node (cudd-recursive-deref mp pointer))
                              (add-node (cudd-recursive-deref mp pointer))
                              (zdd-node (cudd-recursive-deref-zdd mp pointer)))

                            (when config/debug-consistency-checks
                              (unless (zerop (cudd-check-keys mp))
                                (log-error :logger cudd-logger "Assert 3 failed at end of finalizer: ~A" '(zerop (cudd-check-keys mp))))
                              (unless (zerop (cudd-debug-check mp))
                                (log-error :logger cudd-logger "Assert 4 failed at end of finalizer: ~A" '(zerop (cudd-debug-check mp)))))))

                      ;; TODO: Remove reliance on #+sbcl :
                      #+sbcl (sb-sys:memory-fault-error (xc)
                               (cond
                                 (config/debug-memory-errors
                                  (log-error :logger cudd-logger "* Memory-fault caught: '~A'
 Re-throwing." xc
 #|(slot-value xc 'sb-kernel::address)
 (slot-value xc 'sb-kernel::context)|#)
                                  (error xc))
                                 (t
                                  ;; suppress error
                                  )))))))))
           (assert (let ((mp (manager-pointer *manager*)))
                     (when config/debug-consistency-checks
                       (with-cudd-critical-section
                         (unless (zerop (cudd-check-keys mp))
                           (log-error :logger cudd-logger "Assert 5 failed: during (wrap-and-finalize): ~A" '(zerop (cudd-check-keys mp))))
                         (unless (zerop (cudd-debug-check mp))
                           (log-error :logger cudd-logger "Assert 6 failed: during (wrap-and-finalize): ~A with MP=~A"
                                      '(zerop (cudd-debug-check mp))
                                      mp))))
                     t))
           node))))))

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
