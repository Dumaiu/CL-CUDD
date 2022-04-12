;;; base class definitions and macros for defining APIs
(in-package :cudd)


(assert (fboundp 'with-cudd-critical-section))
(assert (fboundp 'log-error))
(assert (boundp '*stderr*))

(export '(
          add-node-type
          bdd-node-type
          zdd-node-type
          node-type
          generalized-bit
          variable-node
          bdd-constant-node
          bdd-variable-node
          index
          bdd-variable-index
          node-pointer
          node-manager
          ))

(with-package-log-hierarchy
  (defvar cudd-node-logger (make-logger)
    ":log4cl logger created in '2-0-1-node.lisp'.

  - Independent of `cudd-logger'.
  * TODO: Make this a child of `cudd-logger'?  How do I specify that?

  - NOTE: Setting this to :trace or higher will slow CUDD way down by logging all `bdd-node' construction|finalization funcalls.
"))
(declaim (special cudd-node-logger))

;; (log4cl:hierarchy-index (find-package :cudd))
;; (log4cl:hierarchy-index cudd:cudd-logger )

(deftype generalized-bit ()
  '(or boolean (member 0 1)))

(deftype add-node-type ()
  '(member add-node add-constant-node add-variable-node))

(deftype bdd-node-type ()
  '(member bdd-node bdd-constant-node bdd-variable-node))

(deftype zdd-node-type ()
  '(member zdd-node zdd-constant-node zdd-variable-node))

(deftype node-type ()
  "One of the three node types or their subclasses."
  '(or add-node-type
    bdd-node-type
    zdd-node-type))


(defun required (&optional name)
  (error "Required slot~[~;: ~S~]" (if name 1 0) name))

;; (defstruct node
;;   "A boxed CUDD node class. Top class of all CUDD nodes."
;;   (pointer (required) :type node-pointer))

;; (declaim (inline node-manager)) ; TODO: Static method
(defclass node ()
  ((pointer :initform (required 'pointer)
            :initarg :pointer
            :type node-pointer
            #|:accessor node-pointer|#)
   (manager :initform *manager*
            :initarg :manager
            :type manager
            :accessor manager
            #|:accessor node-manager
            TODO: My ideal would be something akin to:

            :accessor (manager :inline t)

            |#))
  (:documentation "Wrapped CUDD node.
  - [2022-03-30 Wed] Added a `manager' reference.
  - TODO: Static generic funcs.  (node-pointer) and (node-manager) are already non-generic inlined readers.
"))

;; id=ctor
(defmethod initialize-instance :after ((node node) &key &allow-other-keys)
  "TODO: Undefine on max [opt] settings."
  (check-type (slot-value node 'pointer) node-pointer)
  (check-type (slot-value node 'manager) manager))

(declaim (inline node-pointer
                 node-manager))
(defun node-pointer (node)
  (declare (node node))
  (the node-pointer (slot-value node 'pointer)))

(defun node-manager (node)
  (declare (node node))
  (the manager (slot-value node 'manager)))

(declaim (inline keys-check?
                 debug-check?))
(defun keys-check? ()
  (or (eq t config/debug-consistency-checks)
      (eq :keys config/debug-consistency-checks)))
(defun debug-check? ()
  (or (eq t config/debug-consistency-checks)
      ;; (member :check-keys config/debug-consistency-checks :test #'eq)
      (eq :debug config/debug-consistency-checks)))

(defun helper/destruct-node (node-pointer node-type
                             &key
                               manager
                               ((:ref ref) t ref-provided?))
  "NB: We *do* want to maintain a reference to the MANAGER from within a node's finalizer.
  MANAGER: Maintain a reference from the node's finalizer to its manager so there is no chance of dangling-pointer errors.  See M. Asai's note in (wrap-and-finalize).
  REF: When T, decrement CUDD ref.  Passed along from (wrap-and-finalize): if we didn't increment during construction, we don't decrement here.
"
  ;; (declare (optimize safety))
  (declare (node-pointer node-pointer)
           (manager manager)
           (boolean ref))
  (unless (null ref-provided?)
    ;; (log-warn :logger cudd-node-logger )
    (warn "Don't pass :ref."))

  (macrolet ((with-mem-fault-protection (&body body)
               "Establish a block to handle a memory fault.  Optionally resume execution, depending on `config/signal-memory-errors'.
  * TODO: Rewrite with (handler-case)?
  * TODO [2022-03-25 Fri] Rewrite (handler-bind-case) to do the (throw)
"
               `(catch 'mem-fault-suppress
                  (handler-bind-case ; for sb-sys:memory-fault-error
                   (progn ,@body)

                   ;; TODO: Remove reliance on '#+sbcl':
                   #+sbcl (sb-sys:memory-fault-error (xc)
                                                     (ecase config/signal-memory-errors
                                                       ((:error :log)
                                                        (let+ (((&accessors-r/o ;manager-pointer
                                                                 manager-index)
                                                                manager)
                                                               (node-string (print-node-to-string node-pointer :type node-type))
                                                               ) ;((manager-string (princ-to-string manager)))
                                                          (declare (string node-string))

                                                          (log-error :logger cudd-node-logger
                                                                     ;; TODO: Give each node an index?
                                                                     "* Error: memory-fault detected in Lisp:
 ~&~T~<~A~>

while destructing
~T~A
in manager #~A.

 Re-throwing? ~A~%"
                                                                     xc
                                                                     node-string
                                                                     manager-index ;manager-string
                                                                     (eq :error config/signal-memory-errors)))

                                                        (if (eq :error config/signal-memory-errors)
                                                            (cerror "Ignore and hope for the best" xc)
                                                            (assert* (eq :log config/signal-memory-errors))))

                                                       ((nil) #| Silence |#))
                                                     ;; Continue:
                                                     (throw 'mem-fault-suppress nil))))))
    (let ((keys-check? (keys-check?))
          (debug-check? (debug-check?)))
      (declare (boolean keys-check? debug-check?))

      (with-cudd-critical-section (:manager manager)
        (let ((mp (manager-pointer manager)))
          (declare (manager-pointer mp))

          (log-msg :trace :logger cudd-node-logger
                   "~2&~T Finalizer for ~A ~A.  REFs: ~D"
                   node-type
                   node-pointer ;;cur-address
                   (cudd-node-ref-count node-pointer))

          (when config/debug-consistency-checks
            (with-mem-fault-protection
                (when keys-check?
                  (unless (zerop (cudd-check-keys mp))
                    (let ((manager-string (princ-to-string manager)))
                      (log-error :logger cudd-node-logger "~&Assert 1 failed: (zerop (cudd-check-keys mp)) at start of finalizer of ~A ~A
in manager ~A~%"
                                 node-type
                                 node-pointer
                                 manager-string)))))
            (with-mem-fault-protection
                (when debug-check?
                  (unless (zerop (cudd-debug-check mp))
                    (let ((manager-string (princ-to-string manager)))
                      (log-error :logger cudd-node-logger "~&Assert 2 failed: (zerop (cudd-debug-check mp)) at start of finalizer of ~A ~A
in manager ~A~%"
                                 node-type
                                 node-pointer
                                 manager-string))))))

          (with-mem-fault-protection
              (cond
                (ref
                 (log-msg :debu8 :logger cudd-node-logger
                          "Reached the deref segment in finalizer for ~A ~A." node-type node-pointer)

                 (when (zerop (cudd-node-ref-count node-pointer))
                   (error "Tried to decrease reference count of node that already has refcount zero"))

                 (etypecase node-type
                   (bdd-node-type (cudd-recursive-deref mp node-pointer))
                   (add-node-type (cudd-recursive-deref mp node-pointer))
                   (zdd-node-type (cudd-recursive-deref-zdd mp node-pointer)))

                 (log-msg :debu7 :logger cudd-node-logger "- After (cudd-recursive-deref ~A), REFs = ~D."
                          node-pointer
                          (cudd-node-ref-count node-pointer))

                 (log-msg :debu8 :logger cudd-node-logger
                          "Past the deref segment in finalizer for ~A ~A." node-type node-pointer))
                (t ; ref=nil
                 (log-msg :debu8 :logger cudd-node-logger
                          "Skipping the deref segment in finalizer for ~A ~A." node-type node-pointer))))

          (when config/debug-consistency-checks
            (with-mem-fault-protection
                (when keys-check?
                  (unless (zerop (cudd-check-keys mp))
                    (log-error :logger cudd-node-logger "~&Assert 3: ~&~T~A ~&failed at end of finalizer for
 ~T~A
 in ~A~%"
                               '(zerop (cudd-check-keys mp))
                               node-pointer
                               mp))))
            (with-mem-fault-protection
                (when debug-check?
                  (unless (zerop (cudd-debug-check mp))
                    (log-error :logger cudd-node-logger "~&Assert 4 failed at end of finalizer: ~A" '(zerop (cudd-debug-check mp))))))))

        (log-msg :debu8 :logger cudd-node-logger "Reached the end of finalizer for ~A ~A." node-type node-pointer)
        t))))

(defun helper/construct-node (pointer type ref manager other-initargs)
  "Used by (wrap-and-finalize)."
  (declare (node-pointer pointer)
           (node-type type)
           (boolean ref)
           (manager manager)
           (list other-initargs))

  (let ((keys-check? (keys-check?))
        (debug-check? (debug-check?)))
    (declare (boolean keys-check? debug-check?))
    (progn;; let ((address (pointer-address pointer)))
      ;;  (declare (ignorable address))

      (with-cudd-critical-section (:manager manager)
        ;; If ref=T, increment the CUDD ref count
        (cond
          (ref
           (log-msg :trace :logger cudd-node-logger
                    "Constructing wrapper node for ~A ~A.  Before incrementing, REFs = ~D."
                    type
                    pointer
                    (cudd-node-ref-count pointer))

           ;; *Side-effect*:
           (cudd-ref pointer)

           (log-msg :debu7 :logger cudd-node-logger "- After (cudd-ref ~A), REFs = ~D."
                    pointer
                    (cudd-node-ref-count pointer)))

          ('otherwise  ; ref=nil
           (log-msg :trace :logger cudd-node-logger "NON-INCREMENTING wrapper for ~A ~A being constructed (REFs = ~D).
 This should happen only for literals."
                    type
                    pointer
                    (cudd-node-ref-count pointer))

           ;; (break "Non-incrementing type: ~A" type)
           (assert* (subtypep type 'variable-node))

           #|(let ((initial-ref-count (cudd-node-ref-count pointer)))
           (declare (fixnum initial-ref-count)) ;
           (assert (>= initial-ref-count 1)) ;
           (unless (= 1 initial-ref-count) ;
           (log-msg :warn :logger cudd-node-logger "Ref count of literal node ~A is ~D, which is > 1" ;
           pointer     ;
           initial-ref-count)))|#))

        (let-1 node (apply #'make-instance type :pointer pointer :manager manager
                           other-initargs)
          ;; let ((node #.(let ((ctor-args '(:pointer pointer)))
          ;;                `(ecase type
          ;;                   (bdd-node (make-bdd-node ,@ctor-args))
          ;;                   (add-node (make-add-node ,@ctor-args))
          ;;                   (zdd-node (make-zdd-node ,@ctor-args))))))

          ;; Construct finalizer for NODE:
          (when config/enable-gc
            (let ((manager manager #|née *manager*|#))
              ;; NOTE: ^^^ This holds the reference from the __finalizer function__ to
              ;; the manager (along with avoiding problems related to dynamic binding).
              ;; Without it, the finalizer may be called after the manager is finalized
              ;; (i.e. cudd-quit is called), invalidating the pointer to the node.
              ;; It is insufficient to reference a manager from a node, since the order
              ;; to call finalizers is unspecified. If a manager and a node is freed in
              ;; the same gc, it could be possible that cudd-quit is called
              ;; first. Manager object should be referenced until the node finalizer
              ;; is called.
              (finalize
               node
               (lambda () ;; id=finalizer
                 "Closure for finalizing a cudd-node."
                 (helper/destruct-node pointer
                                       type
                                       :manager manager
                                       :ref ref)))))

          ;; After constructing the finalizer:
          (when config/debug-consistency-checks
            (with-cudd-critical-section (:manager manager)
              (let ((mp (manager-pointer manager)))

                (when keys-check?
                  #.(let ((test-5 '(zerop (cudd-check-keys mp))))
                      `(unless ,test-5
                         (log-error :logger cudd-node-logger "~&Assert 5 failed: during (helper/construct-node): ~A" ',test-5))))

                (when debug-check?
                  #.(let ((test-6 '(zerop (cudd-debug-check mp))))
                      `(unless ,test-6
                         (log-error :logger cudd-node-logger "~&Assert 6 failed: during (helper/construct-node): ~A with MP=~A"  ',test-6  mp)))))))
          node)))))

(defmacro wrap-and-finalize (pointer type &rest *other-initargs &key
                                                                  (ref t)
                                                                  (manager '*manager*)
                             &allow-other-keys)
  "Wrap the given pointer in a node of type TYPE.
If a node for the same pointer is already in the lisp image, it is reused.
Otherwise, a new node object is instantiated.

When a new lisp node is created, we call cudd-ref on the given pointer.
We also set a finalizer for the node
which calls cudd-recursive-deref on the pointer when the lisp node is garbage collected.

  * TODO: Support kwargs
  * TODO: Kwarg to disable (with-cudd-critical-section)
"
  ;; (declare (optimize debug))

  ;; NOTE: I can't use (alexandria:once-only) here.  It isn't "`declare'-friendly":
  `(let ((manager ,manager))
     (declare (manager manager))

     (with-cudd-critical-section (:manager manager)
       (let* ((pointer ,pointer)
              (type ,type)
              (ref ,ref)
              (address (pointer-address pointer)))
         (declare (node-pointer pointer)
                  (node-type type)
                  (boolean ref))

         (ensure-gethash
          address
          (manager-node-hash manager)
          ;; This form executes iff ADDRESS isn't already present in the hashtable:
          (helper/construct-node pointer type ref manager (list ,@*other-initargs)))))))

(defun node (pointer type &key (ref t) (manager *manager*))
  (declare (node-pointer pointer)
           (node-type type)
           (boolean ref)
           (manager manager))
  ;; TODO: Adjust value of REF based on TYPE ?
  (wrap-and-finalize pointer type :ref ref :manager manager))

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

(defclass constant-node (node) ())

(defclass variable-node (node) ()
  (:documentation "ABC."))

;; (defclass literal-node () ())

(declaim (inline make-bdd-node
                 make-add-node
                 make-zdd-node))

(defclass bdd-node (node) ()
  (:documentation "Node of a binary decision diagram (BDD)"))
;; (defstruct (bdd-node (:include node))
;;   "Node of a binary decision diagram (BDD)")

(defun make-bdd-node (&rest args)
  (apply #'make-instance 'bdd-node args))

(defclass bdd-constant-node (bdd-node constant-node)
  ((constant :type boolean :initform (required ':constant)
             :reader constant
             :initarg :constant))
  (:documentation "A 0 or 1 literal."))

;; (defmethod literal ((node bdd-constant-node))
;;   (let+ ((&slots-r/o pointer) )))

;; (defmethod print-object ((node bdd-constant-node) stream)
;;   (format stream "#BDD<~A>" (node-value node)))

;; (deftype nat ()
;;   'non-negative-fixnum)

(defclass bdd-variable-node (bdd-node variable-node)
  ((index :type non-negative-fixnum
          :initform (required 'index)
          :initarg :index
          :reader index
          #| TODO :reader (bdd-variable-index :inline t) |#
          ))
  (:documentation "A BDD variable literal."))

(declaim (inline bdd-variable-index))
(defun bdd-variable-index (bdd-variable-node)
  (declare (bdd-variable-node bdd-variable-node))
  (the non-negative-fixnum (slot-value bdd-variable-node 'index)))

(defun bdd-node (pointer &key (manager *manager*))
  (declare (node-pointer pointer))
  (wrap-and-finalize pointer 'bdd-node
                     ;; :ref (not (cudd-node-is-constant pointer))
                     :manager manager))

;; (defmethod print-object ((node bdd-variable-node) stream)
;;   (let-1 (index (node-index node))
;;       (format stream "#BDD<variable ~D>" index)))

(defclass add-node (node) ()
  (:documentation "Node of an algebraic decision diagram (ADD)"))

(defclass add-constant-node (add-node constant-node) ()
  (:documentation "Unlike with the other ?DD types, users may make new ADD constants."))

(defclass add-variable-node (add-node variable-node) ()
  #|(FIXME)|#)

(defun make-add-node (&rest args)
  (apply #'make-instance 'add-node args))

(defun add-node (pointer &key (manager *manager*))
  (declare (node-pointer pointer))
  (wrap-and-finalize pointer 'add-node :ref (not (cudd-node-is-constant pointer))
                                       :manager manager))

(defclass zdd-node (node) ()
  (:documentation "Node of an zero-suppressed decision diagram (ZDD)"))

(defun make-zdd-node (&rest args)
  (apply #'make-instance 'zdd-node args))

(assert (not (eq 'cudd-T 'cl-cudd.baseapi:cudd-T)))
(defun cudd-T (node)
  "Evaluate the 'then' branch.  Undefined if NODE is not a branch node!"
  (declare (bdd-node node))
  ;; (check-type node bdd-node)
  (let ((res (wrap-and-finalize (cl-cudd.baseapi:cudd-T (node-pointer node)) 'bdd-node
                                :manager (node-manager node))))
    (declare (bdd-node res))
    res))


(assert (not (eq 'cudd-E 'cl-cudd.baseapi:cudd-E)))
(defun cudd-E (node)
  "Evaluate the 'else' branch.  Undefined if NODE is not a branch node!"
  (declare (bdd-node node))
  ;; (check-type node bdd-node)
  (let ((res (wrap-and-finalize (cl-cudd.baseapi:cudd-E (node-pointer node)) 'bdd-node
                                :manager (node-manager node))))
    (declare (bdd-node res))
    res))

(declaim (maybe-inline print-pointer-as-string/helper)
         (notinline print-pointer-as-string/helper))
(defgeneric print-pointer-as-string/helper (pointer type &key)
  (:documentation "NOTE: Error if *PRINT-READABLY* is set.")

  (:argument-precedence-order type pointer)

  (:method :before (node type &key &allow-other-keys)
    (when *print-readably* (not-implemented-error 'readable-cudd-node-representation)))

  (:method :around (pointer type &rest keys
                    &key (manager *manager*)
                    &allow-other-keys)
    "TODO: What does printing the 'index' do?"
    (declare (type node-type type)
             (manager manager))
    (with-output-to-string (stream)
      (print-unreadable-object (pointer stream :type type :identity t)
        ;; (format stream "INDEX ~A " (cudd-node-read-index pointer))
        (princ (apply #'call-next-method pointer type :nested t
                      keys)
               stream)
        (format stream ", LEAF (VALUE ~A)" (cudd-node-value pointer))
        (format stream ", REF ~d" (cudd-node-ref-count pointer))
        (format stream ", MANAGER #~D" (manager-index manager)))))

  (:method (pointer (type (eql 'bdd-variable-node))
            &key nested ; security
              index
            &allow-other-keys)
    (declare (node-pointer pointer)
             (boolean nested)
             (type integer index))
    (assert* nested)
    (with-output-to-string (stream)
      (format stream ", #~A" index)))

  (:method (pointer (type (eql 'bdd-constant-node))
            &key nested
              constant
            &allow-other-keys)
    (declare (node-pointer pointer)
             (boolean nested)
             (boolean constant))
    (assert* nested)
    ;; (assert* constant)
    (assert* (cudd-node-is-constant pointer))
    (with-output-to-string (stream)
      (format stream "~A" constant)))

  (:method (pointer type &key nested &allow-other-keys)
    "Base case.  May not be called."
    (declare (node-type type))
    (assert* (eq t nested)))
  ); (print-pointer-as-string/helper)


(declaim (maybe-inline print-node-to-string))
(defgeneric print-node-to-string (node &key)
  (:method ((node node) &key &allow-other-keys)
    "Recurse."
    (print-node-to-string (the node-pointer (node-pointer node))
                          :type (type-of node)
                          :manager (node-manager node)))

  (:method ((node bdd-constant-node) &key)
    (let-1 pointer (node-pointer node)
      (assert* (cudd-node-is-constant pointer))
      (with-output-to-string (stream)
        (print-unreadable-object (node stream :type 'bdd-constant-node :identity nil)
          (format stream "~A" (constant node))
          ;; (format stream "INDEX ~A " (cudd-node-read-index pointer))
          (format stream ", LEAF (VALUE ~A)" (cudd-node-value pointer))
          (format stream ", REF ~d"
                  (cudd-node-ref-count pointer))
          (format stream ", MANAGER #~D"
                  (manager-index (node-manager node)))))))

  ;; (:method ((node bdd-variable-node) &key)
  ;;   (let-1 pointer (node-pointer node)
  ;;     (declare (node-pointer pointer))
  ;;     (print-unreadable-object (node nil :type 'bdd-variable-node
  ;;                                        :identity t))
  ;;     XXX))

  (:method ((pointer #.(typexpand 'node-pointer))
            &key
              (type 'node)
              (manager *manager*))
    (declare (node-pointer pointer)
             (manager manager))
    (with-output-to-string (stream)
      (print-unreadable-object (pointer stream :type type :identity nil)
        (format stream "INDEX ~A " (cudd-node-read-index pointer))
        (if (cudd-node-is-constant pointer)
            (format stream "LEAF (VALUE ~A)" (cudd-node-value pointer))
            (format stream "INNER 0x~x" (pointer-address pointer)))
        (format stream " REF ~d"
                (cudd-node-ref-count pointer))
        (format stream ", MANAGER #~D"
                (manager-index (node-manager node)))))))
(declaim (notinline print-node-to-string))

(defmethod print-object ((object node) stream)
  (format stream (print-node-to-string object :type (type-of object))))
