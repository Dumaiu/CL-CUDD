;;; base class definitions and macros for defining APIs
(in-package :cudd)


(assert (fboundp 'with-cudd-critical-section))
(assert (fboundp 'log-error))
(assert (boundp '*stderr*))

(with-package-log-hierarchy
  (defvar cudd-node-logger (make-logger)
	":log4cl logger created in '2-0-1-node.lisp'.

  - Independent of `cudd-logger'.
	* TODO: Make this a child of `cudd-logger'?  How do I specify that?

  - NOTE: Setting this to :trace or higher will slow CUDD way down by logging all `bdd-node' construction|finalization funcalls.
"))

;; (log4cl:hierarchy-index (find-package :cudd))
;; (log4cl:hierarchy-index cudd:cudd-logger )


(defun required ()
  (error "Required slot"))
;;; Wrapped CUDD node
(defstruct node
  "A boxed CUDD node class. Top class of all CUDD nodes."
  (pointer (required) :type cffi:foreign-pointer))

(declaim (inline keys-check?
				 debug-check?))
(defun keys-check? ()
  (or (eq t config/debug-consistency-checks)
	  (eq :keys config/debug-consistency-checks)))
(defun debug-check? ()
  (or (eq t config/debug-consistency-checks)
	  ;; (member :check-keys config/debug-consistency-checks :test #'eq)
	  (eq :debug config/debug-consistency-checks)))

(defun helper/destruct-node (node-pointer node-type manager ref)
  "NB: We *do* want to maintain a reference to the MANAGER from within a node's finalizer.
  MANAGER: Maintain a reference from the node's finalizer to its manager so there is no chance of dangling-pointer errors.  See M. Asai's note in (wrap-and-finalize).
  REF: When T, decrement CUDD ref.  Passed along from (wrap-and-finalize): if we didn't increment during construction, we don't decrement here.
"
  (declare (optimize safety)) ; TODO: remove
  (declare (node-pointer node-pointer)
		   (manager manager)
		   (boolean ref))

  (macrolet ((with-mem-fault-protection (&body body)
			   "Establish a block to handle a memory fault.  Optionally resume execution, depending on `config/signal-memory-errors'.
  * TODO: Rewrite with (handler-case)?
"
			   `(catch 'mem-fault-suppress
				 (handler-bind-case ; for sb-sys:memory-fault-error
				  (progn ,@body)

				  ;; TODO: Remove reliance on '#+sbcl':
				  #+sbcl (sb-sys:memory-fault-error (xc)
													(ecase config/signal-memory-errors
													  ((:error :log)
													   (let+ (((&accessors-r/o manager-pointer) manager)) ;((manager-string (princ-to-string manager)))
														 (log-error :logger cudd-node-logger
																	;; TODO: Give each node an index?
																	"* Error: memory-fault detected in Lisp:
 ~&~T~<~A~>

while destructing ~A ~A in manager #~D.

 Re-throwing? ~A~%"
																	node-type
																	node-pointer
																	xc
																	manager-pointer ;manager-string
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

				 (ecase node-type
				   (bdd-node (cudd-recursive-deref mp node-pointer))
				   (add-node (cudd-recursive-deref mp node-pointer))
				   (zdd-node (cudd-recursive-deref-zdd mp node-pointer)))

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

(defun helper/construct-node (pointer type ref manager)
  "Used by (wrap-and-finalize)."
  (declare (node-pointer pointer)
		   (boolean ref)
		   (manager manager))

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
					"Constructing wrapper node for ~A.  Before incrementing, REFs = ~D."
					pointer
					(cudd-node-ref-count pointer))

		   ;; *Side-effect*:
		   (cudd-ref pointer)

		   (log-msg :debu7 :logger cudd-node-logger "- After (cudd-ref ~A), REFs = ~D."
					pointer
					(cudd-node-ref-count pointer)))

		  ('otherwise  ; ref=nil
		   (log-msg :trace :logger cudd-node-logger "NON-INCREMENTING wrapper for ~A being constructed (REFs = ~D).
 This should happen only for literals."
					pointer
					(cudd-node-ref-count pointer))

		   #|(let ((initial-ref-count (cudd-node-ref-count pointer)))
		   (declare (fixnum initial-ref-count)) ;
		   (assert (>= initial-ref-count 1)) ;
		   (unless (= 1 initial-ref-count) ;
		   (log-msg :warn :logger cudd-node-logger "Ref count of literal node ~A is ~D, which is > 1" ;
		   pointer     ;
		   initial-ref-count)))|#))

		(let ((node #.(let ((ctor-args '(:pointer pointer)))
						`(ecase type
						   (bdd-node (make-bdd-node ,@ctor-args))
						   (add-node (make-add-node ,@ctor-args))
						   (zdd-node (make-zdd-node ,@ctor-args))))))

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
			   (lambda ()
				 "Closure for finalizing a cudd-node."
				 (helper/destruct-node pointer type manager ref)))))

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

(defmacro wrap-and-finalize (pointer type &optional (ref t))
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
  `(let* ((pointer ,pointer)
		  (type ,type)
		  (ref ,ref)
		  (address (pointer-address pointer))
		  (manager *manager*))
	 (declare
	  (foreign-pointer pointer)
	  (type (member bdd-node add-node zdd-node) type)
	  (boolean ref))

	 (ensure-gethash
	  address
	  (manager-node-hash manager)
	  ;; This form executes iff ADDRESS isn't already present in the hashtable:
	  (helper/construct-node pointer type ref manager))))


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

(deftype generalized-bit ()
  '(or boolean (member 0 1)))

(assert (not (eq 'cudd-T 'cl-cudd.baseapi:cudd-T)))
(defun cudd-T (node)
  "Evaluate the 'then' branch.  Undefined if NODE is not a branch node!"
  (declare (bdd-node node))
  ;; (check-type node bdd-node)
  (let ((res (wrap-and-finalize (cl-cudd.baseapi:cudd-T (node-pointer node)) 'bdd-node)))
	(declare (bdd-node res))
	res))


(assert (not (eq 'cudd-E 'cl-cudd.baseapi:cudd-E)))
(defun cudd-E (node)
  "Evaluate the 'else' branch.  Undefined if NODE is not a branch node!"
  (declare (bdd-node node))
  ;; (check-type node bdd-node)
  (let ((res (wrap-and-finalize (cl-cudd.baseapi:cudd-E (node-pointer node)) 'bdd-node)))
	(declare (bdd-node res))
	res))
