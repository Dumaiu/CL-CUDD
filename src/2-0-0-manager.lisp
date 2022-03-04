;;; base class definitions and macros for defining APIs
(in-package :cudd)

;;; Manager

(export '(manager-init
		  manager-initf
		  cudd-logger
		  manager-index
		  *managers*))

;; (assert (find-class 'manager-mutex))

(deftype uint ()
  'non-negative-fixnum)

(deftype node-pointer ()
  'foreign-pointer)

(deftype manager-pointer ()
  'foreign-pointer)

(deftype variable ()
  '(integer 0))

(defvar *manager-counter* 0
  "Number of managers created.
  * TODO: Secure with a mutex if it needs to be used by multiple threads. ")
(declaim (type uint *manager-counter*))

(defvar *managers* #.`(make-weak-hash-table
					   ,@(when (featurep :sb-thread)
						   '(:synchronized t))
					   :weakness :value
					   :weakness-matters t)
		"Managers currently active.")




(assert (not (eq 'variable 'cl:variable)))
(defmethod documentation (object (_ (eql 'variable)))
  "Recurse.  Cause for this overload is the shadowing of 'cl:variable'."
  (documentation object 'cl:variable))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +manager-initarg-defaults+
	  '((initial-num-vars 0)
		(initial-num-vars-z 0)
		(initial-num-slots 256)
		(cache-size 262144)
		(max-memory 0))
	:documentation "Used by (manager-init), (with-manager)."
	:test #'equal))

(defun make-manager-hash-table ()
  (let ((table #.`(make-weak-hash-table :weakness :value
										,@(when (featurep :sb-thread)
											'(:synchronized t)))))
	(declare (hash-table table))
	table))

;; TODO:
;; (declaim (maybe-inline internal/manager-pointer
;;                        internal/manager-node-table))
(declaim (inline internal/manager-node-table
				 (setf internal/manager-node-table)
				 internal/manager-mutex))
(defstruct (manager
			(:conc-name internal/manager-))
  "A boxed CUDD manager class.

  - [2022-02-10 Thu] Note: The :conc-name for the struct is not 'manager-'; this is so I can extend the slot access functions with guards.
"
  ;; NOTE: Each manager's index should be unique--(manager-init) must increment *manager-counter* post-construction.
  (index *manager-counter*
   :type uint
   :read-only t)
  (pointer (error "MANAGER needs to wrap a pointer")
   :type foreign-pointer)

  ;; Added on 3/5 2018.
  ;; It stores a mapping between a node pointer <-> a lisp node.
  ;; This is added since each dd-node is considered unique and
  ;; it is ugly when there are multiple lisp node objects for a single dd-node pointer.
  (node-table (make-manager-hash-table)
   :type hash-table)

  #+thread-support (mutex (make-recursive-lock (string (gensym "cudd-manager-lock")))
					:type manager-mutex
					:read-only t))

(assert* (fboundp 'internal/manager-node-table))
;; Alias (manager-node-hash):
(setf (fdefinition 'manager-node-hash) #'internal/manager-node-table)
(setf (fdefinition '(setf manager-node-hash))
	  #'(setf internal/manager-node-table))
(declaim (ftype (function (manager) uint) manager-index))
(setf (fdefinition 'manager-index) #'internal/manager-index)

;; Alias (manager-mutex):
#+thread-support
(progn
  (declaim (ftype (function (manager) manager-mutex) manager-mutex))
  (setf (fdefinition 'manager-mutex) #'internal/manager-mutex))

#.(cond
	(config/guard-pointer-access
	 `(defun manager-pointer (manager)
		,(format nil "Slot access with ptr validation test.  To disable, set ~S=NIL and rebuild :cl-cudd."
				 'config/guard-pointer-access)
		(declare (manager manager))
		;; TODO: Should we also do a nullity test in here?
		(with-slots (pointer) manager
		  (cond
			((null-pointer-p pointer)
			 (let ((manager-string (princ-to-string manager)))
			   #.(let ((msg '("Call to (manager-pointer) of `manager' object, ~A, with null pointer" manager-string)))
				   `(progn
					  (log-msg :error :logger cudd-logger ,@msg)
					  (error 'cudd-null-pointer-error ,@msg)))))
			('otherwise
			 pointer)))))
	('otherwise ; alias (manager-pointer) -> (internal/manager-pointer)
	 `(setf (fdefinition 'manager-pointer) #'internal/manager-pointer)))

;; Alias (setf (manager-pointer)) -> (setf (internal/manager-pointer)):
(setf (fdefinition '(setf manager-pointer))
	  #'(setf internal/manager-pointer))

(define-symbol-macro %mp% (manager-pointer *manager*))

#+thread-support
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-symbol-macro *cudd-mutex* (manager-mutex *manager*))
  ;; (setf (documentation '*cudd-mutex* 'variable)
  ;;       (format nil "Used in (~S)."
  ;;               'wrap-and-finalize))
  )

#-thread-support
(defmacro with-lock-held ((_lock) &body body)
  "Execute BODY unconditionally.  Should only run in the absence of a real (with-lock-held)."
  (declare (symbol _lock)
		   (ignore _lock))
  `(progn
	 ,@body))

(assert (fboundp 'with-lock-held))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun keyword-plist? (x)
	"Cheap property-list check.  TODO: Move to somewhere else."
	(and (consp x)
		 (keywordp (car x))))

  (defun helper/with-cudd-critical-section/parse-body (body)
	(declare (list body))

	(flet ((parse-mutex-kwarg (&key (manager '*manager* manager-provided?)
								 (mutex `(manager-mutex ,manager) mutex-provided?))
			 "Helper lambda.  Parses keyword arg list."
			 ;; (when *other (not-implemented-error 'with-cudd-critical-section/other-options
			 ;;                                     "Only ':manager' is accepted, for now.") )

			 (when (and manager-provided? mutex-provided?)
			   (error "Only use one of :mutex or :manager as a parameter."))

			 (list :mutex mutex)))

	  (match body
		((list* (guard kwargs
					   (keyword-plist? kwargs))
				body_)
		 (nconc (apply #'parse-mutex-kwarg kwargs)
				(list :body body_)))
		(_
		 (list :mutex '(manager-mutex *manager*)
			   :body body)))))

  ;; Some quick testing:
  (assert (equal
		   (helper/with-cudd-critical-section/parse-body '(foo bar baz))
		   '(:mutex (manager-mutex *manager*)
			 :body (foo bar baz))))

  (assert (equal
		   (helper/with-cudd-critical-section/parse-body '((:manager m)
														   foo bar baz))
		   '(:mutex (manager-mutex m)
			 :body (foo bar baz))))

  (assert (equal (helper/with-cudd-critical-section/parse-body '(#|with-cudd-critical-section|# (:mutex mut)
																 foo bar baz))
				 '(:mutex mut :body (foo bar baz)))))

(defmacro with-cudd-critical-section (&body body)
  "Acquire lock around the CUDD API while executing BODY.

  The first sexp in BODY is allowed to be a keyword plist.  For example::

	(with-cudd-critical-section (:manager «manager-form») ...)

  - ':manager «manager-form»': «manager-form» is evaluated to find the mutex for the critical section.  NB: Don't use this option in a node finalizer!
  - ':mutex «mutex-form»': In lieu of ':manager', you can pass a mutex directly.  *Do* use this in a node finalizer.
"
  (let+ ((parsed-body (helper/with-cudd-critical-section/parse-body body))
		 ((&plist-r/o (mutex-form :mutex) (body :body)) parsed-body))
	(declare (type (not null) mutex-form)
			 (list body))
	`(with-recursive-lock-held (,mutex-form)
	   ,@body)))

(defun manager-init #.`(&key ,@+manager-initarg-defaults+)
  "Construct and return a new `manager' instance, loading CUDD backend to go with it."
  (let* ((p (cudd-init initial-num-vars
					   initial-num-vars-z
					   initial-num-slots
					   cache-size
					   max-memory))
		 (m (make-manager :pointer p))
		 ;; (mutex (manager-mutex m))
		 )
	(declare (manager m)
			 ;; (manager-mutex mutex)
			 )

	(assert* (not (null-pointer-p p)))

	;; (break "~A" m)
	(let ((manager-index (manager-index m)))
	  (assert* (= manager-index *manager-counter*))
	  (incf *manager-counter*); *Side-effect*
	  ;; TODO: Decrement if the stack gets unwound during construction

	  (with-cudd-critical-section (:manager m)
		;; see 2-4-hook.lisp
		(cudd-add-hook p (callback before-gc-hook) :cudd-pre-gc-hook)
		(cudd-add-hook p (callback after-gc-hook) :cudd-post-gc-hook)
		(cudd-add-hook p (callback before-gc-hook) :cudd-pre-reordering-hook)
		(cudd-add-hook p (callback after-gc-hook) :cudd-post-reordering-hook)

		(finalize m (lambda ()
					  "Manager callback finalizer"
					  (assert* (null (gethash manager-index *managers*)))

					  (unwind-protect (progn
										;;with-cudd-critical-section (:mutex mutex)
										(assert* (not (null-pointer-p p)))

										;; NB: Don't retain a reference to the containing `manager' in this finalizer.  See Masataro Asai's NOTE on the finalizer for `node'.
										#.(let ((fmt '("~&Freeing CUDD manager #~D at ~a~%" manager-index p)))
											`(progn
											   (format *error-output* ,@fmt)
											   (log-msg :debug :logger cudd-logger ,@fmt)))

										(let ((undead-node-count (cudd-check-zero-ref p)))
										  (declare (uint undead-node-count))
										  (assert* (zerop undead-node-count) (p undead-node-count)
												   "Assert failed in finalizer of manager #~D ~A, with ~D unrecovered nodes (should be 0)."
												   manager-index p undead-node-count)))

						;; Cleanup:
						(cudd-quit p) ; *Side-effect*

						#| (setf p (null-pointer)) ; pointless |#)
					  t))

		;; *Side-effect*:
		(assert* (null (gethash manager-index *managers*)))
		(setf (gethash manager-index *managers*) m))

	  (let ((manager-string (princ-to-string m)))
		(log-msg :debug :logger cudd-logger "Initialized new CUDD manager ~A." manager-string)))
	m))


(defmacro manager-initf (&optional (manager-form '*manager*)
						 &key force)
  "Like (manager-init), but expects a SETFable form.
  - MANAGER-FORM must be evaluable.
  - A truthy MANAGER-FORM is an error, unless FORCE=T as well, in which case the old manager will be killed.
  * TODO: Support a more flexible mix of &optional|&key args.
  * TODO: (define-modify-macro)?
"
  ;; (break "~A" manager-form)
  (once-only (force
			  (manager manager-form))
	`(progn
	   (check-type ,force boolean)
	   (check-type ,manager (or null manager))
	   (cond
		 ((or (null ,manager)
			  ,force)
		  (unless (null ,manager)
			(manager-quit ,manager))
		  (setf ,manager-form (manager-init)))
		 (t (error "'~A' already denotes a live ~S.  ~&Use '~S' to override."
				   ',manager-form
				   'manager
				   '(manager-initf ,manager-form :force t)
				   ))))))

(defvar *manager* nil "The current manager.

Every function in this package works with this manager.

Bound to a global manager by default.")
(declaim (type (or manager null) *manager*))




(defmacro with-manager (#.`(&rest
							  keys
							&key
							  ,@+manager-initarg-defaults+)
						&body body)
  "Bind a freshly generated manager to *MANAGER*.
This macro is not so useful when multiple managers are in place.
Also, all data on the diagram are lost when it exits the scope of WITH-MANAGER.

* INITIAL-NUM-VARS and INITIAL-NUM-VARS-Z: are just initial values.
  The number of variables in CUDD manager is automatically increased when it exceeds this value.

* INITIAL-NUM-SLOTS : initial size of the unique tables

* CACHE-SIZE : initial size of the cache

* MAX-MEMORY : target maximum memory occupation. If zero, CUDD decides suitable
  values for the maximum size of the cache and for the limit for fast
  unique table growth based on the available memory.

* TODO [2022-02-09 Wed]: ':copy' keyword.  Copy the initvals from an existing manager.  Also, perhaps, the autoreorder configuration.
"

  (declare (ignorable initial-num-vars
					  initial-num-vars-z
					  initial-num-slots
					  cache-size
					  max-memory))
  `(let ((*manager* (manager-init ,@keys)))
	 ,@body))

(defun manager-quit (&optional (manager *manager*))
  "Shut down the CUDD manager MANAGER:
  After dismantling the hashtable, run a full Lisp garbage collection to hopefully reclaim the nodes' memory.  Then acquire the CUDD mutex and call `Cudd_Quit()`.

  * TODO: Remove MANAGER from `*managers*'.
"
  (declare (manager manager))

  ;; We don't need to hold the CUDD mutex for this part:
  (with-slots (node-table) manager
	(unless (null-pointer-p (manager-pointer manager))
	  (let ((manager-string (princ-to-string manager)))
		(log-msg :debug :logger cudd-logger "Closing CUDD manager ~A." manager-string))
	  (clrhash node-table)
	  ;; (setf node-table (make-manager-hash-table))

	  ;; This should call the finalizers:
	  (gc :full t)

	  (with-cudd-critical-section (:manager manager)
		;; NOTE: Re-read pointer, in case it got changed asynchronously (e.g., if (manager-quit) got called from another thread):
		(with-accessors ((pointer manager-pointer)) manager
		  (declare (manager-pointer pointer))
		  (unless (null-pointer-p pointer)
			(cudd-quit pointer)
			(setf pointer (null-pointer)))))))
  manager)
