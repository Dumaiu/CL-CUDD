;;; base class definitions and macros for defining APIs
(in-package :cudd)

;;; Manager

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +manager-initarg-defaults+
	  '((initial-num-vars 0)
		(initial-num-vars-z 0)
		(initial-num-slots 256)
		(cache-size 262144)
		(max-memory 0))
	:documentation "Used by (manager-init), (with-manager)."
	:test #'equal))

(defstruct manager
  "A boxed CUDD manager class"
  (pointer (error "MANAGER needs to wrap a pointer")
   :type cffi:foreign-pointer)

  ;; Added on 3/5 2018.
  ;; It stores a mapping between a node pointer <-> a lisp node.
  ;; This is added since each dd-node is considered unique and
  ;; it is ugly when there are multiple lisp node objects for a single dd-node pointer.
  (node-hash #.`(tg:make-weak-hash-table :weakness :value
										 ,@(when (featurep :sb-thread)
											 '(:synchronized t)))
   :type hash-table))

(define-symbol-macro %mp% (manager-pointer *manager*))

(defun manager-init #.`(&key
						,@+manager-initarg-defaults+)
  (with-cudd-critical-section
	(let* ((p (cudd-init initial-num-vars
						 initial-num-vars-z
						 initial-num-slots
						 cache-size
						 max-memory))
		   (m (make-manager :pointer p)))
	  ;; see 2-4-hook.lisp
	  (cudd-add-hook p (callback before-gc-hook) :cudd-pre-gc-hook)
	  (cudd-add-hook p (callback after-gc-hook) :cudd-post-gc-hook)
	  (cudd-add-hook p (callback before-gc-hook) :cudd-pre-reordering-hook)
	  (cudd-add-hook p (callback after-gc-hook) :cudd-post-reordering-hook)
	  (tg:finalize m (lambda ()
					   (with-cudd-critical-section
						 (format *error-output* "~&freeing a cudd manager at ~a~%" p)
						 (cudd-quit p))))
	  m)))

(defvar *manager* nil "The current manager.

Every function in this package works with this manager.

Bound to a global manager by default.")

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

"

  (declare (ignorable initial-num-vars
					  initial-num-vars-z
					  initial-num-slots
					  cache-size
					  max-memory))
  `(let ((*manager* (manager-init ,@keys)))
	 ,@body))

(defun info (&optional (manager *manager*))
  (declare (type manager manager))
  (with-temporary-file (:stream s :pathname path)
    (print-info manager path)
    (slurp-stream-string s)))


