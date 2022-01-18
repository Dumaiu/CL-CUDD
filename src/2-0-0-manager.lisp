;;; base class definitions and macros for defining APIs
(in-package :cudd)

;;; Manager

(export '(manager-init
          manager-initf
          cudd-logger))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +finalizer-log-level+ :debu6
    "FIXME: Unused.")
  (with-package-log-hierarchy
    (defvar cudd-logger (make-logger))))


(deftype uint ()
  'non-negative-fixnum)

(deftype node-pointer ()
  'foreign-pointer)

(deftype manager-pointer ()
  'foreign-pointer)

(deftype variable ()
  '(integer 0))

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
(defstruct (manager
            (:conc-name internal/manager-))
  "A boxed CUDD manager class"
  (pointer (error "MANAGER needs to wrap a pointer")
   :type foreign-pointer)

  ;; Added on 3/5 2018.
  ;; It stores a mapping between a node pointer <-> a lisp node.
  ;; This is added since each dd-node is considered unique and
  ;; it is ugly when there are multiple lisp node objects for a single dd-node pointer.
  (node-table (make-manager-hash-table)
   :type hash-table))

(assert (fboundp 'internal/manager-node-table))
;; Alias (manager-node-hash):
(setf (fdefinition 'manager-node-hash) #'internal/manager-node-table)
(setf (fdefinition '(setf manager-node-hash))
      #'(setf internal/manager-node-table))

#.(cond
    (config/guard-pointer-access
     `(defun manager-pointer (manager)
        ,(format nil "Slot access with ptr validation test.  To disable, set ~S=NIL and rebuild :cl-cudd."
                 'config/guard-pointer-access)
        (declare (manager manager))
        ;; TODO: Should we alaso do a nullity test in here?
        (with-slots (pointer) manager
          (cond
            ((null-pointer-p pointer)
             (error 'cudd-null-pointer-error "Call to (manager-pointer) of `manager' object, ~A, with null pointer"
                    manager))
            ('otherwise
             pointer)))))
    ('otherwise ; alias (manager-pointer) -> (internal/manager-pointer)
     `(setf (fdefinition 'manager-pointer) #'internal/manager-pointer)))
;; Alias (setf (manager-pointer)) -> (setf (internal/manager-pointer)):
(setf (fdefinition '(setf manager-pointer))
      #'(setf internal/manager-pointer))

(define-symbol-macro %mp% (manager-pointer *manager*))

(defun manager-init #.`(&key ,@+manager-initarg-defaults+)
  "Construct and return a new `manager' instance, loading CUDD backend to go with it."
  (let* ((p (cudd-init initial-num-vars
                       initial-num-vars-z
                       initial-num-slots
                       cache-size
                       max-memory))
         (m (make-manager :pointer p)))
    (with-cudd-critical-section
      ;; see 2-4-hook.lisp
      (cudd-add-hook p (callback before-gc-hook) :cudd-pre-gc-hook)
      (cudd-add-hook p (callback after-gc-hook) :cudd-post-gc-hook)
      (cudd-add-hook p (callback before-gc-hook) :cudd-pre-reordering-hook)
      (cudd-add-hook p (callback after-gc-hook) :cudd-post-reordering-hook)
      (finalize m (lambda ()
                    (with-cudd-critical-section
                      (format *error-output* "~&freeing a cudd manager at ~a~%" p)
                      (log-msg :debug :logger cudd-logger "Freeing CUDD manager at ~A." p)
                      (let ((undead-node-count (cudd-check-zero-ref p)))
                        (declare (fixnum undead-node-count)) ; TODO: Better type
                        (assert (zerop undead-node-count) (p undead-node-count)
                                "Assert failed in finalizer of manager ~A, with ~D unrecovered nodes (should be 0)."
                                p undead-node-count))
                      (assert (not (null-pointer-p p)))
                      (cudd-quit p)
                      (setf p (null-pointer)))
                    t)))

    (log-msg :debug :logger cudd-logger "Initialized new CUDD manager ~A." m)
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

"

  (declare (ignorable initial-num-vars
                      initial-num-vars-z
                      initial-num-slots
                      cache-size
                      max-memory))
  `(let ((*manager* (manager-init ,@keys)))
     ,@body))

(defun info ()
  (uiop:with-temporary-file (:stream s :pathname path)
    (print-info %mp% path)
    (uiop:slurp-stream-string s)))

(defun manager-quit (&optional (manager *manager*))
  "Shut down the CUDD manager MANAGER:
  After dismantling the hashtable, run a full Lisp garbage collection to hopefully reclaim the nodes' memory.  Then acquire the CUDD mutex and call `Cudd_Quit()`.
"
  (declare (manager manager))
  ;; We don't need to hold the CUDD mutex for this part:
  (with-slots (node-table pointer) manager
    (log-msg :debug :logger cudd-logger "Closing CUDD manager ~A." manager)
    (clrhash node-table)
    (setf node-table (make-manager-hash-table))
    ;; This should call the finalizers:
    (gc :full t))

  (with-cudd-critical-section
    ;; Re-read pointer, in it got changed elsewhere (like in the hashtable's finalizer):
    (with-slots (node-table pointer) manager
     (unless (null-pointer-p pointer)
       (cudd-quit pointer)
       (setf pointer (null-pointer))))))
