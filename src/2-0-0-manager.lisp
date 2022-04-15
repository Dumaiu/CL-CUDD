;;; base class definitions and macros for defining APIs
(in-package :cudd)

;;; Manager

(export '(manager-init
          make-manager
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

;; (declaim (maybe-inline internal/manager-pointer
;;                        internal/manager-node-table))
;; (declaim (inline internal/manager-node-table
;;                  (setf internal/manager-node-table)
;;                  internal/manager-mutex))

;;; id=class
(defclass manager ()
  ((index :initform *manager-counter*
          :type uint
          ;; XXX :read-only t
          :initarg :index
          :reader index
          :reader manager-index
          :documentation "NOTE: Each manager's index should be unique--the initializer must increment *manager-counter* post-construction."
          )
   (pointer :initform (error "MANAGER needs to wrap a `manager-pointer'")
            :type manager-pointer
            :accessor manager-pointer ; needs R/W
            :initarg :pointer)

   ;; Added on 3/5 2018.
   ;; It stores a mapping between a node pointer <-> a lisp node.
   ;; This is added since each dd-node is considered unique and
   ;; it is ugly when there are multiple lisp node objects for a single dd-node pointer.
   (node-table :initform (make-manager-hash-table)
               :type hash-table
               :reader manager-node-table
               :reader manager-node-hash)

   #+thread-support
   (mutex :initform (make-recursive-lock (string (gensym "cudd-manager-lock")))
          :type manager-mutex
          :reader manager-mutex
          #| XXX :read-only t|#))
  (:documentation "A boxed CUDD manager class.

   - [2022-04-15 Fri] Switched to (defclass).
"))

;; (:conc-name internal/manager-)

#|
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

TODO: What about #-thread-support ?
|#


#.(cond
    (config/guard-pointer-access
     #|`(defun manager-pointer (manager)
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
     pointer))))|#
     `(defmethod manager-pointer :before ((manager manager))
        ,(format nil "Slot access with ptr validation test.  To disable, set ~S=NIL and rebuild :cl-cudd." 'config/guard-pointer-access)
        ;; TODO: Should we also do a nullity test in here?
        (with-slots (pointer) manager
          (when (null-pointer-p pointer)
            (let ((manager-string (princ-to-string manager)))
              #.(let ((msg '("Call to (manager-pointer) of `manager' object, ~A, with null pointer" manager-string)))
                  `(progn
                     (log-msg :error :logger cudd-logger ,@msg)
                     (error 'cudd-null-pointer-error ,@msg))))))))
    ('otherwise
     `(defmethod manager-pointer :before ((manager manager))
        ,(format nil "Does nothing.  To enable, set ~S=T and rebuild :cl-cudd." 'config/guard-pointer-access)
        (declare (ignore manager)))
     #| alias (manager-pointer) -> (internal/manager-pointer)
     `(setf (fdefinition 'manager-pointer) #'internal/manager-pointer)|#
     ))

(declaim (inline manager-init
                 make-manager))
(defun manager-init (&rest *args)
  "Ctor."
  (apply #'make-instance 'manager *args))

;; Alias:
(setf (symbol-function 'make-manager) #'manager-init)

#|
;; Alias (setf (manager-pointer)) -> (setf (internal/manager-pointer)):
(setf (fdefinition '(setf manager-pointer))
      #'(setf internal/manager-pointer))
|#

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

(defmethod print-object ((m manager) stream)
  (assert (not *print-readably*))
  (print-unreadable-object (m stream :type t :identity nil)
    (with-slots (index
                 mutex
                 node-table)
        m
      (format stream "#~D:" index)
      (format stream " nodes: ~D" (hash-table-count node-table))
      (format stream ", mutex: ~A" mutex))))



(defmethod shared-initialize #.`((m manager) slot-names &rest *keys
                                 &key ,@+manager-initarg-defaults+
                                 &allow-other-keys)
  "Construct and return a new `manager' instance, loading CUDD backend to go with it.

  * TODO [optimize] (call-next-method).
  * TODO Thread-safety for *manager-counter* & *manager-index*
"
  (let* ((p (cudd-init initial-num-vars
                       initial-num-vars-z
                       initial-num-slots
                       cache-size
                       max-memory))
         (m (apply #'call-next-method m slot-names :pointer p  *keys)))
    (declare (manager m))

    (assert* (not (null-pointer-p p)))

    ;; (break "~A" m)
    (let ((manager-index (manager-index m)))
      (assert* (= manager-index *manager-counter*))
      ;; *Side-effect*
      (incf *manager-counter*)
      ;; TODO: Decrement if the stack gets unwound during construction

      #.(let ((fmt '("~&Constructing CUDD manager #~D ~%" manager-index ;; p
                     )))
          `(progn
             (format *stderr* ,@fmt)
             (log-msg :debug :logger cudd-logger ,@fmt)))

      (with-cudd-critical-section (:manager m)
        ;; see 2-4-hook.lisp
        (cudd-add-hook p (callback before-gc-hook) :cudd-pre-gc-hook)
        (cudd-add-hook p (callback after-gc-hook) :cudd-post-gc-hook)
        (cudd-add-hook p (callback before-gc-hook) :cudd-pre-reordering-hook)
        (cudd-add-hook p (callback after-gc-hook) :cudd-post-reordering-hook)

        (finalize m (lambda ()
                      "Manager callback finalizer.
  NOTE that I don't both using (with-cudd-critical-section) here, since the manager is dead anyway.  No other thread should be interacting with it.

  * TODO: Refactor to its own function...
"
                      (declare (optimize safety))
                      (assert* (null (gethash manager-index *managers*)))

                      (unwind-protect (progn
                                        ;;with-cudd-critical-section (:mutex mutex)
                                        (assert* (not (null-pointer-p p)))

                                        ;; NB: Don't retain a reference to the containing `manager' in this finalizer.  See Masataro Asai's NOTE on the finalizer for `node'.
                                        #.(let ((fmt '("~&Freeing CUDD manager #~D~%" manager-index ;; p
                                                       )))
                                            `(progn
                                               (format *stderr* ,@fmt)
                                               (log-msg :debug :logger cudd-logger ,@fmt)))

                                        (when config/check-zero-ref-when-manager-finalized
                                          (let ((undead-node-count (cudd-check-zero-ref p)))
                                            (declare (uint undead-node-count))
                                            (unless (zerop undead-node-count)
                                              (ecase  config/check-zero-ref-when-manager-finalized
                                                (:log
                                                 (log-error :logger cudd-logger
                                                            "* ERROR: In finalizer, manager #~D ~A was left with ~D unrecovered node~:P (should be 0)."
                                                            manager-index p undead-node-count))
                                                (:error
                                                 (not-implemented-error 'error-arg)
                                                 (assert* (zerop undead-node-count) (p undead-node-count)
                                                          "Assert failed in finalizer of manager #~D ~A, with ~D unrecovered node~:P (should be 0)."
                                                          manager-index p undead-node-count))))))
                                        (when t ; FIXME config/check-...
                                          (unless (zerop (cudd-check-keys p))
                                            (log-error :logger cudd-logger "* Error: (cudd-check-keys) failed in finalizer of manager #~D" manager-index))

                                          (unless (zerop (cudd-debug-check p))
                                            (log-error :logger cudd-logger "* Error: (cudd-debug-check) failed in finalizer of manager #~D" manager-index))))

                        ;; Cleanup:
                        (cudd-quit p) ; *Side-effect*

                        #| (setf p (null-pointer)) ; pointless |#)
                      t))

        ;; *Side-effect*:
        (assert* (null (gethash manager-index *managers*)))
        (setf (gethash manager-index *managers*) m))

      ;; TODO:
      ;; (let ((manager-string (princ-to-string m)))
      ;;  (log-msg :debug :logger cudd-logger "Initialized new CUDD manager ~A." manager-string))
      (log-msg :debug :logger cudd-logger "Initialized CUDD manager #~D." manager-index))
    m))


(defmacro manager-initf (&optional (manager-form '*manager*)
                         &rest other-initargs!
                         &key force
                         &allow-other-keys)
  "Like (manager-init), but expects a SETFable form.
  - MANAGER-FORM must be evaluable.
  - A truthy MANAGER-FORM is an error, unless FORCE=T as well, in which case the old manager will be killed.
  * TODO: Support a more flexible mix of &optional|&key args.
  * TODO: (define-modify-macro)?
"
  ;; (break "~A" manager-form)
  (delete-from-plistf other-initargs! :force)
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
          (setf ,manager-form (manager-init ,@other-initargs!)))
         (t (error "'~A' already denotes a live ~S.  ~&Use '~S' to override."
                   ',manager-form
                   'manager
                   '(manager-initf ,manager-form :force t)
                   ))))))

(defvar *manager* nil "The current manager.

Every function in this package works with this manager.

Bound to a global manager by default.")
(declaim (type (or manager null) *manager*))




(defmacro with-manager (#.`(&rest keys &key
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
      (let+ (((&accessors-r/o manager-index) manager))
        (declare (type uint manager-index))
        (log-msg :debug :logger cudd-logger "Closing CUDD manager #~D." manager-index)
        ;; (let ((manager-string (princ-to-string manager)))
        ;;  (log-msg :debug :logger cudd-logger "Closing CUDD manager ~A." manager-string))
        )

      ;; *Side-effect*:
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
