(in-package :cudd)

;; add-hook
;; remove-hook
;; isinhook
;; stdprereordhook
;; stdpostreordhook
;;
;; in cl-cudd we completely ignore the provided hook facility.

(defvar *before-gc-hook* nil
  "A list of function designators that is called before GC.
The function takes one argument, a mode string (\"DD\", \"BDD\" or \"ZDD\").

This list is managed by a lisp process, independently from Cudd_AddHook function.
WITH-MANAGER macro registers 4 global hooks which in turn call the functions in this special variable.")
(defvar *after-gc-hook*  nil
  "A list of function designators that is called before GC.
The function takes one argument, a mode string (\"DD\", \"BDD\" or \"ZDD\").

This list is managed by a lisp process, independently from Cudd_AddHook function.
WITH-MANAGER macro registers 4 global hooks which in turn call the functions in this special variable.")
(defvar *before-reordering-hook* nil
  "A list of function designators that is called before GC.
The function takes one argument, a mode string (\"DD\", \"BDD\" or \"ZDD\").

This list is managed by a lisp process, independently from Cudd_AddHook function.
WITH-MANAGER macro registers 4 global hooks which in turn call the functions in this special variable.")
(defvar *after-reordering-hook*  nil
  "A list of function designators that is called before GC.
The function takes one argument, a mode string (\"DD\", \"BDD\" or \"ZDD\").

This list is managed by a lisp process, independently from Cudd_AddHook function.
WITH-MANAGER macro registers 4 global hooks which in turn call the functions in this special variable.")

(defun hook-runner (hooks dd mode)
  (let ((*manager* (make-manager :pointer dd)))
    (handler-case
        (dolist (fn hooks 1)
          (funcall fn mode))
      (error ()
        0))))

(defcallback before-gc-hook :int ((dd :pointer) (mode :string) (data :pointer))
  (declare (ignore data))
  (hook-runner *before-gc-hook* dd mode))

(defcallback after-gc-hook :int ((dd :pointer) (mode :string) (data :pointer))
  (declare (ignore data))
  (hook-runner *after-gc-hook* dd mode))

(defcallback before-reordering-hook :int ((dd :pointer) (mode :string) (data :pointer))
  (declare (ignore data))
  (hook-runner *before-reordering-hook* dd mode))

(defcallback after-reordering-hook :int ((dd :pointer) (mode :string) (data :pointer))
  (declare (ignore data))
  (hook-runner *after-reordering-hook* dd mode))

(when (null *manager*)
  (manager-initf *manager*)
  ;; (break "Count: ~D" (cudd-node-ref-count (cudd-read-logic-zero %mp%)))
  )

;; Do a sanity check on the Boolean constants' reference counts:
;; TODO: Should the comparison be strict?
(with-cudd-critical-section
  (let ((1-count (cudd-node-ref-count (cudd-read-one %mp%)))
        (0-count (cudd-node-ref-count (cudd-read-logic-zero %mp%))))
    (assert (>= 1-count 1) (1-count)
            "Assert failed: (cudd-node-ref-count (cudd-read-one %mp%)) should be 1, but we have ~D." 1-count)
    (assert (>= 0-count 1) (0-count)
            "Assert failed: (cudd-node-ref-count (cudd-read-logic-zero %mp%)) should always be 1, but we have ~D." 0-count)))

