;;;; Dynamic configuration variables.  See ../README.md.
;;;; TODO [optimization]: Pre-build options which turn these dynavars into compile-time constants
(in-package :cudd)

(define-constant config/guard-pointer-access t
  :documentation #.(format nil "TODO: Disable on high optimization | (safety 0).
  * TODO: When changed, flag ASDF system for rebuilding?  Or at least this file (~A)."
                           (current-lisp-file-pathname)))

(defvar config/enable-gc t
  "When true, new nodes get equipped with finalizers.")

(defvar config/signal-memory-errors :error)
(declaim (type (member :error :log nil)
               config/signal-memory-errors ))

(defvar config/debug-consistency-checks :debug
  "When truthy, make calls to CUDD's reflective funcs whenever a :cl-cudd node is created or finalized.  NOTE: These cause immense lag.
  Possible values:
    - `NIL': No checks
    - `:keys': Call (cudd-check-keys)--writes to `uiop:*stdout*' constantly
    - `:debug': (cudd-debug-check)--still slow, but only writes to `*stdout*' on failure
    - `T': Both
  * TODO [optimization]: Disable by default on max speed.")
(declaim (type (member NIL T :keys :debug)
               config/debug-consistency-checks))

(declaim (boolean
          config/guard-pointer-access
          config/enable-gc
          ;; config/signal-memory-errors
          ;; config/debug-consistency-checks
          ))

(export '(
          config/guard-pointer-access
          config/enable-gc
          cudd-logger
          config/signal-memory-errors
          config/debug-consistency-checks
          ))
