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

(defvar config/debug-consistency-checks :check-keys
  "When truthy, make calls to CUDD's reflective funcs whenever a :cl-cudd node is created or finalized.  NOTE: These write to `uiop:*stdout*' and cause immense lag.
  Possible values:
    - `NIL': No checks
    - `:check-keys': Call (cudd-check-keys)
    - `T': All checks
  * TODO [optimization]: Disable by default on max speed.")
(declaim (type (member NIL T :check-keys)
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
