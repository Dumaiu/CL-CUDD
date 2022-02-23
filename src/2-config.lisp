;;;; Dynamic configuration variables.  See ../README.md.
(in-package :cudd)

(defvar config/guard-pointer-access t
  #.(format nil "TODO: Disable on high optimization | (safety 0).
  * TODO: When changed, flag ASDF system for rebuilding?  Or at least this file (~A)."
            (current-lisp-file-pathname)))

(defvar config/enable-gc t
  "When true, new nodes get equipped with finalizers.")

(defvar config/debug-memory-errors t)

(defvar config/debug-consistency-checks nil
  "TODO: Disable by default on max speed.")

(declaim (boolean
          config/guard-pointer-access
          config/enable-gc
          config/debug-memory-errors
          config/debug-consistency-checks))

(export '(
          config/guard-pointer-access
          config/enable-gc
          cudd-logger
          config/debug-memory-errors
          config/debug-consistency-checks
          ))
