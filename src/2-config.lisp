;;;; Dynamic configuration variables
(in-package :cudd)

(defvar config/guard-pointer-access t
  "TODO: Disable on high optimization | (safety 0).")

(defvar config/enable-gc t
  "When true, new nodes get equipped with finalizers.")

(defvar config/debug-memory-errors t)

(defvar config/debug-consistency-checks nil
  "TODO: Disable by default if assertions are turned off.")

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
