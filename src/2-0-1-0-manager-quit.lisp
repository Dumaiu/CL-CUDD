(in-package :cudd)

(declaim (inline destruct-node))
(defun destruct-node (node manager)
  "Used by (manager-quit).  Dealloc NODE and zero its pointer out."
  (declare (type node node)
           (type manager manager))
  (let ((node-pointer (node-pointer node)))
    (declare (node-pointer node-pointer)
             #|(dynamic-extent node-pointer) XXX|#)
    (unless (null-pointer-p node-pointer) ; don't lock if node is already null
      (with-cudd-critical-section
        (setq node-pointer (node-pointer node)) ; re-read after locking
        (unless (null-pointer-p node-pointer)
          (let ((node-type (node-type node)))
            (declare (node-type node-type))
            (destruct-cudd-node-impl node-pointer node-type manager)
            (setf (node-pointer node) (null-pointer))
            node))))))


(defun destruct-manager (manager)
  "Helper called by (manager-quit).

  After dismantling the hashtable, call (destruct-manager-impl) to clean up the CUDD side.

  Theoretically thread-safe.
"
  (declare (manager manager))

  (with-slots (pointer) manager ; needs to be reentrant
    ;; We don't need to hold the CUDD mutex for this part:
    (cond
      ((null-pointer-p pointer)
       (log-msg :debug :logger cudd-logger "CUDD manager ~A already closed." manager))
      (t
       (log-msg :debug :logger cudd-logger "Closing CUDD manager ~A." manager)
       (let ((nodes (manager-node-hash manager)))
         (declare (hash-table nodes))
         (iter
           (for (_ node) :in-hashtable nodes)
           ;; (declare (node node))
           (destruct-node node manager)
           (assert (null-pointer-p (node-pointer node))))

         (clrhash nodes)) ; may not be necessary, as the nodes are weak refs and completely neutralized by this point

       ;; TODO: Can we avoid this outer lock?  (There's one in (destruct-manager-impl)).
       (with-cudd-critical-section
         ;; Re-read pointer, in it got changed asynchronously:
         (unless (null-pointer-p pointer)
           (destruct-manager-impl pointer)
           (setf pointer (null-pointer))))))
    (assert (null-pointer-p pointer)))
  nil)

(declaim (inline manager-quit))
(defun manager-quit (&optional (manager *manager*))
  "Shut down the CUDD manager MANAGER."
  (declare (manager manager))
  (destruct-manager manager)
  manager)

;; (declaim (inline manager-reinit))
(defmacro manager-reinitf (&optional (manager-form '*manager*))
  "Reset the `manager' stored at MANAGER-FORM, returning the new instance.

  MANAGER-FORM must evaluate to a `manager'.
"
  (with-gensyms (manager)
    `(with-cudd-critical-section
       (let ((,manager ,manager-form))
         (declare (manager ,manager))

         (destruct-manager ,manager)
         (let ((new-manager (manager-initf ,manager-form :force t)))
           (declare (manager new-manager))
           (log-msg :debug :logger cudd-logger "Reinitialized CUDD manager stored at ~S."
                    ',manager-form)
           new-manager)))))
