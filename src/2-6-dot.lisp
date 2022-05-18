(in-package :cudd)

(declaim (reentrant plot))
(defun plot (pathname node &aux (manager (node-manager node)))
  "Create '<PATHNAME>.dot' and process it to '<PATHNAME>.pdf'.  Requires that Graphviz be installed.
  * TODO [2021-09-21 Tue]: Raise a specialized exception when `uiop:run-program' fails.
"
  (declare (manager manager))
  (let ((dot (namestring (make-pathname :type "dot" :defaults pathname)))
        (pdf (namestring (make-pathname :type "pdf" :defaults pathname))))
    ;; (break)
    (with-cudd-critical-section (:manager manager)
      (let ((mp (manager-pointer manager))
            (node-ptr (node-pointer node)))
        (declare (manager-pointer mp)
                 (node-pointer node-ptr))
        (etypecase node
          ((or add-node bdd-node)
           (cl-cudd.baseapi:dump-dot mp
                                     (cudd-regular node-ptr)
                                     dot))
          (zdd-node
           (cl-cudd.baseapi:zdd-dump-dot mp (cudd-regular node-ptr)
                                         dot)))))
    (run-program `("dot" ,dot "-Tpdf" "-o" ,pdf))))
