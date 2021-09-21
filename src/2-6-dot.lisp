
(in-package :cudd)

(defun plot (pathname node)
  "Create '<PATHNAME>.dot' and process it to '<PATHNAME>.pdf'.  Requires that Graphviz be installed.
  * TODO [2021-09-21 Tue]: Raise an specialized exception when `uiop:run-program' fails.
"
  ;; (setq pathname (uiop:ensure-absolute-pathname pathname))
  (let ((dot (namestring (make-pathname :type "dot" :defaults pathname)))
        (pdf (namestring (make-pathname :type "pdf" :defaults pathname))))
	;; (break)
    (etypecase node
      ((or add-node bdd-node)
       (cl-cudd.baseapi:dump-dot
        (manager-pointer *manager*)
        (cudd-regular (node-pointer node))
        dot))
      (zdd-node
       (cl-cudd.baseapi:zdd-dump-dot
        (manager-pointer *manager*)
        (cudd-regular (node-pointer node))
        dot)))
    (uiop:run-program `("dot" ,dot "-Tpdf" "-o" ,pdf))))
