;; DDDMP: tools for visualizing Decision Diagrams

(in-package :cl-cudd.baseapi)

(defcfun "fopen" :pointer (path :string) (mode :string))
(defcfun "fclose" :pointer (file :pointer))

(defun dump-dot (manager nodes pathname &key inames onames)
  "Writes a file representing the argument DDs in a format suitable
  for the graph drawing program dot. Returns nil.

  dump-dot uses a minimal unique subset of the
  hexadecimal address of a node as name for it. If the argument inames
  is non-nil, it is assumed to hold the the names of the
  inputs. Similarly for onames. dump-dot uses the following
  convention to draw arcs:

  * solid line: THEN arcs;
  * dotted line: complement arcs;
  * dashed line: regular ELSE arcs.

  The dot options are chosen so that the drawing fits on a letter-size sheet.

  - Note [2021-09-21 Tue]: For a higher-level function which tries to output the rendered graph to PDF, see `cudd:plot'.
  * TODO [2021-09-21 Tue]: More specific error types
"
  (let* ((nodes (if (typep nodes 'sequence) nodes (list nodes)))
         (n (length nodes))
         (n-names (if inames (length inames) 0)))
    (with-foreign-objects
        ((node-array :pointer n)
         (iname-array :string n-names)
         (oname-array :string n))
      (loop :for i :from 0 :below n
            :for node :in nodes
            :do (setf (mem-aref node-array :pointer i) node))
      (when inames
        (loop :for i :from 0 :below n-names
              :for name :across inames
              :do (setf (mem-aref iname-array :string i)
                        (or name (null-pointer)))))
      (when onames
        (loop :for i :from 0 :below n
              :for name :across onames
              :do (setf (mem-aref oname-array :string i)
                        (or name (null-pointer)))))
      (let ((file (fopen (coerce pathname 'string) "w")))
        (if (null-pointer-p file)
            (error "Could not open file for writing")
            (unwind-protect
                (let ((result
                       (cudd-dump-dot manager n node-array
                                      (if inames iname-array (null-pointer))
                                      (if onames oname-array (null-pointer))
                                      file)))
                  (if (= result 1)
                      nil
                      (error "Could not dump to dot file")))
              (fclose file)))))))

(defun zdd-dump-dot (manager nodes pathname &key inames onames)
  "Writes a file representing the argument DDs in a format suitable
  for the graph drawing program dot. Returns nil.

  dump-dot uses a minimal unique subset of the
  hexadecimal address of a node as name for it. If the argument inames
  is non-nil, it is assumed to hold the the names of the
  inputs. Similarly for onames. dump-dot uses the following
  convention to draw arcs:

  * solid line: THEN arcs;
  * dotted line: complement arcs;
  * dashed line: regular ELSE arcs.

  The dot options are chosen so that the drawing fits on a letter-size sheet."
  (let* ((nodes (if (typep nodes 'sequence) nodes (list nodes)))
         (n (length nodes))
         (n-names (if inames (length inames) 0)))
    (with-foreign-objects
        ((node-array :pointer n)
         (iname-array :string n-names)
         (oname-array :string n))
      (loop :for i :from 0 :below n
            :for node :in nodes
            :do (setf (mem-aref node-array :pointer i) node))
      (when inames
        (loop :for i :from 0 :below n-names
              :for name :across inames
              :do (setf (mem-aref iname-array :string i)
                        (or name (null-pointer)))))
      (when onames
        (loop :for i :from 0 :below n
              :for name :across onames
              :do (setf (mem-aref oname-array :string i)
                        (or name (null-pointer)))))
      (let ((file (fopen (coerce pathname 'string) "w")))
        (if (null-pointer-p file)
            (error "Could not open file for writing")
            (unwind-protect
                (let ((result
                       (cudd-zdd-dump-dot manager n node-array
                                          (if inames iname-array (null-pointer))
                                          (if onames oname-array (null-pointer))
                                          file)))
                  (if (= result 1)
                      nil
                      (error "Could not dump to dot file")))
              (fclose file)))))))

(defun print-info (manager pathname)
  "(cudd-print-info) -> Cudd_PrintInfo().  Output is printed into PATHNAME and stdout.
  * TODO: More specific error types.
"
  (declare (foreign-pointer manager)
           ((or string pathname) pathname))
  (let ((file (fopen (namestring pathname) "w")))
    (if (null-pointer-p file)
        (error "Could not open file for writing")
        (unwind-protect
            (let ((result
                    ;; [2022-01-14 Fri] No side effects, according to docs, so we shouldn't need protection:
                    (progn ;with-cudd-critical-section
                      (cudd-print-info manager file))))
              (if (= result 1)
                  nil
                  (error "Could not dump to ~S" pathname)))
          (fclose file))))
  (with-open-file (in pathname :direction :input)
    (loop :for c = (read-char in nil :eof)
          :until (eq c :eof)
          :do (write-char c))))


;;; mtr

;; (declaim (ftype (function (t) boolean) null-pointer- p))
(defun dump-mtr-tree (tree acc)
  (ematch tree
    ((guard p (null-pointer-p p))  ;;(null-pointer) ; XXX The "constructor-matching" syntax doesn't work here?  Perhaps because `null-pointer' isn't a declared type?
	 ;; XXX (satisfies null-pointer-p) ; also fails
     (nreverse acc))
    ((-> (:struct mtr-node)
       flags
       low
       size
       child
       younger)
     (dump-mtr-tree
      younger
      (cons `(:flag ,flags
              :low ,low
              :size ,size
              ,@(when (member :mtr-terminal flags)
                  `(:children ,(dump-mtr-tree child nil))))
            acc)))))
