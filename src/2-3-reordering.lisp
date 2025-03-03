(in-package :cudd)

(deftype bdd-reordering-method ()
  `(member
    ,@(foreign-enum-keyword-list 'cudd-reordering-type)
    ;; :CUDD-REORDER-SAME
    ;; :CUDD-REORDER-NONE
    ;; :CUDD-REORDER-RANDOM
    ;; :CUDD-REORDER-RANDOM-PIVOT
    ;; :CUDD-REORDER-SIFT
    ;; :CUDD-REORDER-SIFT-CONVERGE
    ;; :CUDD-REORDER-SYMM-SIFT
    ;; :CUDD-REORDER-SYMM-SIFT-CONV
    ;; :CUDD-REORDER-WINDOW-2
    ;; :CUDD-REORDER-WINDOW-3
    ;; :CUDD-REORDER-WINDOW-4
    ;; :CUDD-REORDER-WINDOW-2-CONV
    ;; :CUDD-REORDER-WINDOW-3-CONV
    ;; :CUDD-REORDER-WINDOW-4-CONV
    ;; :CUDD-REORDER-GROUP-SIFT
    ;; :CUDD-REORDER-GROUP-SIFT-CONV
    ;; :CUDD-REORDER-ANNEALING
    ;; :CUDD-REORDER-GENETIC
    ;; :CUDD-REORDER-LINEAR          ; not documented
    ;; :CUDD-REORDER-LINEAR-CONVERGE ; not documented
    ;; :CUDD-REORDER-LAZY-SIFT       ; not documented
    ;; :CUDD-REORDER-EXACT
    ))

(defun enable-reordering (&optional (method :cudd-reorder-same)
                            (manager *manager*))
  "Enables automatic dynamic reordering of BDDs and ADDs.

  Parameter method is used to determine the method used for
  reordering. If CUDD_REORDER_SAME is passed, the method is unchanged.

  @see Cudd_AutodynDisable Cudd_ReorderingStatus Cudd_AutodynEnableZdd

 - [2022-01-19 Wed] TODO: Keyword parameters. "
  (declare (bdd-reordering-method method)
           (manager manager))
  (with-cudd-critical-section
    (let ((mp (manager-pointer manager)))
      (declare (manager-pointer mp))
      (assert (not (null-pointer-p mp)))
      (cudd-autodyn-enable mp method))))

(define-simple-managed-function disable-reordering cudd-autodyn-disable
  "Disables automatic dynamic reordering of BDDs and ADDs.

  @see Cudd_AutodynEnable Cudd_ReorderingStatus Cudd_AutodynDisableZdd ")

(deftype zdd-reordering-method ()
  '(member :CUDD-REORDER-SAME
    :CUDD-REORDER-NONE
    :CUDD-REORDER-RANDOM
    :CUDD-REORDER-RANDOM-PIVOT
    :CUDD-REORDER-SIFT
    :CUDD-REORDER-SIFT-CONVERGE
    :CUDD-REORDER-SYMM-SIFT
    :CUDD-REORDER-SYMM-SIFT-CONV))

(defun zdd-enable-reordering (&optional (method :cudd-reorder-same))
  "Enables automatic dynamic reordering of ZDDs.

  Parameter method is used to determine the method used for
  reordering. If CUDD_REORDER_SAME is passed, the method is unchanged.

  @see Cudd_AutodynDisableZdd Cudd_ReorderingStatusZdd Cudd_AutodynEnable "
  (declare (zdd-reordering-method method))
  (cudd-autodyn-enable-zdd %mp% method))

(define-simple-managed-function zdd-disable-reordering cudd-autodyn-disable-zdd
  "Disables automatic dynamic reordering of ZDDs.

  @see Cudd_AutodynEnableZdd Cudd_ReorderingStatusZdd Cudd_AutodynDisable ")

(defun reordering-status ()
  "Reports the status of automatic dynamic reordering of BDDs and ADDs.
Return T if automatic reordering is enabled. NIL otherwise.
Secondary value returns the current reordering method.

  @see Cudd_AutodynDisableZdd Cudd_ReorderingStatusZdd Cudd_AutodynEnable"
  (with-foreign-object (method-ptr 'cudd-reordering-type)
    (values (= 1 (cudd-reordering-status %mp% method-ptr))
            (the bdd-reordering-method
                 (mem-ref method-ptr 'cudd-reordering-type)))))

(defun zdd-reordering-status ()
  "Reports the status of automatic dynamic reordering of ZDDs.
Return T if automatic reordering is enabled. NIL otherwise.
Secondary value returns the current reordering method.

  @see Cudd_AutodynEnableZdd Cudd_ReorderingStatusZdd Cudd_AutodynDisableZdd"
  (with-foreign-object (method-ptr 'cudd-reordering-type)
    (values (= 1 (cudd-reordering-status %mp% method-ptr))
            (the zdd-reordering-method
                 (mem-ref method-ptr 'cudd-reordering-type)))))


(defun reduce-heap (&optional (method :cudd-reorder-same) (minsize 33000000))
  "Initiates variable reordering explicitly (for bdd/add).
MINSIZE specifies the lower threshold of the number of the (live/referenced) nodes to initiate reordering:
Number of nodes should be larger than this value.
Default value is 33000000. In CUDD each node consumes 3 words, so this threshold corresponds to 100MB.
  * [2022-02-01 Tue] TODO: ':manager' kwarg.
  * TODO: Factor out MINSIZE.
"
  (declare (bdd-reordering-method method))
  (with-cudd-critical-section
    (let ((result (cudd-reduce-heap %mp% method minsize)))
      (or (= 1 result)
          (error "(cudd-reduce-heap) failed")))))

(defun zdd-reduce-heap (&optional (method :cudd-reorder-same) (minsize 33000000))
  "Initiates variable reordering explicitly (for zdd).
MINSIZE specifies the lower threshold of the number of the (live/referenced) nodes to initiate reordering:
Number of nodes should be larger than this value.
Default value is 33000000. In CUDD each node consumes 3 words, so this threshold corresponds to 100MB."
  (declare (zdd-reordering-method method))
  (assert (= 0 (cudd-zdd-reduce-heap %mp% method minsize))))

#+nil
(defun shuffle-heap ()
  "Reorders variables according to given permutation.

The i-th entry of the permutation array contains the index
of the variable that should be brought to the i-th level.  The size
of the array should be equal or greater to the number of variables
currently in use."
  cudd-shuffle-heap)
#+nil
(defun zdd-shuffle-heap ()
  "Reorders variables according to given permutation.

The i-th entry of the permutation array contains the index
of the variable that should be brought to the i-th level.  The size
of the array should be equal or greater to the number of variables
currently in use."
  cudd-zdd-shuffle-heap)


(deftype mtr-type ()
  `(member
    ,@(foreign-enum-keyword-list 'mtr-type)))

(deftype mtr-flags ()
  `(member
    ,@(foreign-bitfield-symbol-list 'mtr-flags)))

(defun dump-variable-group-hierarchy ()
  (dump-mtr-tree (cudd-read-tree %mp%) nil))
(defun dump-zdd-variable-group-hierarchy ()
  (dump-mtr-tree (cudd-read-zdd-tree %mp%) nil))

(defun set-variable-group (type &key from to size)
  "Defines a variable group in the current manager. It calls cudd-make-tree-node (Cudd_MakeTreeNode).
At least 2 of FROM, TO or SIZE should be specified.

:MTR-DEFAULT requires the variables within the group to be contiguous after the reordering.
:MTR-FIXED   requires the variables within the group to be unaffected by the reordering.
In any cases, groups could be reordered.

If the new group intersects an existing group, it must
either contain it or be contained by it.
"
  (declare ((or null non-negative-fixnum) from to size)
           (mtr-type type))
  (assert
   (not (null-pointer-p
         (cond
           ((and from to size)
            (assert (= size (- from to)))
            (cudd-make-tree-node %mp% from size type))
           ((and from to)
            (cudd-make-tree-node %mp% from (- from to) type))
           ((and to size)
            (cudd-make-tree-node %mp% (- to size) size type))
           ((and from size)
            (cudd-make-tree-node %mp% from size type))
           (t (simple-program-error "insufficient group specification!")))))
   nil "If the new group intersects an existing group, it must either contain it or be contained by it.
Current variable group hierarchy:
"))

(defun set-zdd-variable-group (type &key from to size)
  "Defines a variable group in the current manager (for zdd).
It calls cudd-make-zdd-tree-node (Cudd_MakeZddTreeNode).
At least 2 of FROM, TO or SIZE should be specified.

:MTR-DEFAULT requires the variables within the group to be contiguous after the reordering.
:MTR-FIXED   requires the variables within the group to be unaffected by the reordering.
In any cases, groups could be reordered.

If the new group intersects an existing group, it must
either contain it or be contained by it.
"
  (declare ((or null non-negative-fixnum) from to size)
           (mtr-type type))
  (assert
   (not (null-pointer-p
         (cond
           ((and from to size)
            (assert (= size (- from to)))
            (cudd-make-zdd-tree-node %mp% from size type))
           ((and from to)
            (cudd-make-zdd-tree-node %mp% from (- from to) type))
           ((and to size)
            (cudd-make-zdd-tree-node %mp% (- to size) size type))
           ((and from size)
            (cudd-make-zdd-tree-node %mp% from size type))
           (t (simple-program-error "insufficient group specification!")))))
   nil "If the new group intersects an existing group, it must either contain it or be contained by it.
Current variable group hierarchy:
"))
