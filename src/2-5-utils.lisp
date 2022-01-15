(in-package :cudd)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadowing-import* 'iterate :cudd)
  (shadowing-import* 'collect :cudd)
  (shadowing-import* 'until :cudd)
  (use-package :series)
  (export '(support-index-int-series
            ;; basic-series
            series)))

(declaim (ftype function fopen
                fclose))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (import '(sb-kernel:pathname-designator))
  #-sbcl (deftype pathname-designator ()
           '(or string pathname synonym-stream file-stream)))

(defmacro with-pointers (pointers &body body)
  "Copied from `e9bf9fe:cudd.lisp' (https://github.com/rpgoldman/CL-CUDD/blob/master/cudd.lisp).
  --------------------------------------------------------------

  Create a binding to pointers using a let-like specification.
  Takes care that the nodes stay alive during the body.

    Example:
    (with-pointers ((f-ptr f)
                    (g-ptr g))
      (cudd-add-apply +or+ f-ptr g-ptr))

  This is implemented by increasing the reference count of
  every node in the body and decreasing it after the body is run"
  (let ((manager (gensym "manager")))
    `(let* ((,manager (manager-pointer *manager*)))
       (progn
         ;; Reference all pointers
         ;; Effectively, we call node-pointer twice on every wrapper.
         ;; The reason is that we have to keep holding on to
         ;; the wrapper until after all referencing is done, i.e.,
         ;; until after the following code block ran.
         ;; Therefore, we first call `(cudd-ref (node-pointer wrapper))`
         ;; for each wrapper. Then later we create the binding for
         ;; the pointers.
         ,@(loop
             :for binding :in pointers
             :unless (and (listp binding) (= (length binding) 2))
               :do (error "Binding ~A is mal-formed" binding)
             :collect `(cudd-ref (node-pointer ,(cadr binding))))
         (let
             ;; Create bindings
             ,(loop
                :for binding :in pointers
                :collect `(,(car binding) (node-pointer ,(cadr binding))))
           (unwind-protect
                (progn
                  ,@body)
             ,@(loop
                 :for binding :in pointers
                 :collect `(cudd-recursive-deref ,manager ,(car binding)))))))))




(define-simple-managed-function read-size cudd-read-size)

#+sbcl (declaim (sb-ext:maybe-inline helper/bdd-seq-to-ptr-vector!
                                     bdd-vector-compose))
(defun helper/bdd-seq-to-ptr-vector! (ptr-array bdds manager-ptr
                                      &aux (node-pointer-t :pointer))
  "Not really a vector, but a CFFI C array.
  - PTR-ARRAY must refer to preallocated data.  This array is modified.
  * TODO: Would this be safe to memoize--if that would help with speed?--or does (cudd-bdd-char-to-vect) make sure nodes are still living?
"
  (declare ;(node-pointer f-ptr)
   (foreign-pointer ptr-array)
   (sequence bdds)
   (manager-pointer manager-ptr))

  (let ((n (cudd-read-size manager-ptr)))
    (declare (uint n))

    ;; Initialization pass:
    (iter
      (for i from 0 below n)
      ;; (break "~D" i)
      (for g-ptr = (cudd-bdd-ith-var manager-ptr i))
      (declare ((or null node-pointer) g-ptr))
      (setf (mem-aref ptr-array node-pointer-t i) g-ptr))

    ;; Assignment pass:
    (iter
      (for g in-sequence bdds with-index i)
      (declare ((or null bdd-node) g))
      ;; (break "~D" i)
      (unless (null g)
        (let ((g-ptr (node-pointer g)))
          (declare (node-pointer g-ptr))
          (setf (mem-aref ptr-array node-pointer-t i) g-ptr)))))
  ptr-array)

(defun bdd-vector-compose (f v &optional (manager *manager*)
                           &aux (node-pointer-t :pointer))
  "If V has more elements than there are variables in MANAGER, the extras are ignored.

  - V should be a sequence of `bdd-node' objects.
"
  (declare (bdd-node f))
  (declare (sequence v))
  (declare (manager manager))

  (let ((res
          (wrap-and-finalize (let ((n (read-size manager))
                                   (manager-ptr (manager-pointer manager))
                                   (f-ptr (node-pointer f)))
                               (declare (uint n))
                               (declare (manager-pointer manager-ptr)
                                        (node-pointer f-ptr))

                               (with-foreign-object (ptr-array node-pointer-t n)

                                 (helper/bdd-seq-to-ptr-vector! ptr-array v manager-ptr)

                                 (let ((res-ptr (cudd-bdd-vector-compose manager-ptr f-ptr ptr-array)))
                                   (declare (node-pointer res-ptr))
                                   res-ptr)))
              'bdd-node)))
    (declare (bdd-node res))
    res))



(defun garbage-collect (&key ((:cache clear-cache?) t) (manager *manager*))
  "Runs the CUDD garbage collector.  According to the docs, ':cache nil' \"should only be specified if the cache has been cleared right before.\"
  - http://web.mit.edu/sage/export/tmp/y/usr/share/doc/polybori/cudd/cuddAllDet.html#cuddGarbageCollect
"
  (declare (boolean clear-cache?)
           (manager manager))
  (with-cudd-critical-section
    (let ((manager-ptr (manager-pointer manager))
          (clear-cache?/int (if clear-cache? 1 0)))
      (declare (manager-pointer manager-ptr)
               (fixnum clear-cache?/int))
      (let ((res (cudd-garbage-collect manager-ptr clear-cache?/int)))
        (declare (fixnum res))
        res))))


(defmacro with-C-file-pointer ((ptr pathname &key direction) &body body)
  "Utility for C file I/O.  Adapted from def. of 'dump-dot'.
  * TODO: ':if-exists', ':if-does-not-exist', as far as C can be relied upon
"
  (declare (symbol ptr)
           (type (member :input :output) direction))
  (check-type direction (member :input :output))

  (let ((dir (ecase direction
               (:input "r")
               (:output "w"))))
    (when (eq pathname '*stdout*)
      (not-implemented-error 'with-C-file-pointer/stdout "Don't know how to output to ~S.  Need to get a real `pathname'." '*stdout*))
    `(locally
         (declare (pathname-designator ,pathname))
       (let* ((filename (namestring (pathname ,pathname)))
              (,ptr (fopen filename ,dir)))
         (unwind-protect
              (progn
                ,@body)
           (fclose ,ptr))))))

(defgeneric dump-blif (pathname nodes &key)
  (:method (pathname-designator (nodes array) &rest keys)
    "Canonicalize PATHNAME-DESIGNATOR and recurse."
    (declare (pathname-designator pathname-designator))
    (assert (not (pathnamep pathname-designator)))
    (let ((pathname (ensure-absolute-pathname pathname-designator *default-pathname-defaults*)))
      (declare (pathname pathname))
      (apply #'dump-blif pathname nodes keys)))

  (:method ((pathname pathname) (nodes array) &key
                                                (manager *manager*))
    "Primary method.
  * TODO: Other keys.
"
    (let ((n (length nodes)))
      (declare (fixnum n))

      ;; KLUDGE: Copying pointers myself:
      (with-foreign-object (node-array :pointer n)
        (iter (for node in-vector nodes with-index i)
          (declare ((or null node) node) (fixnum i))
          (for node-ptr = (node-pointer node))
          (setf (mem-aref node-array :pointer i) node-ptr))

        (with-C-file-pointer (f pathname :direction :output)
          (let ((result-code (convert-from-foreign
                              (cudd-dump-blif (manager-pointer manager)
                                              (convert-to-foreign n :int)
                                              node-array
                                              ;; TODO:
                                              (null-pointer)
                                              (null-pointer)
                                              (null-pointer)
                                              f)
                              :int)))
            (ecase result-code
              (0 (error "Cudd_DumpBlif() reported failure"))
              (1
               (assert (file-exists-p pathname))
               t)))))))

  (:method (pathname (nodes sequence) &rest keys)
    (let ((element-type (type-of (elt nodes 0))))
      (assert (every (lambda (x) (typep x element-type)) nodes))
      (let ((array (coerce nodes 'array)))
        (declare (array array))
        (apply #'dump-blif pathname array keys))))

  (:method (pathname (node node) &rest keys)
    "Recurse."
    ;; (break)
    ;; (print node)
    (apply #'dump-blif pathname (vector node) keys)))

(defgeneric boolean-diff (f v &optional manager)
  (:documentation "CUDD's 'Boolean difference', also known as the Boolean, or logical, derivative.")
  (:method ((f bdd-node) v &optional (manager *manager*))
    "Wrapper for cudd_bddBooleanDiff()."
    (declare (manager manager))
    (declare (variable v))
    (let ((manager-ptr (manager-pointer manager))
          (f-ptr (node-pointer f)))
      (declare (foreign-pointer manager-ptr f-ptr))
      (let* ((res-ptr (cudd-bdd-boolean-diff manager-ptr f-ptr v))
             (res (wrap-and-finalize res-ptr 'bdd-node)))
        (declare (node-pointer res-ptr)
                 (bdd-node res))
        res))))

(defun compose (f g v &optional (manager *manager*))
  (declare (manager manager))
  (declare (bdd-node f))
  (declare (bdd-node g))
  (declare (type (and fixnum (integer 0)) v))
  (let ((manager-ptr (manager-pointer manager))
        (f-ptr (node-pointer f))
        (g-ptr (node-pointer g)))
    (let* ((res-ptr (cudd-bdd-compose manager-ptr f-ptr g-ptr v))
           (res (wrap-and-finalize res-ptr 'bdd-node)))
      (declare (node-pointer res-ptr)
               (bdd-node res))
      res)))

;; XXX #+sbcl (declaim (maybe-inline support-index-int-series))
(defun support-index-int-series (node &optional (manager *manager*)
                             &aux (C-array-element :int))
  "Return a `series:series' whose length is Cudd_ReadSize() with one :int \in {0,1} per var."
  (declare (node node) (manager manager))
  (let* ((manager-ptr (manager-pointer manager))
         (total-num-vars (cudd-read-size manager-ptr))
         (int-array-ptr (cudd-support-index manager-ptr (node-pointer node))))
    (declare (foreign-pointer manager-ptr int-array-ptr)
             (type (and fixnum (integer 0)) total-num-vars))
    (let ((res (map-fn '(and fixnum (member 0 1))
                   (lambda (i)
                     (declare (fixnum i))
                     (let ((b_i (mem-aref int-array-ptr C-array-element i)))
                       (declare (fixnum b_i))
                       ;; (the boolean (= b_i 1))
                       b_i))
                   (scan-range :from 0 :upto total-num-vars))))
      (declare (series res))
      res)))

(defun support-index (node &optional (manager *manager*))
  "Returns a series whose length is Cudd_ReadSize().  Each element is T iff the variable with that index is in the support of NODE.
  * TODO: Perhaps inline (support-index-int-series)?
"
  (declare (manager manager))
  (declare (node node))
  (let* ((int-series (support-index-int-series node manager))
         (res (map-fn 'boolean
                     (lambda (b_i)
                       (declare (type (and fixnum (member 0 1))  b_i))
                       (the boolean (= b_i 1)))
                     int-series)))
    (declare (series int-series res))
    res)
  ;; (let* ((manager-ptr (manager-pointer manager))
  ;;        (total-num-vars (cudd-read-size manager-ptr))
  ;;        (int-array-ptr (cudd-support-index manager-ptr (node-pointer node))))
  ;;   (declare (foreign-pointer manager-ptr int-array-ptr)
  ;;            (type (and fixnum (integer 0)) total-num-vars))
  ;;   (iter
  ;;     (with bitv = (make-array total-num-vars :element-type 'boolean :initial-element nil :adjustable nil))
  ;;     (for i below total-num-vars)
  ;;     (declare (fixnum i))
  ;;     (for b_i = (mem-aref int-array-ptr C-array-element i))
  ;;     (declare (fixnum b_i))
  ;;     (assert (member b_i '(0 1) :test #'=))
  ;;     (setf (aref bitv i) (= b_i 1))
  ;;     (finally (return bitv))))
  ); support-index

(defun support-size (node &optional (manager *manager*))
  "See Cudd_SupportSize()."
  (declare (manager manager))
  (declare (node node))
  (cudd-support-size (manager-pointer manager)
                     (node-pointer node)))

(defun sharing-size (nodes)
  "Wrapper for (cudd-sharing-size).  NODES should be a collection of nodes/BDDs.  Note that a `manager' parameter isn't useful to CUDD for this function!

  * TODO: Get the 'n' count from (read-size)?
  * TODO: [opt] with ':series'?
"
  (declare (sequence nodes))
  (let ((n (length nodes)))
    (with-foreign-object (node_arr :pointer n)
      (iter
        (for (the node node) in-sequence nodes with-index i)
        (for node_ptr = (node-pointer node))
        (setf (mem-aref node_arr :pointer i) node_ptr))
      (let ((result (cudd-sharing-size node_arr n)))
        (declare (fixnum result))
        result))))


(defun print-debug (node &key
                           (manager *manager*)
                           ((:n num-vars) (bdd-variables manager))
                           (level 1))
  "* TODO: Raise custom exception on failure.
 * TODO: Print to *standard-output*, not cstdout.
"
  (declare (node node)
           (manager manager)
           (fixnum num-vars)
           (fixnum level))
  (check-type num-vars (integer 0))
  (check-type level (integer 0))
  (let ((errcode (cudd-print-debug (manager-pointer manager)
                                   (node-pointer node)
                                   num-vars level)))
    (declare (type integer errcode))
    (if (= 1 errcode) nil
        (error "Cudd_PrintDebug() failed"))))

(defun print-info (&optional (manager *manager*) (pathname "cudd.info"))
  "Delegate to (cudd.baseapi:print-info).
  * TODO: Better default filename...
"
  (declare (manager manager)
           (type (or string pathname) pathname))
  (cl-cudd.baseapi:print-info (manager-pointer manager) pathname))

#|
(defgeneric print-info (manager pathname)
(:documentation "Delegate to (cudd.baseapi:print-info).")
(:method ((manager manager) pathname)
"Recurse."
(print-info (manager-pointer manager) pathname))
(:method (manager pathname)
(declare (foreign-pointer manager)
((or string pathname) pathname))
(cl-cudd.baseapi:print-info manager pathname)))
|#

(defun info (&optional (manager *manager*))
  (declare (type manager manager))
  (with-temporary-file (:stream s :pathname path)
    (print-info manager path)
    (slurp-stream-string s)))

(defgeneric eval (dd inputs &optional manager)
  (:documentation "TODO: Other DD types.")
  (:argument-precedence-order inputs dd) ; convert INPUTS first
  (:method (dd (inputs sequence) &optional (manager *manager*))
    "Coerce SEQUENCE, which should consist of generalized-booleans, to a C int array, and recurse."
    (declare (type node dd))
    (let (;; (int-sz (foreign-type-size :int))
          (num-inputs (length inputs)))
      (with-foreign-object (input-arr :int num-inputs)
        (iter
          (for b in-sequence inputs with-index i)
          (declare (generalized-bit b))
          ;; TODO: Initialize to 0:
          (for b* = (the bit (ecase b
                               ((0 nil) 0)
                               ((1 t) 1))))
          (declare (bit b*))
          (setf (mem-aref input-arr :int i) b*))
        (eval dd input-arr manager))))
  (:method ((bdd bdd-node) input-arr &optional (manager *manager*))
    (declare (foreign-pointer input-arr))
    (let ((res-ptr (cudd-eval (manager-pointer manager) (node-pointer bdd) input-arr)))
      (declare (foreign-pointer res-ptr))
      ;; TODO (check-result-value res-ptr)
      ;; (call-next-method res-ptr input-arr manager)
      (let ((res (wrap-and-finalize res-ptr 'bdd-node)))
        (declare (bdd-node res))
        res)))
  ;; (:method (ptr input-arr &optional (manager *manager*))
  ;;	(declare (foreign-pointer ptr))
  ;;	(declare (foreign-pointer input-arr))
  ;;	(let ((res (make-bdd-node :pointer res-ptr)))
  ;;    (declare (bdd-node res))
  ;;    res))
  )

(defun map-ones (node fn)
  "Runs a DFS on a ZDD. It calls the given callback function when it reaches the 1-node.
The callback is called with an argument containing a bit vector which stores 1-bit.
Returns the node."
  (let ((bv (make-array (zdd-max-variables) :element-type 'bit))
        (one (cudd-read-one %mp%))
        (zero (cudd-read-zero %mp%)))
    (labels ((rec (p)
               (cond
                 ((pointer-eq p one)
                  (funcall fn bv))
                 ((pointer-eq p zero)
                  ;; do nothing
                  )
                 ((cudd-is-non-constant p)
                  (let ((index (cudd-node-read-index p)))
                    (setf (aref bv index) 1)
                    (rec (cudd-node-then p))
                    (setf (aref bv index) 0)
                    (rec (cudd-node-else p)))))))
      (rec (node-pointer node))))
  node)

(defmacro do-ones ((var dd) &body body)
  "Runs a DFS on a ZDD. BODY is executed for each path in a ZDD to the constant 1-node.
Entire body is wrapped in a block NIL.
Symbol VAR is lexically bound to a bit vector which stores 1-bit when a zdd variable is true on the path.
Returns the node."
  `(block nil
     (map-ones ,dd (lambda (,var) ,@body))))


(defun integer->zdd-unate (int)
  "Converts an integer to a zdd bit-by-bit."
  (declare (integer int))
  (let ((zdd (zdd-set-of-emptyset)))
    (dotimes (i (integer-length int) zdd)
      (when (logbitp i int)
        (setf zdd (zdd-change zdd i))))))

(defun integer->zdd-binate (int)
  "Converts an integer to a zdd bit-by-bit, in a binate representation. (ith bit is encoded into 2i and 2i+1 bits)"
  (declare (integer int))
  (let ((zdd (zdd-set-of-emptyset)))
    (dotimes (i (integer-length int) zdd)
      (setf zdd (zdd-change zdd (if (logbitp i int)
                                    (* i 2)
                                    (1+ (* i 2))))))))

(setf (fdefinition 'integer->zdd) #'integer->zdd-unate)

(defun bitvector->zdd (bv)
  "Converts a bit-vector to a zdd bit-by-bit."
  (declare (bit-vector bv))
  (let ((zdd (zdd-set-of-emptyset)))
    (dotimes (i (length bv) zdd)
      (when (= 1 (aref bv i))
        (setf zdd (zdd-change zdd i))))))

(defun follow-diagram (node thing)
  "Follow the decision diagram according to the bits in THING, which is either an integer or a bit vector.
Follow the then-branch when 1, else-branch otherwise."
  (let ((p (node-pointer node)))
    (etypecase thing
      (integer
       (iter (for index = (cudd-node-read-index p))
         (while (< index (integer-length thing)))
         (setf p
               (if (logbitp index thing)
                   (cudd-node-then p)
                   (cudd-node-else p)))))
      (bit-vector
       (iter (for index = (cudd-node-read-index p))
         (while (< index (length thing)))
         (setf p
               (if (plusp (aref thing index))
                   (cudd-node-then p)
                   (cudd-node-else p))))))
    (etypecase node
      (add-node (wrap-and-finalize p 'add-node))
      (bdd-node (wrap-and-finalize p 'bdd-node))
      (zdd-node (wrap-and-finalize p 'zdd-node)))))


(defun cudd-print (node &optional (manager *manager*))
  "Print a DD to cstdout.  See cuddP().  Returns T on success, 0 on failure.
  * TODO: Print to Lisp *stdout*.
  * TODO: Raise exception?
"
  (declare (manager manager)
           (node node))
  (= 1 (the fixnum (cuddp (manager-pointer manager) (node-pointer node)))))

(defun count-dead-bdd-nodes (&optional (manager *manager*))
  "Return an int: number of currently dead ADD|BDD nodes."
  (declare (manager manager))
  (let* ((ptr (manager-pointer manager))
         (dead (cudd-read-dead ptr)))
    (declare (manager-pointer ptr))
    (declare (fixnum dead))
    dead))

(defun count-live-bdd-nodes (&optional (manager *manager*))
  "Return an int: the total number of live ADD|BDD nodes."
  (declare (manager manager))
  (let ((ptr (manager-pointer manager)))
    (declare (manager-pointer ptr))
    (let ((total (cudd-read-keys ptr))
          (dead (cudd-read-dead ptr)))
      (declare (fixnum total dead))
      (let ((live (- total dead)))
        (declare (fixnum live))
        (assert (>= live 0))
        live))))

(defun bdd-transfer (bdd &key (src *manager*) dest)
  "Note that the argument order isn't the same as C.
  * TODO: What's the right argument for the 'ref' parameter of (wrap-and-finalize)?
  * TODO: What if SRC ≡ DEST?
"
  (declare (bdd-node bdd)
           (manager src)
           (manager dest))
  (if (eql src dest)
      (not-implemented-error 'vacuous-bdd-transfer "'src' and 'dest' args to (bdd-transfer) must be different.")
      (let ((*manager* dest))
        (with-cudd-critical-section (:manager dest)
          ;; (not-implemented-error 'bdd-transfer)
          (let ((bdd-ptr (cudd-bdd-transfer (manager-pointer src) (manager-pointer dest) (node-pointer bdd))))
            (declare (type node-pointer bdd-ptr))
            (the bdd-node (wrap-and-finalize bdd-ptr 'bdd-node)))))))


(defun dump-factored-form (nodes &key
                                   (fp *stdout*)
                                   (manager *manager*)
                                   ((:n max-nodes) (sharing-size nodes))
                                   (inames nil)
                                   (onames nil))
  "If the underlying CUDD call fails, raises an exception.
  By default, writes to *STDOUT*.
  - Note: Use (with-C-file-pointer) to get an arg for FP."
  (declare (sequence nodes)
           (manager manager)
           (fixnum max-nodes)
           ;; XXX (foreign-pointer fp)
           )
  (let ((errcode (cudd-dump-factored-form (manager-pointer manager)
                                      max-nodes
                                      goddamn-array ; FIXME--convert to C array
                                      (if inames (not-implemented-error 'inames) (null-pointer))
                                      (if onames (not-implemented-error 'onames) (null-pointer))
                                      fp)))
    (declare (type (member 0 1) errcode))
    (ecase errcode
      (1 t)
      (0 (error "Cudd_DumpFactoredForm() failed.  TODO: args passed to (dump-factored-form).")))))
