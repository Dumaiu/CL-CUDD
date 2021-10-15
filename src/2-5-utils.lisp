(in-package :cudd)

(deftype node-pointer ()
  'foreign-pointer)

(deftype variable ()
  '(integer 0))

(defgeneric boolean-diff (f v &optional manager)
  (:documentation "CUDD's 'Boolean difference', also known as the Boolean, or logical, derivative.")
  (:method ((f bdd-node) v &optional (manager *manager*))
	"Wrapper for cudd_bddBooleanDiff()."
	(declare (manager manager))
	(check-type v variable)
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
  (check-type f bdd-node)
  (check-type g bdd-node)
  (check-type v (integer 0))
  (let ((manager-ptr (manager-pointer manager))
		(f-ptr (node-pointer f))
		(g-ptr (node-pointer g)))
	(let* ((res-ptr (cudd-bdd-compose manager-ptr f-ptr g-ptr v))
		   (res (wrap-and-finalize res-ptr 'bdd-node)))
	  (declare (node-pointer res-ptr)
			   (bdd-node res))
	  res)))

(defun support-index (node &optional (manager *manager*)
					  &aux (C-array-element :int))
  "Returns a bit-vector whose length is Cudd_ReadSize().  Each element is T iff the variable with that index is in the support of NODE."
  (declare (manager manager))
  (check-type node node)
  (let* ((manager-ptr (manager-pointer manager))
		 (total-num-vars (cudd-read-size manager-ptr))
		 (int-array-ptr (cudd-support-index manager-ptr (node-pointer node))))
	(declare (foreign-pointer manager-ptr int-array-ptr)
			 (type (integer 0) total-num-vars))
	(iter
	  (with bitv = (make-array total-num-vars :element-type 'boolean :initial-element nil :adjustable nil))
	  (for i below total-num-vars)
	  (for b_i = (mem-aref int-array-ptr C-array-element i))
	  (declare (integer b_i))
	  (assert (member b_i '(0 1) :test #'=))
	  (setf (aref bitv i) (= b_i 1))
	  (finally (return bitv)))))

(defun support-size (node &optional (manager *manager*))
  "See Cudd_SupportSize()."
  (declare (manager manager))
  (check-type node node)
  (cudd-support-size (manager-pointer manager)
					 (node-pointer node)))

(defun sharing-size (nodes #|&optional (manager *manager*)|#)
  "Wrapper for (cudd-sharing-size).  NODES should be a collection of nodes/BDDs."
  (check-type nodes sequence)
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
  (declare (manager manager))
  (check-type node node)
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

(deftype generalized-bit ()
  '(or boolean (member 0 1)))

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
		  (check-type b generalized-bit)
		  (for b* = (the bit (case b
							   ((0 nil) 0)
							   ((1 t) 1))))
		  (declare (bit b*))
		  (setf (mem-aref input-arr :int i) b*))
		(eval dd input-arr manager))))
  (:method ((bdd bdd-node) input-arr &optional (manager *manager*))
	(check-type input-arr foreign-pointer)
	(let ((res-ptr (cudd-eval (manager-pointer manager) (node-pointer bdd) input-arr)))
	  (declare (foreign-pointer res-ptr))
	  ;; TODO (check-result-value res-ptr)
	  ;; (call-next-method res-ptr input-arr manager)
	  (let ((res (wrap-and-finalize res-ptr 'bdd-node)))
		(declare (bdd-node res))
		res)))
  ;; (:method (ptr input-arr &optional (manager *manager*))
  ;;	(check-type ptr foreign-pointer)
  ;;	(check-type input-arr foreign-pointer)
  ;;	(let ((res (make-bdd-node :pointer res-ptr)))
  ;;	  (declare (bdd-node res))
  ;;	  res))
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
  (= 1 (cuddp (manager-pointer manager) (node-pointer node))))
