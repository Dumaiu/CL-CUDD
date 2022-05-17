;;; set operations for zdd

;;; NOTE: On the unary functions: We don't need to enter the crit. sec. early (if at all).  The node argument should have a reference to its own manager; therefore neither should run out of extent before the operation ends.
;; TODO: For the same reason, (define-zdd-binary-op) doesn't use (with-cudd-critical-section).

(in-package :cudd)

(defmacro zdd-ref-let* (bindings &body body)
  "
The purpose of using this macro is to defining a high-level operations on zdd without
instantiating the lisp node objects. For internal use only.

Bind temporary variables like let*, call cudd-ref, execute the body,
then cleanup the nodes with cudd-recursive-deref-zdd in unwind-protect.

However each binding may have an optional third element i.e. (var val no-deref).
When no-deref evaluates to non-nil, it does not call cudd-recursive-deref-zdd.
This is useful for returning a meaningful node.

Finally, a binding could be just t, which means the value of this form is
dereferenced (cudd-deref) once AFTER all unwind-protect form is exited.

This corresponds to the programming pattern that appears on CUDD manual, as follows:

tmp = Cudd_ReadZddOne(manager,0);
Cudd_Ref(tmp);
for (i = 3; i >= 0; i--) {
   var = Cudd_zddIthVar(manager,i);
   Cudd_Ref(var);
   f = Cudd_zddIntersect(manager,var,tmp);
   Cudd_Ref(f);
   Cudd_RecursiveDerefZdd(manager,tmp);
   Cudd_RecursiveDerefZdd(manager,var);
   tmp = f;
}
f = Cudd_zddDiff(manager,Cudd_ReadZddOne(manager,0),tmp);
Cudd_Ref(f);
Cudd_RecursiveDerefZdd(manager,tmp);

Cudd_Deref(f); // if you need a function that returns f with refcount zero

In our case, a single loop of this example roughly translates to the following form:

(zdd-ref-let* (t
               (tmp (cudd-read-zdd-one %mp% 0))
               (var (cudd-zdd-ith-var %mp% i))
               (f   (cudd-zdd-intersect %mp% var tmp)
                    t))
  f)

Notice that:
+ Temporary variables are recursively deref'ed by unwind-protect.
+ f is marked not recursively deref'ed, by (f (cudd-zdd-intersect %mp% var tmp) t).
+ Using t in the binding, thus deref f once (non recursively) when exit.
"
  (ematch bindings
    (nil `(progn ,@body))
    ((list* 't rest)
     (with-gensyms (res)
       `((lambda (,res)
           (cudd-deref ,res)
           ,res)
         (zdd-ref-let* ,rest ,@body))
       ;; `(let ((,res (zdd-ref-let* ,rest ,@body)))
       ;;    (cudd-deref ,res)
       ;;    ,res)
       ))
    ((list* (list var form) rest)
     `(let ((,var ,form))
        (cudd-ref ,var)
        (unwind-protect (zdd-ref-let* ,rest ,@body)
          (cudd-recursive-deref-zdd %mp% ,var))))
    ((list* (list var form) rest)
     `(let ((,var ,form))
        (cudd-ref ,var)
        (unwind-protect (zdd-ref-let* ,rest ,@body)
          (cudd-recursive-deref-zdd %mp% ,var))))
    ((list* (list var form t) rest)
     `(let ((,var ,form))
        (cudd-ref ,var)
        (zdd-ref-let* ,rest ,@body)))
    ((list* (list var form no-deref) rest)
     `(let ((,var ,form))
        (cudd-ref ,var)
        (if ,no-deref
            (zdd-ref-let* ,rest ,@body)
            (unwind-protect (zdd-ref-let* ,rest ,@body)
              (cudd-recursive-deref-zdd %mp% ,var)))))))

;;;; elementary sets

;; TODO:
#|(declaim (maybe-inline zdd-emptyset
zdd-set-of-emptyset
zdd-singleton
zdd-subset-0
zdd-subset-1
))
|#
(declaim (reentrant zdd-emptyset))
(defun zdd-emptyset (&key (manager *manager*))
  "Returns an empty set {}."
  (declare (manager manager))
  (zero-node 'zdd-node :manager manager))

(declaim (reentrant zdd-set-of-emptyset))
(defun zdd-set-of-emptyset (&key (manager *manager*))
  "Returns a set of an empty set {{}}."
  (declare (manager manager))
  (one-node 'zdd-node :manager manager))

(declaim (reentrant zdd-singleton))
(defun zdd-singleton (var &key (manager *manager*))
  "Returns {{var}}. This is not equivalent to (make-var 'zdd-node :index var), see make-var documentation."
  (declare (manager manager))
  (zdd-change (zdd-set-of-emptyset) var :manager manager))

;;;; between a ZDD and a single variable

(declaim (reentrant zdd-subset-0
                    zdd-subset-1))
(defun zdd-subset-0 (zdd var &key ((:manager m) (node-manager zdd)))
  "Computes the subset of S that does not contain element VAR (integer)."
  (declare (zdd-node zdd)
           (manager m))
  (let-1 mp (manager-pointer m)
    (declare (manager-pointer mp))
    (assert* (not (null-pointer-p mp)))
    (wrap-and-finalize (cudd-zdd-subset-0 mp (node-pointer zdd) var)
        'zdd-node
      :manager m)))
(defun zdd-subset-1 (zdd var &key ((:manager m) (node-manager zdd)))
  "Computes the subset of S that contains element VAR (integer), and remove VAR from each combination."
  (declare (zdd-node zdd)
           (manager m))
  (let-1 mp (manager-pointer m)
    (declare (manager-pointer mp))
    (assert* (not (null-pointer-p mp)))
    (wrap-and-finalize (cudd-zdd-subset-1 (manager-pointer m) (node-pointer zdd) var)
        'zdd-node
      :manager m)))

(declaim (reentrant zdd-change))
(defun zdd-change (zdd var &key ((:manager m)  (node-manager zdd)))
  "Flip the membership of variable VAR in ZDD.

  TODO: Remove :manager arg, since it's an error if doesn't match (node-manager ZDD).
"
  (declare (zdd-node zdd)
           (manager m))
  (let-1 mp (manager-pointer m)
    (declare (manager-pointer mp))
    (assert* (not (null-pointer-p mp)))
    (wrap-and-finalize
        (cudd-zdd-change mp (node-pointer zdd) var)
        'zdd-node
      :manager m)))

(declaim (reentrant zdd-set))
(defun zdd-set (zdd var &key ((:manager m) (node-manager zdd)))
  "Add a variable VAR; i.e. force the value of VAR to be true"
  (declare (zdd-node zdd)
           (manager m))
  (let-1 mp (manager-pointer m)
    (declare (type manager-pointer mp))
    (assert* (not (null-pointer-p mp)))
    (wrap-and-finalize
        (zdd-ref-let* (t
                       (then (cudd-zdd-subset-1 mp (node-pointer zdd) var))
                       (else (cudd-zdd-subset-0 mp (node-pointer zdd) var))
                       (union (cudd-zdd-union mp then else))
                       (result (cudd-zdd-change mp union var) t))
          result)
        'zdd-node
      :manager m)))

(declaim (reentrant zdd-unset))
(defun zdd-unset (zdd var &key ((:manager m) (node-manager zdd)))
  "Remove a variable VAR; i.e. force the value of VAR to be false"
  (declare (zdd-node zdd)
           (manager m))
  (let-1 mp (manager-pointer m)
    (declare (manager-pointer mp))
    (assert* (not (null-pointer-p mp)))
    (wrap-and-finalize
        (zdd-ref-let* (t
                       (then (cudd-zdd-subset-1 mp (node-pointer zdd) var))
                       (else (cudd-zdd-subset-0 mp (node-pointer zdd) var))
                       (union (cudd-zdd-union mp then else) t))
          union)
        'zdd-node
      :manager m)))

(declaim (reentrant zdd-dont-care))
(defun zdd-dont-care (zdd var &key ((:manager m) (node-manager zdd)))
  "Direct the both arcs of the VAR'th node to the next index.
If it does not exist (i.e. then-arc points to 0 and zero-suppressed) creates a new node."
  (declare (zdd-node zdd)
           (manager m))
  (let-1 mp (manager-pointer m)
    (declare (manager-pointer mp))
    (assert* (not (null-pointer-p mp)))
    (wrap-and-finalize
        (zdd-ref-let* (t
                       (then (cudd-zdd-subset-1 mp (node-pointer zdd) var))
                       (else (cudd-zdd-subset-0 mp (node-pointer zdd) var))
                       (union (cudd-zdd-union mp then else))
                       (flipped (cudd-zdd-change mp union var))
                       (result (cudd-zdd-union mp union flipped) t))
          result)
        'zdd-node
      :manager m)))

;;;; between 2 ZDDs

(defmacro define-zdd-binary-op (name (f-name g-name &key
                                                      ((:manager manager-name) 'm)
                                                      ((:manager-pointer manager-pointer-name) '%mp%))
                                &body body)
  "Helper for generating dyadic ZDD functions.
 (wrap-and-finalize) is automatically included.

  MANAGER-POINTER-NAME : Only here for convenience.
"
  (declare (type (and symbol (not null) (not keyword))
                 name f-name g-name manager-name manager-pointer-name))
  (let (docstring! eval-form!)
    (ematch body
      ((list docstring form)
       (check-type docstring string)
       (setq docstring! docstring)
       (setq eval-form! form))
      ((list form)
       (setq eval-form! form)))

    `(progn
       (declaim (maybe-inline ,name))
       (declaim (reentrant ,name))
       (defun ,name (,f-name ,g-name &key ((:manager ,manager-name) (node-manager ,f-name)))
         ,@(when docstring! `(,docstring!))
         (declare (zdd-node ,f-name ,g-name)
                  (manager ,manager-name))
         (unless (and (eq ,manager-name (node-manager ,f-name))
                      (eq ,manager-name (node-manager ,g-name)))
           (error 'cudd-manager-mismatch-error "Managers in a binary op need to match"))
         (let-1 ,manager-pointer-name (manager-pointer ,manager-name)
           (declare (manager-pointer ,manager-pointer-name))
           (assert* (not (null-pointer-p ,manager-pointer-name)))
           (wrap-and-finalize (the node-pointer ,eval-form!) 'zdd-node
             :manager ,manager-name))))))

(define-zdd-binary-op zdd-union (f g)
  "Computes the union of F and G."
  (cudd-zdd-union %mp% (node-pointer f) (node-pointer g)))

;; (defun zdd-union (f g)
;;   "Computes the union of F and G."
;;   (wrap-and-finalize
;;       (cudd-zdd-union %mp% (node-pointer f) (node-pointer g))
;;       'zdd-node))

(define-zdd-binary-op zdd-intersection (f g)
  "Computes the intersection of F and G."
  (cudd-zdd-intersect %mp% (node-pointer f) (node-pointer g)))

(defun zdd-union* (&rest args)
  "Performs zdd-union on all variables."
  (cond
    ((second args)
     (reduce #'zdd-union args))
    (args
     (first args))
    ((null args)
     (zdd-emptyset))))

(defun zdd-intersection* (first &rest args)
  "Performs zdd-intersection on all variables.
Null intersection (union of all combinations) is undefined because
ZDD has no upper limit on the number of variables."
  (if args
      (reduce #'zdd-intersection args :initial-value first)
      first))


(define-zdd-binary-op zdd-difference (f g)
  "Computes the difference of F and G."
  (cudd-zdd-diff %mp% (node-pointer f) (node-pointer g)))

;;;; unate operations

(setf (fdefinition 'zdd-onset) #'zdd-subset-1
      (documentation 'zdd-onset 'function)
      "Computes the subset of S that contains element VAR (integer), and remove VAR from each combination. (same as zdd-subset-1)")

(setf (fdefinition 'zdd-offset) #'zdd-subset-0
      (documentation 'zdd-offset 'function)
      "selects the subset of the combinations each of which does not include var. (same as zdd-subset-1)")

(define-zdd-binary-op zdd-divide-unate (f g)
  "Computes the weak division of F by G (assumes unate representation).
cf. Shin-ichi Minato: Zero-Suppressed BDDs and Their Applications"
  (cudd-zdd-divide %mp% (node-pointer f) (node-pointer g)))

(define-zdd-binary-op zdd-product-unate (f g)
  "Computes the product of F by G (assumes unate representation).
cf. Shin-ichi Minato: Zero-Suppressed BDDs and Their Applications"
  (cudd-zdd-unate-product %mp% (node-pointer f) (node-pointer g)))

(define-zdd-binary-op zdd-remainder-unate (f g)
  "Computes the remainder of division of F by G (assumes unate representation)."
  (zdd-ref-let* (t
                 (p1 (cudd-zdd-divide %mp% (node-pointer f) (node-pointer g)))
                 (p2 (cudd-zdd-unate-product %mp% (node-pointer f) p1))
                 (p3 (cudd-zdd-diff %mp% (node-pointer f) p2) t))
    p3))

;; aliasing
(setf (fdefinition 'zdd-product) #'zdd-product-unate)
(setf (fdefinition 'zdd-divide) #'zdd-divide-unate)
(setf (fdefinition 'zdd-remainder) #'zdd-remainder-unate)

;;;; binate operations

(define-zdd-binary-op zdd-divide-binate (f g)
  "Computes the weak division of F by G (assumes binate representation).
cf. Shin-ichi Minato: Zero-Suppressed BDDs and Their Applications"
  (cudd-zdd-weak-div %mp% (node-pointer f) (node-pointer g)))

(define-zdd-binary-op zdd-product-binate (f g)
  "Computes the product of F by G (assumes binate representation).
cf. Shin-ichi Minato: Zero-Suppressed BDDs and Their Applications"
  (cudd-zdd-product %mp% (node-pointer f) (node-pointer g)))

(define-zdd-binary-op zdd-remainder-binate (f g)
  "Computes the remainder of division of F by G (assumes binate representation)."
  (zdd-ref-let* (t
                 (p1 (cudd-zdd-weak-div %mp% (node-pointer f) (node-pointer g)))
                 (p2 (cudd-zdd-product %mp% (node-pointer f) p1))
                 (p3 (cudd-zdd-diff %mp% (node-pointer f) p2) t))
    p3))

(defun zdd-count-minterm (f &optional support-size &key  ((:manager m) (node-manager f)))
  "Computes the number of minterms in f.
SUPPORT-SIZE specifies the number of variables in the support of f, i.e.,
the number of the variables that F essentially depends on."
  (declare (manager m))
  (with-cudd-critical-section (:manager m)
    (if support-size
        (cudd-zdd-count-minterm (manager-pointer m) (node-pointer f) support-size)
        (cudd-zdd-count-double (manager-pointer m) (node-pointer f)))))


;;;; reimplementing set operations in Extra package

#|

NOTE: use the native CUDD cache.

|#

(let ((op (new-cached-operator 2)))
  (defun cudd-zdd-supset (dd f g)
    (let* ((one (cudd-read-one dd))
           (zero (cudd-read-zero dd)))
      (labels ((rec (f g)
                 (macrolet ((retnull (x)
                              `(when (null-pointer-p ,x) (return-from rec ,x))))
                   (cond
                     ((pointer-eq f g)    f)
                     ((pointer-eq zero f) zero)
                     ((pointer-eq zero g) zero)
                     ((pointer-eq one f)  one)
                     ((pointer-eq one g)
                      (if (cudd-zdd-empty-belongs dd f) one zero))
                     (t
                      (let ((zres (cudd-cache-lookup-2-zdd dd op f g)))
                        (when (not (null-pointer-p zres))
                          (return-from rec zres)))
                      (let ((i1 (cudd-node-level-zdd dd f))
                            (i2 (cudd-node-level-zdd dd g)))
                        (cond
                          ((< i1 i2)
                           ;; all NULLs are derived from external function calls, but they
                           ;; are already handled by type translators.
                           (rec (cudd-node-else f) g))
                          ((= i1 i2)
                           (zdd-ref-let* (t ; returned with reference 0
                                          (ztmp (cudd-zdd-union dd (cudd-node-then g) (cudd-node-else g)))
                                          (zres0 (rec (cudd-node-else f) ztmp))
                                          (zres1 (rec (cudd-node-then f) (cudd-node-then g)))
                                          (zres (cudd-zdd-get-node dd (cudd-node-index f) zres1 zres0) t))
                             ;; cudd-zdd-get-node / cudd-zdd-unique-inter increases the zres0/zres1 refcount.
                             ;; since we already called cudd_ref on them, this effect should be cancelled
                             (cudd-deref zres0)
                             (cudd-deref zres1)
                             (cudd-cache-insert-2 dd op f g zres)
                             zres))      ;is returned referenced
                          (t
                           (zdd-ref-let* (t ; returned with reference 0
                                          (ztmp (cudd-zdd-union dd (cudd-node-then g) (cudd-node-else g)))
                                          (zres (rec f ztmp) t))
                             (cudd-cache-insert-2 dd op f g zres)
                             zres)))))))))
        (rec f g)))))


(define-zdd-binary-op zdd-supset (f g)
  "Returns the subset of F whose element is a superset of at least one element of G. {p ∈ P | ∃q ∈ Q p ⊇ q}

Coudert, Olivier, Jean Christophe Madre, and Henri Fraisse. \"A new viewpoint on two-level logic minimization.\"
Design Automation, 1993. 30th Conference on.

Reference implementation is available in Extra libnrary by Alan Mishchenko.
https://people.eecs.berkeley.edu/~alanmi/research/extra/
"
  (cudd-zdd-supset %mp% (node-pointer f) (node-pointer g)))

;; (defun zdd-maximal (f)
;;   (wrap-and-finalize
;;    'zdd-node t nil))

;; (defun zdd-minimal (f)
;;   (wrap-and-finalize
;;    'zdd-node t nil))

;; (defun zdd-subset (f)
;;   (wrap-and-finalize
;;    'zdd-node t nil))
