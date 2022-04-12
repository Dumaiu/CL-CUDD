;;; base class definitions and macros for defining APIs
(in-package :cudd)

;;; Utilities for def-cudd-call

(export '(cudd-manager-mismatch-error))

(define-condition cudd-manager-mismatch-error (cudd-error #|TODO: CL type|#) ())

(defun generic-cudd-function (generic-name arguments generic-docu)
  (flet ((clean-arguments (arguments)
           (mapcar (lambda (arg)
                     (if (and (listp arg) (eq (second arg) :node))
                         (first arg)
                         arg))
                   arguments)))
    `(defgeneric ,generic-name ,(clean-arguments arguments)
       ,@(when generic-docu
           `((:documentation ,generic-docu))))))

(defun node-function (generic-name arguments native-function node-type
                      dont-wrap-result)
  "TODO: Add `manager' param"
  (declare (optimize debug))
  (with-gensyms (mp)
   (labels ((convert-arguments (arguments)
              (mapcar (lambda-match
                        ((list var :node)
                         (list var node-type))
                        (arg arg))
                      arguments))
            (clean-arguments (arguments)
              "For nodes retrieve (node-pointer)."
              (mapcar (lambda-match
                        ((list var :node)
                         `(node-pointer ,var))
                        (arg arg))
                      arguments))
            (node-names (arguments)
              (iter
                (for arg in arguments)
                (match arg
                  ((list var :node)
                   (collecting var into nodes)))
                (finally (return nodes))))
            (make-funcall (native arguments)
              `(,native ,mp ; anaphoric
                        ,@(clean-arguments arguments))))
     (with-gensyms (pointer
                    managers
                    manager)
       ;; (assert* (>= (length arguments) 2)) ; TODO: See if it works for unaries.
       (let ((converted-arguments (convert-arguments arguments))
             (node-names (node-names arguments)))
         (declare (optimize debug))
         ;; (break "converted-arguments: ~A" converted-arguments)
         ;; (break "~A" node-names)
         `(defmethod ,generic-name ,converted-arguments
            ;; TODO: (mapcar #'node-manager)?
            (let ((,managers (mapcar #'manager (list ,@node-names))))
              (unless (reduce #'eq ,managers)
                (assert* (>= (length ,managers) 2))
                (error 'cudd-manager-mismatch-error "Nodes ~A didn't have the same manager.  Managers found: ~A"
                       ',node-names
                       ,managers))
              (let* ((,manager (first ,managers))
                     (,mp (manager-pointer ,manager)))
                (declare (manager ,manager)
                         (manager-pointer ,mp))
                (with-cudd-critical-section (:manager ,manager)
                  ,(let-1 funcall-form (make-funcall native-function arguments)
                     (if dont-wrap-result
                         funcall-form
                         `(let* ((,pointer ,funcall-form))
                            (declare (node-pointer ,pointer))
                            (wrap-and-finalize ,pointer ',node-type
                                               :manager ,manager)))))))))))))

(defun add-function (generic-name arguments add-function dont-wrap)
  (node-function generic-name arguments add-function 'add-node dont-wrap))
(defun bdd-function (generic-name arguments bdd-function dont-wrap)
  (node-function generic-name arguments bdd-function 'bdd-node dont-wrap))
(defun zdd-function (generic-name arguments zdd-function dont-wrap)
  (node-function generic-name arguments zdd-function 'zdd-node dont-wrap))
(defun common-function (generic-name arguments function dont-wrap)
  (node-function generic-name arguments function t dont-wrap))

(defun set-add-docu (add-function add-docu)
  (when (symbolp add-function)
    `(setf (documentation ',add-function 'function) ,add-docu)))
(defun set-bdd-docu (bdd-function bdd-docu)
  (when (symbolp bdd-function)
    `(setf (documentation ',bdd-function 'function) ,bdd-docu)))
(defun set-zdd-docu (zdd-function zdd-docu)
  (when (symbolp zdd-function)
    `(setf (documentation ',zdd-function 'function) ,zdd-docu)))
(defun set-common-docu (function docu)
  (when (symbolp function)
    `(setf (documentation ',function 'function) ,docu)))


(defun find-2list (item 2list)
  (cond
    ((endp 2list) nil)
    ((eq (car 2list) item) (cadr 2list))
    (t (find-2list item (cddr 2list)))))

(defmacro def-cudd-call (generic-name ((&rest functions) &rest arguments)
                         &rest documentation)
  "TODO: (with-cudd-critical-section)?"
  (let* ((add-function    (find-2list :add functions))
         (bdd-function    (find-2list :bdd functions))
         (zdd-function    (find-2list :zdd functions))
         (common-function (find-2list :common functions))
         (generic-docu    (find-2list :generic documentation))
         (add-docu        (or (find-2list :add documentation) generic-docu))
         (bdd-docu        (or (find-2list :bdd documentation) generic-docu))
         (zdd-docu        (or (find-2list :zdd documentation) generic-docu))
         (dont-wrap       (find-2list :dont-wrap-result documentation)))
    `(progn
       ,(generic-cudd-function generic-name arguments generic-docu)
       ,(when add-function
          (add-function generic-name arguments add-function dont-wrap))
       ,(when bdd-function
          (bdd-function generic-name arguments bdd-function dont-wrap))
       ,(when zdd-function
          (zdd-function generic-name arguments zdd-function dont-wrap))
       ,(when common-function
          (common-function generic-name arguments common-function dont-wrap))
       ,(when (and add-docu add-function)
          (set-add-docu add-function add-docu))
       ,(when (and bdd-docu bdd-function)
          (set-bdd-docu bdd-function bdd-docu))
       ,(when (and zdd-docu zdd-function)
          (set-zdd-docu zdd-function zdd-docu))
       ,(when common-function
          (set-common-docu common-function generic-docu)))))

(defmacro with-seq-as-array (array seq &body body)
  (let ((i (gensym "i"))
        (e (gensym "e")))
    `(with-foreign-object (,array :pointer (length ,seq))
       (let ((,i 0))
         (map nil
              (lambda (,e)
                (setf (mem-aref ,array :pointer ,i) (node-pointer ,e)
                      ,i (+ ,i 1)))
              ,seq
              ))

       #+nil (loop :for ,e :being :each :element :of ,seq
                   :for ,i  :from 0
                   :do (setf (mem-aref ,array :pointer ,i) (node-pointer ,e)))
       ,@body)))
