(in-package :cudd)

(declaim (maybe-inline print-node-pointer-to-string)
         (notinline print-node-pointer-to-string))
(defgeneric print-node-pointer-to-string (pointer type &key)
  (:documentation "NOTE: Error if *PRINT-READABLY* is set.")

  (:argument-precedence-order type pointer)

  (:method :before (node type &key &allow-other-keys)
    (when *print-readably* (not-implemented-error 'readable-cudd-node-representation)))

  (:method :around (pointer type &rest keys
                    &key (manager *manager*)
                    &allow-other-keys)
    "TODO: What does printing the 'index' do?"
    (declare (type node-type type)
             (manager manager))
    (with-output-to-string (stream)
      (print-unreadable-object (pointer stream :type nil :identity t)
        ;; (format stream "INDEX ~A " (cudd-node-read-index pointer))
        (format stream "~A" type)

        (princ (apply #'call-next-method pointer type :nested t
                      keys)
               stream)

        (format stream ", REF ~d" (cudd-node-ref-count pointer))
        (format stream ", MANAGER #~D" (manager-index manager))
        (format stream ";"))))

  ;; id=variable
  (:method (pointer (type (eql 'bdd-variable-node))
            &key nested ; security
              index
            &allow-other-keys)
    (declare (node-pointer pointer)
             (boolean nested)
             (type integer index))
    (assert* nested)
    (with-output-to-string (stream)
      (format stream ": VAR #~A" index)))

  ;; id=constant
  (:method (pointer (type (eql 'bdd-constant-node))
            &key nested
              (constant nil constant?)
            &allow-other-keys)
    (declare (node-pointer pointer)
             (boolean nested)
             (boolean constant))
    (assert* nested)
    (assert* constant?)
    ;; (assert* constant)
    (assert* (cudd-node-is-constant pointer))
    (with-output-to-string (stream)
      (format stream ": ~A" constant)))

  (:method (pointer type &key nested &allow-other-keys)
    "For leaf nodes.
  * TODO: What does the 'index' mean in this case?
"
    (declare (node-type type))
    (assert* (eq t nested))
    (with-output-to-string (stream)
      (format stream ", INDEX ~A" (cudd-node-read-index pointer))
     (format stream ", LEAF (VALUE ~A)" (cudd-node-value pointer))
     (format stream ", INNER 0x~x" (pointer-address pointer))))
  ); (print-node-pointer-to-string)


(declaim (maybe-inline print-node-to-string))
(defgeneric print-node-to-string (node &key)
  (:method ((node node) &key &allow-other-keys)
    "Recurse."
    (print-node-to-string (the node-pointer (node-pointer node))
                          :type (type-of node)
                          :manager (node-manager node)))

  (:method ((node bdd-constant-node) &key)
    (let ((pointer (node-pointer node))
          (constant (constant node)))
      (assert* (cudd-node-is-constant pointer))
      (print-node-pointer-to-string pointer 'bdd-constant-node
                                      :constant constant
                                      :manager (node-manager node))
      ;; (with-output-to-string (stream)
      ;;   (print-unreadable-object (node stream :type 'bdd-constant-node :identity nil)
      ;;     (format stream "~A" )
      ;;     ;; (format stream "INDEX ~A " (cudd-node-read-index pointer))
      ;;     (format stream ", LEAF (VALUE ~A)" (cudd-node-value pointer))
      ;;     (format stream ", REF ~d"
      ;;             (cudd-node-ref-count pointer))
      ;;     (format stream ", MANAGER #~D"
      ;;             (manager-index (node-manager node)))))
      ))

  (:method ((node bdd-variable-node) &key)
    (let ((pointer (node-pointer node))
          (index (index node)))
      (print-node-pointer-to-string pointer 'bdd-variable-node
                                      :index index
                                      :manager (node-manager node))))

  ;; (:method ((node bdd-variable-node) &key)
  ;;   (let-1 pointer (node-pointer node)
  ;;     (declare (node-pointer pointer))
  ;;     (print-unreadable-object (node nil :type 'bdd-variable-node
  ;;                                        :identity t))
  ;;     ))

  (:method ((pointer #.(typexpand 'node-pointer))
            &rest keys
            &key
              (type 'node)
              (manager *manager*)
            &allow-other-keys)
    "General case."
    (declare (node-pointer pointer)
             (manager manager))
    (apply #'print-node-pointer-to-string pointer type
           :manager manager
           keys)
    ;; (with-output-to-string (stream)
    ;;   (print-unreadable-object (pointer stream :type type :identity nil)
    ;;     (format stream "INDEX ~A " (cudd-node-read-index pointer))
    ;;     (if (cudd-node-is-constant pointer)
    ;;         (format stream "LEAF (VALUE ~A)" (cudd-node-value pointer))
    ;;         (format stream "INNER 0x~x" (pointer-address pointer)))
    ;;     (format stream " REF ~d"
    ;;             (cudd-node-ref-count pointer))
    ;;     (format stream ", MANAGER #~D"

    ))
(declaim (notinline print-node-to-string))

(defmethod print-object ((object node) stream)
  (format stream (print-node-to-string object)))
