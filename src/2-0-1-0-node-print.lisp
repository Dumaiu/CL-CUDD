(in-package :cudd)

(declaim (maybe-inline print-node-pointer-to-string)
         (notinline print-node-pointer-to-string))
(defgeneric print-node-pointer-to-string (pointer type &key)
  (:documentation "NOTE: Error if *PRINT-READABLY* is set.")

  (:argument-precedence-order type pointer)

  (:method :before (node type &key &allow-other-keys)
    (when *print-readably* (not-implemented-error 'readable-cudd-node-representation)))

  ;; id=around
  (:method :around (pointer type &rest keys
                    &key (manager *manager*)
                    &allow-other-keys)
    "TODO: What does printing the 'index' do?"
    (declare (type node-type type)
             (manager manager))
    (with-output-to-string (stream)
      (print-unreadable-object (pointer stream :type nil :identity t)
        (format stream "~A" type)

        (princ (apply #'call-next-method pointer type :nested t
                      keys)
               stream)

        (format stream " INDEX ~A" (cudd-node-read-index pointer))
        (cond
          ((cudd-node-is-constant pointer)
           (format stream ", LEAF (VALUE ~A)" (cudd-node-value pointer)))
          ('otherwise
           (format stream ", INNER 0x~x" (pointer-address pointer))))
        (format stream ", REF ~d" (cudd-node-ref-count pointer))
        (format stream ", MANAGER #~D" (manager-index manager))
        (format stream ";"))))

  ;; id=variable
  (:method (pointer (type (eql 'bdd-variable-node))
            &key nested ; security
              ((:index variable-id)) ; TODO
            &allow-other-keys)
    (declare (node-pointer pointer)
             (boolean nested)
             (type integer variable-id))
    (assert* nested)
    (with-output-to-string (stream)
      (format stream ": VAR #~A" variable-id)))

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
      (format stream ": ~A" constant)
      (call-next-method)))

  ;; (:method (pointer (type (eql 'add-constant-node)) &key nested)
  ;;   "Generic constant print.  TODO: Combine with (eql 'zdd-constant-node)."
  ;;   (declare (node-pointer pointer)
  ;;            (boolean nested)
  ;;            ;; (boolean constant)
  ;;            )
  ;;   (assert* nested)
  ;;   (with-output-to-string (stream)
  ;;     (format stream ", LEAF (VALUE ~A)" (cudd-node-value pointer))))

  ;; (:method (pointer (type (eql 'zdd-constant-node)) &key nested)
  ;;   "Generic constant print.  TODO: Combine with (eql 'add-constant-node)."
  ;;   (declare (node-pointer pointer)
  ;;            (boolean nested)
  ;;            ;; (boolean constant)
  ;;            )
  ;;   (assert* nested)
  ;;   (with-output-to-string (stream)
  ;;     (format stream ", LEAF (VALUE ~A)" (cudd-node-value pointer))))

  (:method (pointer type &key nested &allow-other-keys)
    "For general nodes.  Does nothing.
"
    (declare (node-type type))
    (assert* (eq t nested))
    ""
    ;; (with-output-to-string (stream)
    ;;   (format stream ", INDEX ~A" (cudd-node-read-index pointer))
    ;;   (cond
    ;;     ((cudd-node-is-constant pointer)
    ;;      (format stream ", LEAF (VALUE ~A)" (cudd-node-value pointer)))
    ;;     ('otherwise
    ;;      (format stream ", INNER 0x~x" (pointer-address pointer)))))

    ;; (with-output-to-string (stream)
    ;;   (print-unreadable-object (pointer stream :type type :identity nil)
    ;;     (format stream "INDEX ~A " (cudd-node-read-index pointer))
    ;;     (if (cudd-node-is-constant pointer)
    ;;         (format stream "LEAF (VALUE ~A)" (cudd-node-value pointer))
    ;;         (format stream "INNER 0x~x" (pointer-address pointer)))
    ;;     (format stream " REF ~d"
    ;;             (cudd-node-ref-count pointer))
    ;;     (format stream ", MANAGER #~D"
    )
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
                                    :manager (node-manager node))))

  (:method ((node bdd-variable-node) &key)
    (let ((pointer (node-pointer node))
          (id (variable-id node)))
      (print-node-pointer-to-string pointer 'bdd-variable-node
                                    :index id
                                    :manager (node-manager node))))

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
           keys)))
(declaim (notinline print-node-to-string))

(defmethod print-object ((object node) stream)
  (format stream (print-node-to-string object)))
