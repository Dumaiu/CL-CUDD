(in-package cl-cudd.test)

(define-package cl-cudd.test.parachute
    (:mix
     :cl-cudd
     :parachute
     cl-cudd.test.common
     :cl)
  (:import-from cl-cudd.test
                models
                parse-bdd/parse-only
                )
  (:import-from sb-ext
                #:search-roots
                #:make-weak-pointer
                ))

(in-package cl-cudd.test.parachute)

(log:config cudd-logger :trace)
(log:config cudd-node-logger :trace)

(define-test+run-interactively bdd-transfer
  :fix (*default-reordering-method*)

  (setq *default-reordering-method* :cudd-reorder-same)

  (let* ((gates (models "gates"))
         (1st (first gates)))
    (declare (pathname 1st))

    (with-manager ()
      (progn ;; let ((old-bdd (parse-bdd/parse-only 1st)))
        ;;  (declare (bdd-node old-bdd))

        ;; (info)
        ;; (print *manager*)
        ;; (print *cudd-mutex*)
        ;; (break "~S" old-bdd)

        (let ((old-manager *manager*))

          (of-type manager old-manager)

          (with-manager ()
            (search-roots *ptr* :gc t)
            (let ((new-manager *manager*))
              (of-type manager new-manager)
              (isnt eql old-manager new-manager)

              ;; (print *manager*)
              ;; (print *cudd-mutex*)

              ;; (info)
              ;; (break "Before transfer")

              (let ((old-bdd (parse-bdd/parse-only 1st)))
                (declare (bdd-node old-bdd))

                (let-1 new-bdd (bdd-transfer old-bdd
                                             :src new-manager
                                             :dest old-manager)
                  (of-type bdd-node new-bdd)

                  (print (dag-size new-bdd))
                  ;; (info)
                  ;; (break "After transfer")
                  )))))

        ;; (info)
        ;; (break "Back in original")
        )))

  (gc :full t :manager :all))


(defun search-r (manager)
  (check-type manager manager)
  (let-1 ptr (make-weak-pointer manager)
    (search-roots ptr
                  :criterion :oldest
                  :gc t)))

(defparameter *ptr* (make-weak-pointer (gethash 15 *managers*)))
(defparameter *ptr-2* (make-weak-pointer (gethash 32 *managers*)))
(search-roots *ptr* :gc t)
(search-roots *ptr-2* :gc t)

