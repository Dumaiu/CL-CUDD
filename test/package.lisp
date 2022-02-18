(in-package :asdf-user)

(define-package cl-cudd.test
    (:mix
     :cl-cudd.internal-utils
     :cl-cudd :cl-cudd.baseapi
     :fiveam :iterate :trivia :arrow-macros
     :asdf :uiop
     :cl)
  (:shadowing-import-from :series ;:cl-cudd.baseapi
                          #:collect)
  (:shadow :next :<>))

(in-package cl-cudd.test)

(def-suite :cl-cudd)
(in-suite :cl-cudd)

;; (defstruct lazy-vector
;;   (contents  :type (vector * (or boolean function))))

(defclass lazy-vector ()
  ((contents :initform #()
             :type (vector ;; (or boolean function)
                    function)
             :initarg :contents))
  (:metaclass structure-class))

(shadowing-import '(generic-cl.lazy-seq::thunk))

(defmethod initialize-instance ((vec lazy-vector) &key source n)
  (declare (sequence source)
           (type (integer 0) n))
  (iter
    (for i from 0 below n)
    (collecting (thunk-fn )(thunk `(with-slots (contents) ,source
                    (the boolean (not (zerop (aref contents ,i)) ))))
      into contents result-type vector)

    (finally
     (return (call-next-method vec :contents contents))))
  )

(defun models (kind)
  "Helper for ':cl-cudd.test': look up a list of *.test files from one of the test/ subdirectories."
  (directory (merge-pathnames "*.tests" (asdf:system-relative-pathname :cl-cudd (format nil "test/~a/" kind)))))

;; we need a better api for creating a bdd
;; -- No kidding. --JDJ-S

(defun basename (pathname)
  (make-pathname :type nil :defaults pathname))
(defun append-name (pathname suffix)
  (make-pathname :name (concatenate 'string (pathname-name pathname) suffix) :defaults pathname))

(defun parse-bdd/parse-only (path ;; &optional zdd-binate?
                             )
  (let ((f (reduce #'node-or
                   (iter (for line in-file path using #'read-line)
                     (collecting
                         (reduce #'node-and
                                 (iter (for c in-vector line)
                                   (for index from 0)
                                   (collecting
                                       (ecase c
                                         (#\0 (node-complement
                                               (make-var 'bdd-node :index index)))
                                         (#\1 (make-var 'bdd-node :index index)))))
                                 :initial-value (one-node 'bdd-node))))
                   :initial-value (zero-node 'bdd-node))))
    (declare (bdd-node f))
    f))


(defun parse-bdd (path &optional zdd-binate?)
  (fresh-line)
  (with-manager ()
    (let ((f
            (reduce #'node-or
                    (iter (for line in-file path using #'read-line)
                      (collecting
                          (reduce #'node-and
                                  (iter (for c in-vector line)
                                    (for index from 0)
                                    (collecting
                                        (ecase c
                                          (#\0 (node-complement
                                                (make-var 'bdd-node :index index)))
                                          (#\1 (make-var 'bdd-node :index index)))))
                                  :initial-value (one-node 'bdd-node))))
                    :initial-value (zero-node 'bdd-node))))
      (pass "constructed DD")
      (finishes (print f))
      (finishes (print (dag-size f)))
      (finishes (print (multiple-value-list (reordering-status))))
      (finishes
        (plot (append-name path "-BDD") f))
      ;; since BDDs may contain complemented edges, it is slightly hard to understand.
      ;; Usually converting it into ADDs will improve the output
      (finishes
        (plot (append-name path "-BDD-as-ADD") (bdd->add f)))
      (finishes
        (if zdd-binate?
            (plot (append-name path "-BDD-as-ZDD-cover") (bdd->zdd-cover f))
            (plot (append-name path "-BDD-as-ZDD-simple") (bdd->zdd-simple f)))))))

(test bdd
  (dolist (m (append (models "gates") (models "modest")))
    (parse-bdd m)
    (parse-bdd m t)))

(defun parse-add (path)
  (fresh-line)
  (with-manager ()
    (let ((f
            (reduce #'node-or
                    (iter (for line in-file path using #'read-line)
                      (collecting
                          (reduce #'node-and
                                  (iter (for c in-vector line)
                                    (for index from 0)
                                    (collecting
                                        (ecase c
                                          (#\0 (node-complement
                                                (make-var 'add-node :index index)))
                                          (#\1 (make-var 'add-node :index index)))))
                                  :initial-value (one-node 'add-node))))
                    :initial-value (zero-node 'add-node))))
      (pass "constructed DD")
      (finishes
        (print f))
      (finishes
        (print (dag-size f)))
      (finishes (print (multiple-value-list (reordering-status))))
      (finishes
        (plot (append-name path "-ADD") f)))))

(test add
  (with-manager ()
    (finishes (print (zero-node 'add-node)))
    (finishes (print (add-constant 0.0d0)))
    (finishes (print (node-and (make-var 'add-node :index 1)
                               (one-node 'add-node))))
    (finishes (print (node-or (make-var 'add-node :index 1)
                              (one-node 'add-node))))
    (finishes (print (node-and (make-var 'add-node :index 1)
                               (zero-node 'add-node))))
    (finishes (print (node-or (make-var 'add-node :index 1)
                              (zero-node 'add-node))))
    (finishes (print (info)))
    (finishes (print (node-count)))
    (finishes (print (peak-node-count)))
    (finishes (print (peak-live-node-count)))
    ;; This number always includes the two constants 1 and 0.
    (is (= 2 (zdd-node-count))))
  (dolist (m (append (models "gates") (models "modest")))
    (format t "~%testing model ~a" m)
    (parse-add m)))


(defun parse-zdd-sets-of-subsets (path)
  (fresh-line)
  (let* ((all "abc"))
    (with-manager ()
      (let* ((f
               (reduce #'zdd-union
                       (iter (for line in-file path using #'read-line)
                         (collecting
                             (iter (for c in-vector line)
                               (with f = (zdd-set-of-emptyset)) ; {{}} --- does not contain anything
                               ;; (break "~@{~a~}" c all (position c all))
                               (setf f (zdd-change f (position c all))) ; add c to {{}} --> {{c}}
                               (finally (return f)))))
                       :initial-value (zdd-emptyset))))
        (pass "constructed DD")
        (finishes
          (print f))
        (finishes
          (print (dag-size f)))
        (finishes (print (multiple-value-list (zdd-reordering-status))))
        (finishes
          (plot (append-name path "-ZDD") f))))))

(test zdd
  (dolist (m (models "sets-of-subsets"))
    (format t "~%testing model ~a" m)
    (parse-zdd-sets-of-subsets m)))


(test reordering
  ;; swap

  ;; permutation

  ;; dynamic reordering
  )


(test bdd-transfer

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
          (declare (manager old-manager))

          (with-manager ()
            (let ((new-manager *manager*))
              (declare (manager old-manager))
              (assert (not (eql old-manager new-manager)))

              ;; (print *manager*)
              ;; (print *cudd-mutex*)

              ;; (info)
              ;; (break "Before transfer")

              (let ((old-bdd (parse-bdd/parse-only 1st)))
                (declare (bdd-node old-bdd))

                (let ((new-bdd (bdd-transfer old-bdd
                                             :src new-manager
                                             :dest old-manager)))
                  (declare (bdd-node new-bdd))

                  ;; (info)
                  ;; (break "After transfer")
                  )))))

        ;; (info)
        ;; (break "Back in original")
        )))
  )


(test support-series

  (let* ((gates (models "gates"))
         (1st (first gates)))
    (declare (pathname 1st))

    (with-manager ()
      (let ((test-bdd (parse-bdd/parse-only 1st)))
        (declare (bdd-node test-bdd))

        (assert;; is
         (= 2 (support-size test-bdd)))

        (assert (= 2 (length (support-index test-bdd))))
        ;; (break "~A" (support-index test-bdd))


        ;; (break "~A"(cudd-support-index %mp%
        ;;                      (node-pointer test-bdd)))

        ;; (break "~A" (support-index test-bdd))

        ;; TODO: make (collect) point to iter:collect
        ;; (assert (= 2 (length (collect (support-index-int-series test-bdd)))))
        ;; (break "~A" (support-index-int-series test-bdd))

        ;; (assert (= 1 (collect-nth 0 (support-index-int-series test-bdd))))
        ;; (assert (= 1 (collect-nth 1 (support-index-int-series test-bdd))))
        (assert (null (collect-nth 2 (support-index-int-series test-bdd))))
        ;; TODO: Benchmark this


        ;; TODO: Silence 'freeing a cudd manager' msg.
        ))))


(let-1 vec #(0 1 2 3 4 5 6)
  (let-1 series (series:scan '(vector * integer) vec)
    (declare (type (series:series integer) series))
    (let-1 map-series (map-fn 'integer (lambda (i)
                                         (print i)
                                         (the integer i))
                              series)
      (declare (type (series:series integer) map-series))
      ;; (series:mapping )
      (collect-nth 2 map-series))
    ))


(let-1 vec #(0 1 2 3 4 5 6)
  (print "Start.")
  (let-1 series (series:scan `(vector integer ,(length vec)) vec)
    (declare (type (series:series integer) series))

    (let-1 map-series (map-fn 'integer (lambda (i)
                                         (print i)
                                         (the integer i))
                              series)
      (declare (type (series:series integer) map-series))
      ;; (series:mapping )
      (collect 'bag (choose (mask (series 2)) map-series)))
    ))


(let-1 vec #(0 1 2 3 4 5 6)
  (print "Starting.")
  (collect 'bag (choose (mask (series 2))
                        (map-fn 'integer (lambda (i)
                                           (print i)
                                           (the integer i))
                                (series:scan `(vector ,(length vec) integer) vec)))))


(let-1 vec #(0 1 2 3 4 5 6)
  (print "Starting.")
  (collect 'bag (map-fn 'integer (lambda (i)
                                   (print i)
                                   (the integer i))
                        (series:scan `(vector ,(length vec) integer) vec))))
