(in-package :cl-cudd)

(define-package :cl-cudd.signal
    (:documentation
     "Handler definition copied from https://quickref.common-lisp.net/trivial-signal.html."
     )
    (:mix
     :cl-cudd
     :trivial-signal
     :asdf :uiop
     :cl)
  (:export
   #:exit-on-signal
   #:*error-output*))

(in-package :cl-cudd.signal)

(defun exit-on-signal (signo)
  (error "~&received ~A~%" (signal-name signo))
  #+sbcl (sb-ext:exit :code 1 :abort t))

;; Side-effects:
(setf (signal-handler :abort) #'exit-on-signal)
(setf (signal-handler :term) #'exit-on-signal)

