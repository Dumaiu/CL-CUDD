(in-package :asdf-user)

(defsystem :cl-cudd.signal
  :depends-on (:cl-cudd
               :bordeaux-threads
               :trivial-signal)
  :pathname "src/"
  :serial t
  :components
  ((:file "3-0-sigabrt")))
