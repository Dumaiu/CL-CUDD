(in-package :asdf-user)
(defsystem cl-cudd
  :serial t
  :author "Christian von Essen <christian@mvonessen.de>"
  :maintainer "Masataro Asai <guicho2.7128@gmail.com>"
  :license "BSD Style (see LICENSE)"
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:cffi
               :alexandria
               :trivial-garbage
               :let-plus
               :bordeaux-threads
               ;; TODO :cl-cudd.build
               :trivia.cffi
               :log4cl
               :iterate
               :series)
  :serial t
  :components ((:file "src/0-package")
               (:file "src/1-0-0-library")
               (:file "src/1-0-1-conditions")
               (:file "src/1-0-2-translators")
               (:file "src/1-1-0-swig-macros")
               (:cffi-grovel-file "src/1-1-1-grovel")
               (:file "src/1-1-2-fun")
               (:file "src/1-2-0-base-common")
               (:file "src/1-2-1-base-add")
               (:file "src/1-2-1-base-bdd")
               (:file "src/1-2-1-base-zdd")
               (:file "src/1-3-dddmp")
               (:file "src/2-config"
                :description "Added [2021-12-08 Wed] -- JJ-S")
               (:file "src/2-0-0-manager")
               (:file "src/2-0-1-node")
               (:file "src/2-0-2-def-cudd-call")
               (:file "src/2-1-add")
               (:file "src/2-1-generic-complex")
               (:file "src/2-1-generic-simple")
               (:file "src/2-1-generic-swap")
               (:file "src/2-1-system")
               (:file "src/2-1-zdd-set-operations")
               (:file "src/2-2-add-bdd-bridge")
               (:file "src/2-2-zdd-bdd-bridge")
               (:file "src/2-3-reordering")
               (:file "src/2-4-hook"
                :description "Side-effect: initializes `*manager*' if null.")
               (:file "src/2-5-utils")
               (:file "src/2-6-dot"))
  :description "A two-layered binding to the CUDD binary decision diagram library.

See README.md for more details."
  :in-order-to ((test-op (test-op cl-cudd.test))))


(defsystem :cl-cudd/signal
  :description "
  * TODO: I wished to name this 'cl-cudd.signal' for consistency, but can't seem get ASDF to find it.
"
  :depends-on (:cl-cudd
               :bordeaux-threads
               :trivial-signal)
  :pathname "src/"
  :serial t
  :components
  ((:file "3-0-sigabrt")))
