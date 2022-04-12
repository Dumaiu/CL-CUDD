;;;; Autogenerated ASD file for system "CL-CUDD"
;;;; In order to regenerate it, run update-asdf from shell (see https://github.com/phoe-krk/asd-generator)
;;;; For those who do not have update-asdf, run `ros install asd-generator` (if you have roswell installed)
;;;; There are also an interface available from lisp: (asd-generator:regen &key im-sure)
(defsystem cl-cudd.test
  :serial t
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "BSD Style (see LICENSE)"
  :depends-on (:cl-cudd :fiveam :iterate :trivia :arrow-macros)
  :serial t
  :pathname "test/"
  :components ((:file "package")
               (:file "main"))
  :description "A two-layered binding to the CUDD binary decision diagram library.

See README.md for more details."
  :perform (test-op :after (op c)
                    (eval (read-from-string "(5am:run! :cl-cudd)"))))
