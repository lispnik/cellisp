(defsystem "cellisp"
  :description "A spreadsheet backend whose formula language is Common Lisp."
  :version "0.1.0"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "cell")
               (:file "sheet")
               (:file "eval")
               (:file "api"))
  :in-order-to ((test-op (test-op "cellisp/test"))))

(defsystem "cellisp/test"
  :depends-on ("cellisp")
  :serial t
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :cellisp/test :run-tests)))
