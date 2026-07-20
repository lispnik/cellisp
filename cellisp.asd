(defsystem "cellisp"
  :description "A spreadsheet backend whose formula language is Common Lisp."
  :version "0.1.0"
  :license "MIT"
  :depends-on ("bordeaux-threads")
  :serial t
  :components ((:file "package")
               (:file "cell")
               (:file "sheet")
               (:file "workbook")
               (:file "eval")
               (:file "stdlib")
               (:file "api")
               (:file "taxonomy")
               (:file "serialize")
               (:file "edit")
               (:file "explain"))
  :in-order-to ((test-op (test-op "cellisp/test"))))

(defsystem "cellisp/test"
  :depends-on ("cellisp")
  :serial t
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :cellisp/test :run-tests)))

;;; Optional rendering layer: turns cell values/errors into display strings and
;;; spreadsheet error tokens. Separate so the core engine carries no UI concern.
(defsystem "cellisp/display"
  :description "A display/formatting layer over the Cellisp engine."
  :depends-on ("cellisp")
  :serial t
  :components ((:file "display"))
  :in-order-to ((test-op (test-op "cellisp/display-test"))))

(defsystem "cellisp/display-test"
  :depends-on ("cellisp/display")
  :serial t
  :components ((:file "display-test"))
  :perform (test-op (o c) (symbol-call :cellisp/display-test :run-tests)))
