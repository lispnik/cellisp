(require :asdf)
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
#+quicklisp (ql:quickload "bordeaux-threads" :silent t)
(asdf:load-system "cellisp")
(asdf:load-system "cellisp/display")

(in-package #:cellisp)
(use-package '#:cellisp/display)

(defun show (label value)
  "Print LABEL => VALUE so each tutorial step demonstrates itself."
  (format t "~&~28A => ~S~%" label value))

(format t "~&==== Cellisp tutorial ====~%")

(defparameter *s* (make-sheet))

(set-cells *s*
  '(("A1" 10)
    ("A2" 20)
    ("A3" 30)
    ("A4" (sum (cells "A1" "A3")))))

(show "A4 = sum(A1:A3)" (get-value *s* "A4"))

(set-cell *s* "A1" 100)
(show "A4 after A1 := 100" (get-value *s* "A4"))   ; now 150

(set-cells *s* '(("B1" (/ 1 0))))
(multiple-value-bind (val err) (get-value *s* "B1")
  (format t "~&B1: value=~S  error=~A~%" val (type-of err)))

(set-cell *s* "B1" '(/ 10 2))
(show "B1 repaired" (get-value *s* "B1"))           ; 5

(defparameter *env-sheet*
  (make-sheet :environment '((tax-rate . 0.08))))

(set-cell *env-sheet* "A1" 250)                  ; a subtotal
(set-name *env-sheet* "subtotal" "A1")           ; name the cell
(set-cell *env-sheet* "A2" '(* (cell "subtotal") tax-rate))

(show "tax on subtotal" (get-value *env-sheet* "A2"))   ; 20.0

(set-cells *env-sheet* '(("D1" 3) ("D2" 4) ("D3" 5)))
(set-range *env-sheet* "scores" "D1" "D3")
(set-cell *env-sheet* "D4" '(sum (cells "scores")))
(show "sum(scores)" (get-value *env-sheet* "D4"))       ; 12

(show "minimum" (minimum 3 1 2))                       ; 1
(show "maximum" (maximum 3 1 2))                       ; 3
(show "sortv"   (sortv '(3 1 2)))                      ; (1 2 3)
(show "iferror" (iferror (elt #() 5) :fallback))       ; :FALLBACK

(show "to-number strict"   (to-number "42"))                     ; 42
(show "to-number euro"     (to-number "1,5" nil :decimal #\,))   ; 1.5
(show "to-number grouped"  (to-number "1,234.56" nil :group #\,)); 1234.56
(show "to-number bad"      (to-number "oops" :na))               ; :NA

(show "vlookup"
      (vlookup "banana"
               '(("apple" 3) ("banana" 5) ("cherry" 8))
               2))                                     ; 5

(defparameter *prices* (make-sheet))
(set-cells *prices*
  '(("A1" "apple")  ("B1" 3)
    ("A2" "banana") ("B2" 5)
    ("A3" "cherry") ("B3" 8)
    ("D1" (vlookup "cherry" (grid "A1" "B3") 2))))
(show "vlookup over grid" (get-value *prices* "D1"))   ; 8

(defparameter *log* '())
(observe *s* "A4" (lambda (new-value) (push new-value *log*)))

(set-cell *s* "A1" 7)     ; A4 = 7 + 20 + 30 = 57, so the observer fires
(show "observer log" *log*)                            ; (57)

(set-cached *s* "A4" t)
(show "A4 (cached)" (get-value *s* "A4"))              ; 57

(defparameter *edit* (make-sheet))
(set-cell *edit* "A1" 1)
(set-cell *edit* "A1" 2)
(undo *edit*)
(show "A1 after undo" (get-value *edit* "A1"))         ; 1
(redo *edit*)
(show "A1 after redo" (get-value *edit* "A1"))         ; 2

(with-transaction (*edit*)
  (set-cell *edit* "B1" 10)
  (set-cell *edit* "B2" 20)
  (set-cell *edit* "B3" '(sum (cells "B1" "B2"))))
(show "B3 (transaction)" (get-value *edit* "B3"))      ; 30

(insert-row *edit* 1)                     ; everything moves down one row
(show "A1 shifted to A2" (get-value *edit* "A2"))      ; 2
(copy-cell *edit* "A2" "C5")
(show "C5 (copied)" (get-value *edit* "C5"))           ; 2

(show "spill extent" (spill *edit* "E1" '(list 100 200 300)))  ; (3 . 1)
(show "E2 from spill"  (get-value *edit* "E2"))                ; 200

(defparameter *wb* (make-workbook))
(defparameter *sales*   (add-sheet *wb* "Sales"))
(defparameter *summary* (add-sheet *wb* "Summary"))

(set-cells *sales* '(("A1" 100) ("A2" 200)))
(set-cell  *summary* "A1" '(+ (cell "Sales!A1") (cell "Sales!A2")))

(show "cross-sheet total" (get-value *summary* "A1"))  ; 300

(set-cell *sales* "A1" 150)
(show "total after edit" (get-value *summary* "A1"))   ; 350

(defparameter *report* (make-sheet))
(set-cells *report*
  '(("A1" "Widget") ("B1" 1200.5) ("C1" 0.23)
    ("A2" "Gadget") ("B2" 890)    ("C2" -0.05)))

(defparameter *fmt* (make-formats))
(set-column-format *fmt* "B" '(:currency "$" 2))       ; B as money
(set-column-format *fmt* "C" '(:percent 1))            ; C as a percentage
(add-conditional *fmt* (lambda (v) (and (realp v) (minusp v)))
                 "LOSS" :column "C")                    ; negatives -> "LOSS"

(show "B1 formatted" (display-value *report* "B1" :formats *fmt*))  ; "$1200.50"
(show "C2 formatted" (display-value *report* "C2" :formats *fmt*))  ; "LOSS"

(ignore-errors (set-cell *report* "D1" '(/ 1 0)))
(multiple-value-bind (v e) (get-value *report* "D1")
  (declare (ignore v))
  (show "D1 as token" (error-token e)))                ; "#DIV/0!"

(format t "~&--- values ---~%")
(print-sheet *report* :formats *fmt* :name "Report")

(format t "~&--- formulas ---~%")
(print-sheet *report* :formulas t :name "Report")

(defparameter *sheet-path*
  (merge-pathnames "cellisp-tutorial.sheet" (uiop:temporary-directory)))

(defparameter *persist* (make-sheet))
(set-cells *persist*
  '(("A1" 21) ("A2" 2) ("A3" (* (cell "A1") (cell "A2")))))
(save-sheet *persist* *sheet-path*)

(let ((reloaded (load-sheet *sheet-path*)))
  (show "A3 recomputed on load" (get-value reloaded "A3")))  ; 42

(defparameter *wb-path*
  (merge-pathnames "cellisp-tutorial-wb.sheet" (uiop:temporary-directory)))
(save-workbook *wb* *wb-path*)

(let ((wb2 (load-workbook *wb-path*)))
  (show "reloaded Summary!A1"
        (get-value (find-sheet wb2 "Summary") "A1")))         ; 350

(defparameter *invoice* (make-sheet :environment '((tax-rate . 0.0875))))
(set-cells *invoice*
  '(("A1" "Item")     ("B1" "Qty") ("C1" "Price") ("D1" "Line")
    ("A2" "Notebook") ("B2" 3)  ("C2" 4.5)  ("D2" (* (cell "B2") (cell "C2")))
    ("A3" "Pen")      ("B3" 10) ("C3" 1.25) ("D3" (* (cell "B3") (cell "C3")))
    ("A4" "Marker")   ("B4" 4)  ("C4" 2.0)  ("D4" (* (cell "B4") (cell "C4")))
    ("A6" "Subtotal") ("D6" (sum (cells "D2" "D4")))
    ("A7" "Tax")      ("D7" (* (cell "D6") tax-rate))
    ("A8" "Total")    ("D8" (+ (cell "D6") (cell "D7")))))
(set-range *invoice* "line-items" "D2" "D4")

(defparameter *invoice-fmt* (make-formats))
(set-column-format *invoice-fmt* "C" '(:currency "$" 2))
(set-column-format *invoice-fmt* "D" '(:currency "$" 2))

(format t "~&--- invoice ---~%")
(print-sheet *invoice* :formats *invoice-fmt* :name "Invoice")
(show "invoice total" (get-value *invoice* "D8"))      ; 36.975

(format t "~&==== tutorial complete ====~%")
