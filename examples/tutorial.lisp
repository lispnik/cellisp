;;;; -*- mode: lisp -*- GENERATED FILE — DO NOT EDIT.
;;;;
;;;; examples/tutorial.lisp is tangled from ../TUTORIAL.org. Edit the prose and
;;;; code in TUTORIAL.org, then re-tangle to regenerate this file:
;;;;
;;;;   emacs --batch --eval '(require (quote ob-tangle))' \
;;;;         --eval '(org-babel-tangle-file "TUTORIAL.org")'
;;;;
;;;; Run it with:  sbcl --script examples/tutorial.lisp

(require :asdf)
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
#+quicklisp (ql:quickload "bordeaux-threads" :silent t)
(asdf:load-system "cellisp")
(asdf:load-system "cellisp/display")

(in-package #:cellisp)
(use-package '#:cellisp/display)

(defparameter *checks* 0)
(defparameter *failures* 0)

(defun show (label value)
  "Print LABEL => VALUE (display only; note it captures just the primary value)."
  (format t "~&~28A => ~S~%" label value))

(defun check (label got expected)
  "Print LABEL => GOT and verify GOT is EQUAL to EXPECTED, tallying failures."
  (incf *checks*)
  (format t "~&~28A => ~S~%" label got)
  (unless (equal got expected)
    (incf *failures*)
    (format *error-output* "~&    !! FAIL ~A: expected ~S~%" label expected)))

(format t "~&==== Cellisp tutorial ====~%")

(defparameter *s* (make-sheet))

(set-cells *s*
  '(("A1" 10)
    ("A2" 20)
    ("A3" 30)
    ("A4" (sum (cells "A1" "A3")))))

(check "A4 = sum(A1:A3)" (get-value *s* "A4") 60)

(defun celsius->fahrenheit (c) (+ 32 (* c 9/5)))

(set-cells *s*
  '(("C1" 100)
    ("C2" (celsius->fahrenheit (cell "C1")))       ; call an ordinary defun
    ("C3" (mapcar #'evenp (cells "A1" "A3")))))     ; a formula returning a list
(check "100C in F"        (get-value *s* "C2") 212)
(check "evens in A1:A3"   (get-value *s* "C3") '(t t t))

(set-cell *s* "A1" 100)
(check "A4 after A1 := 100" (get-value *s* "A4") 150)

(set-cells *s* '(("B1" (/ 1 0))))
(multiple-value-bind (val err) (get-value *s* "B1")
  (format t "~&B1: value=~S  error=~A~%" val (type-of err)))

(set-cell *s* "B1" '(/ 10 2))
(check "B1 repaired" (get-value *s* "B1") 5)

(set-cells *s* '(("E1" 5) ("E2" (/ 1 0)) ("E3" 7)))   ; E2 is broken
(set-cell  *s* "E4" '(sum (safe-cells "E1" "E3")))     ; tolerates the hole
(check "sum(safe E1:E3)" (get-value *s* "E4") 12)      ; 5 + 7, E2 skipped

(set-cell *s* "A5" '(* (cell "A4") 2))
(check "A5 = A4 * 2" (get-value *s* "A5") 300)

(format t "~&--- explain A5 ---~%")
(explain *s* "A5")

(defparameter *env-sheet*
  (make-sheet :environment '((tax-rate . 8/100))))     ; an exact rational

(set-cell *env-sheet* "A1" 250)                  ; a subtotal
(set-name *env-sheet* "subtotal" "A1")           ; name the cell
(set-cell *env-sheet* "A2" '(* (cell "subtotal") tax-rate))

(check "tax on subtotal" (get-value *env-sheet* "A2") 20)   ; 250 * 8/100

(set-cells *env-sheet* '(("D1" 3) ("D2" 4) ("D3" 5)))
(set-range *env-sheet* "scores" "D1" "D3")
(set-cell *env-sheet* "D4" '(sum (cells "scores")))
(check "sum(scores)" (get-value *env-sheet* "D4") 12)

(check "minimum" (minimum 3 1 2) 1)
(check "maximum" (maximum 3 1 2) 3)
(check "sortv"   (sortv '(3 1 2)) '(1 2 3))
(check "iferror" (iferror (elt #() 5) :fallback) :fallback)

(defparameter *agg* (make-sheet))
(set-cells *agg*
  '(("A1" 3) ("A2" 4) ("A3" 5)
    ("B1" (average (cells "A1" "A3")))
    ("B2" (cnt (cells "A1" "A3")))
    ("B3" (countif #'evenp (cells "A1" "A3")))
    ("B4" (sumif #'oddp (cells "A1" "A3")))))
(check "average(A1:A3)" (get-value *agg* "B1") 4)
(check "cnt(A1:A3)"     (get-value *agg* "B2") 3)
(check "countif evenp"  (get-value *agg* "B3") 1)   ; just 4
(check "sumif oddp"     (get-value *agg* "B4") 8)   ; 3 + 5

(check "to-number strict"   (to-number "42") 42)
(check "to-number euro"     (to-number "1,5" nil :decimal #\,) 1.5)
(check "to-number grouped"  (to-number "1,234.56" nil :group #\,) 1234.56)
(check "to-number bad"      (to-number "oops" :na) :na)

(check "vlookup"
       (vlookup "banana" '(("apple" 3) ("banana" 5) ("cherry" 8)) 2)
       5)

(defparameter *prices* (make-sheet))
(set-cells *prices*
  '(("A1" "apple")  ("B1" 3)
    ("A2" "banana") ("B2" 5)
    ("A3" "cherry") ("B3" 8)
    ("D1" (vlookup "cherry" (grid "A1" "B3") 2))))
(check "vlookup over grid" (get-value *prices* "D1") 8)

(defparameter *log* '())
(observe *s* "A4" (lambda (new-value) (push new-value *log*)))

(set-cell *s* "A1" 7)     ; A4 = 7 + 20 + 30 = 57, so the observer fires
(check "observer log" *log* '(57))

(set-cached *s* "A4" t)
(check "A4 (cached)" (get-value *s* "A4") 57)

(defparameter *val* (make-sheet))
(set-cell *val* "A1" 10)
(set-validator *val* "A1" #'plusp)             ; only positives allowed
(ignore-errors (set-cell *val* "A1" -5))       ; rejected
(multiple-value-bind (v e) (get-value *val* "A1")
  (check "rejected value" v nil)
  (check "rejection token" (error-token e) "#VALUE!"))

(defparameter *edit* (make-sheet))
(set-cell *edit* "A1" 1)
(set-cell *edit* "A1" 2)
(undo *edit*)
(check "A1 after undo" (get-value *edit* "A1") 1)
(redo *edit*)
(check "A1 after redo" (get-value *edit* "A1") 2)

(with-transaction (*edit*)
  (set-cell *edit* "B1" 10)
  (set-cell *edit* "B2" 20)
  (set-cell *edit* "B3" '(sum (cells "B1" "B2"))))
(check "B3 (transaction)" (get-value *edit* "B3") 30)

(insert-row *edit* 1)                     ; everything moves down one row
(check "A1 shifted to A2" (get-value *edit* "A2") 2)
(copy-cell *edit* "A2" "C5")
(check "C5 (copied)" (get-value *edit* "C5") 2)

(check "spill extent" (spill *edit* "E1" '(list 100 200 300)) '(3 . 1))
(check "E2 from spill" (get-value *edit* "E2") 200)

(defparameter *wb* (make-workbook))
(defparameter *sales*   (add-sheet *wb* "Sales"))
(defparameter *summary* (add-sheet *wb* "Summary"))

(set-cells *sales* '(("A1" 100) ("A2" 200)))
(set-cell  *summary* "A1" '(+ (cell "Sales!A1") (cell "Sales!A2")))

(check "cross-sheet total" (get-value *summary* "A1") 300)

(set-cell *sales* "A1" 150)
(check "total after edit" (get-value *summary* "A1") 350)

(defparameter *report* (make-sheet))
(set-cells *report*
  '(("A1" "Widget") ("B1" 1200.5) ("C1" 0.23)
    ("A2" "Gadget") ("B2" 890)    ("C2" -0.05)))

(defparameter *fmt* (make-formats))
(set-column-format *fmt* "B" '(:currency "$" 2))       ; B as money
(set-column-format *fmt* "C" '(:percent 1))            ; C as a percentage
(add-conditional *fmt* (lambda (v) (and (realp v) (minusp v)))
                 "LOSS" :column "C")                    ; negatives -> "LOSS"

(check "B1 formatted" (display-value *report* "B1" :formats *fmt*) "$1200.50")
(check "C2 formatted" (display-value *report* "C2" :formats *fmt*) "LOSS")

(ignore-errors (set-cell *report* "D1" '(/ 1 0)))
(multiple-value-bind (v e) (get-value *report* "D1")
  (declare (ignore v))
  (check "D1 as token" (error-token e) "#DIV/0!"))

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
  (check "A3 recomputed on load" (get-value reloaded "A3") 42))

(defparameter *wb-path*
  (merge-pathnames "cellisp-tutorial-wb.sheet" (uiop:temporary-directory)))
(save-workbook *wb* *wb-path*)

(let ((wb2 (load-workbook *wb-path*)))
  (check "reloaded Summary!A1"
         (get-value (find-sheet wb2 "Summary") "A1") 350))

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
(set-note  *invoice* "D7" "state sales tax")     ; a note, retrievable + saved
(check "note on D7" (cell-note *invoice* "D7") "state sales tax")

(defparameter *invoice-fmt* (make-formats))
(set-column-format *invoice-fmt* "C" '(:currency "$" 2))
(set-column-format *invoice-fmt* "D" '(:currency "$" 2))

(format t "~&--- invoice ---~%")
(print-sheet *invoice* :formats *invoice-fmt* :name "Invoice")
(show "invoice total (float)" (get-value *invoice* "D8"))   ; 36.975

(check "exact tax (34 * 7/80)" (* 34 7/80) 119/40)   ; 119/40 = 2.975, exactly

(format t "~&==== ~D checks, ~D failures ====~%" *checks* *failures*)
(format t "~&==== tutorial complete ====~%")
(when (plusp *failures*)
  (uiop:quit 1))                 ; fail the run if any claimed output changed
