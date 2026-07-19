(defpackage #:cellisp/test
  (:use #:cl #:cellisp)
  (:export #:run-tests))
(in-package #:cellisp/test)

(defvar *fails* 0)
(defvar *count* 0)
(defvar *evals* 0)   ; counts formula-body evaluations, for the dedup test

(defmacro check (form expected &optional (test '#'equal))
  `(progn
     (incf *count*)
     (let ((got ,form) (exp ,expected))
       (unless (funcall ,test got exp)
         (incf *fails*)
         (format t "FAIL: ~S~%  got ~S~%  expected ~S~%" ',form got exp)))))

(defmacro check-signals (condition form)
  `(progn
     (incf *count*)
     (unless (handler-case (progn ,form nil)
               (,condition () t))
       (incf *fails*)
       (format t "FAIL: ~S did not signal ~S~%" ',form ',condition))))

(defun run-tests ()
  (setf *fails* 0 *count* 0)

  ;; reference parsing round-trips
  (check (ref-string "A1") "A1" #'string=)
  (check (ref-string "AA10") "AA10" #'string=)
  (check (ref-string (parse-ref "Z1")) "Z1" #'string=)
  (check (parse-ref "B3") '(2 . 1))

  ;; malformed refs signal SHEET-ERROR, not a raw PARSE-INTEGER error
  (check-signals sheet-error (parse-ref "A1B"))
  (check-signals sheet-error (parse-ref "A1.5"))
  (check-signals sheet-error (parse-ref "12"))
  ;; a cons designator is validated too: must be (non-neg-int . non-neg-int)
  (check (parse-ref '(2 . 1)) '(2 . 1))
  (check-signals sheet-error (parse-ref '(-1 . 2)))
  (check-signals sheet-error (parse-ref '("a" . "b")))

  ;; literals and a simple formula
  (let ((s (make-sheet)))
    (set-cell s "A1" 10)
    (set-cell s "A2" 20)
    (set-cell s "A3" '(+ (cell "A1") (cell "A2")))
    (check (get-value s "A3") 30)

    ;; propagation: change A1, A3 updates
    (set-cell s "A1" 100)
    (check (get-value s "A3") 120)

    ;; transitive propagation
    (set-cell s "A4" '(* (cell "A3") 2))
    (check (get-value s "A4") 240)
    (set-cell s "A2" 0)
    (check (get-value s "A4") 200))

  ;; each cell computes at most once per sweep: A2 is read by B1 and B2,
  ;; whose values both feed C1 (a diamond), yet A2's formula body must run
  ;; exactly once when the shared input A1 changes.
  (let ((s (make-sheet)))
    (set-cell s "A1" 1)
    (set-cell s "A2" '(progn (incf *evals*) (cell "A1")))
    (set-cell s "B1" '(cell "A2"))
    (set-cell s "B2" '(cell "A2"))
    (set-cell s "C1" '(+ (cell "B1") (cell "B2")))
    (check (get-value s "C1") 2)
    (setf *evals* 0)
    (set-cell s "A1" 5)
    (check *evals* 1)                    ; not 2+ (once per reader) or more
    (check (get-value s "C1") 10))

  ;; set-cells: install a whole batch, then one sweep. Forward references
  ;; in any order resolve with no transient error; the return value is the
  ;; list of resulting values in input order; a later pair for a cell wins.
  (let ((s (make-sheet)))
    (check (set-cells s '(("A3" (+ (cell "A1") (cell "A2")))   ; forward refs
                          ("A1" 10) ("A2" 20)))
           '(30 10 20))
    (check (get-value s "A3") 30)
    (set-cells s '(("A1" 5) ("A1" 7)))   ; duplicate designator: last wins
    (check (get-value s "A1") 7)
    (check (get-value s "A3") 27))        ; dependent A3 recomputed once

  ;; ranges and aggregates
  (let ((s (make-sheet)))
    (loop for i from 1 to 5 do (set-cell s (format nil "A~D" i) i))
    (set-cell s "B1" '(sum (cells "A1" "A5")))
    (set-cell s "B2" '(average (cells "A1" "A5")))
    (set-cell s "B3" '(cnt (cells "A1" "A5")))
    (check (get-value s "B1") 15)
    (check (get-value s "B2") 3)
    (check (get-value s "B3") 5)
    (set-cell s "A5" 95)
    (check (get-value s "B1") 105))

  ;; aggregates ignore non-numeric cells; AVERAGE of no numbers signals.
  (let ((s (make-sheet)))
    (set-cell s "A1" 10)
    (set-cell s "A2" "text")
    (set-cell s "A3" 20)
    (set-cell s "B1" '(sum (cells "A1" "A3")))
    (set-cell s "B2" '(cnt (cells "A1" "A3")))
    (set-cell s "B3" '(average (cells "A1" "A3")))
    (check (get-value s "B1") 30)       ; "text" ignored
    (check (get-value s "B2") 2)        ; two numeric values
    (check (get-value s "B3") 15)       ; (10 + 20) / 2
    (set-cell s "C1" "a")
    (set-cell s "C2" "b")
    (check-signals sheet-error (set-cell s "B4" '(average (cells "C1" "C2")))))

  ;; arbitrary Lisp in formulas
  (let ((s (make-sheet)))
    (set-cell s "A1" 16)
    (set-cell s "A2" '(isqrt (cell "A1")))
    (set-cell s "A3" '(if (> (cell "A1") 10) "big" "small"))
    (check (get-value s "A2") 4)
    (check (get-value s "A3") "big" #'string=))

  ;; environment constants; the compiled thunk is cached but must still
  ;; re-evaluate on input changes and recompile when the formula changes.
  (let ((s (make-sheet :environment '((tax . 1/10)))))
    (set-cell s "A1" 200)
    (set-cell s "A2" '(* (cell "A1") tax))
    (check (get-value s "A2") 20)
    (set-cell s "A1" 500)                    ; cached thunk re-run
    (check (get-value s "A2") 50)
    (set-cell s "A2" '(+ (cell "A1") tax))   ; new formula -> recompiled
    (check (get-value s "A2") 5001/10))

  ;; environment values that are not self-evaluating (lists, symbols) must
  ;; be treated as data, not spliced into the compiled thunk as code.
  (let ((s (make-sheet :environment '((names . ("ann" "bob")) (mode . active)))))
    (set-cell s "A1" '(first names))
    (set-cell s "A2" '(string mode))
    (check (get-value s "A1") "ann" #'string=)
    (check (get-value s "A2") "ACTIVE" #'string=))

  ;; cycle detection
  (let ((s (make-sheet)))
    (set-cell s "A1" 1)
    (set-cell s "A2" '(cell "A1"))
    (check-signals cyclic-reference
                   (set-cell s "A1" '(cell "A2"))))

  ;; reading an empty cell errors
  (let ((s (make-sheet)))
    (check-signals unbound-cell (set-cell s "A1" '(cell "Z9"))))

  ;; eval error is captured and surfaced
  (let ((s (make-sheet)))
    (set-cell s "A1" 0)
    (check-signals cell-eval-error (set-cell s "A2" '(/ 1 (cell "A1")))))

  ;; a cell that errored recovers once its inputs become valid: the
  ;; dependency link to its precedent must survive the failed eval.
  (let ((s (make-sheet)))
    (set-cell s "A1" 0)
    (handler-case (set-cell s "A2" '(/ 100 (cell "A1"))) (cell-eval-error () nil))
    (check (precedents s "A2") '((0 . 0)))          ; A2 still records A1
    (check (dependents s "A1") '((1 . 0)))          ; A1 still knows A2 reads it
    (set-cell s "A1" 5)                             ; fix the divisor
    (multiple-value-bind (v e) (get-value s "A2")
      (check v 20)                                  ; A2 recomputed
      (check e nil)))

  ;; same recovery when the precedent started out empty (unbound-cell)
  (let ((s (make-sheet)))
    (handler-case (set-cell s "A2" '(+ 1 (cell "Z9"))) (unbound-cell () nil))
    (set-cell s "Z9" 41)
    (check (get-value s "A2") 42))

  ;; clear-cell breaks dependents
  (let ((s (make-sheet)))
    (set-cell s "A1" 5)
    (set-cell s "A2" '(* (cell "A1") 2))
    (check (get-value s "A2") 10)
    (clear-cell s "A1")
    (multiple-value-bind (v e) (get-value s "A2")
      (check v nil)
      (check (and e t) t)))

  (format t "~&~D checks, ~D failures.~%" *count* *fails*)
  (when (plusp *fails*) (error "Test failures: ~D" *fails*))
  t)
