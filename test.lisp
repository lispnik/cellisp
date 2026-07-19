(defpackage #:cellisp/test
  (:use #:cl #:cellisp)
  (:export #:run-tests))
(in-package #:cellisp/test)

(defvar *fails* 0)
(defvar *count* 0)
(defvar *evals* 0)   ; counts formula-body evaluations, for the dedup test
(defvar *vcount* 0)  ; volatile-cell recompute counter
(defvar *pcount* 0)  ; plain-cell recompute counter (contrast)
(defvar *ccount* 0)  ; cached-cell primary-computation counter

;; test-only mixins for the COMBINED-CLASS multi-mixin test (top level so
;; their types are known when RUN-TESTS is compiled)
(defclass demo-mixin-a () ((xa :initform :a)))
(defclass demo-mixin-b () ((xb :initform :b)))

;; named functions for the symbol-referenced serialization test
(defun ser-clamp (v) (min 100 (max 0 v)))
(defun ser-even (v) (and (integerp v) (evenp v)))
(defun ser-source () 7)
(defvar *ser-sink* '())
(defun ser-sink (v) (push v *ser-sink*))
(defvar *ser-obs* '())
(defun ser-obs (v) (push v *ser-obs*))
(defun ser-nonneg (v) (and (realp v) (>= v 0)))

;;; --- property-based testing -----------------------------------------
;;; Invariant: after any sequence of incremental edits, every cell equals a
;;; full RECALC-ALL. This guards the propagation short-circuit (and the whole
;;; recompute core) against leaving a stale value. Uses a small deterministic
;;; LCG so failures are reproducible.

(defvar *prng* 1)
(defun nextr (n)
  "Deterministic pseudo-random integer in [0, N) (LCG seeded via *PRNG*)."
  (setf *prng* (mod (+ (* *prng* 1103515245) 12345) 2147483648))
  (mod (ash *prng* -8) n))
(defun cref (i) (format nil "A~D" i))
(defun rand-formula (i)
  "A random formula for cell I (1-based) referencing only earlier cells — so
the graph stays acyclic and unbound-free — using lossy ops (mod/min/max/abs)
that frequently leave a value unchanged, exercising the short-circuit."
  (if (or (= i 1) (zerop (nextr 3)))
      (nextr 10)                                   ; literal 0..9
      (flet ((e () `(cell ,(cref (1+ (nextr (1- i)))))))
        (case (nextr 6)
          (0 `(+ ,(e) ,(e)))
          (1 `(* ,(e) ,(e)))
          (2 `(- ,(e) ,(e)))
          (3 `(max ,(e) ,(e)))
          (4 `(min ,(e) ,(e)))
          (t `(mod (abs ,(e)) 4))))))
(defun values-vector (s n)
  (loop for i from 1 to n collect (get-value s (cref i))))
(defun property-incremental=full (&key (trials 40) (n 12) (edits 50) (seed 1))
  "Run random trials; after each incremental edit assert the sheet's values
equal a full RECALC-ALL. Returns T iff the invariant always held."
  (setf *prng* seed)
  (dotimes (tr trials t)
    (let ((s (make-sheet)))
      (loop for i from 1 to n do (set-cell s (cref i) (rand-formula i)))
      (dotimes (e edits)
        (let ((i (1+ (nextr n))))
          (set-cell s (cref i) (rand-formula i)))  ; one incremental edit
        (let ((before (values-vector s n)))
          (recalc-all s)                           ; full recompute (the oracle)
          (unless (equal before (values-vector s n))
            (format t "~&property violation (trial ~D, edit ~D):~%  incremental ~S~%  full        ~S~%"
                    tr e before (values-vector s n))
            (return-from property-incremental=full nil)))))))

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

  ;; propagation short-circuit: a cell that recomputes to an UNCHANGED value
  ;; does not re-run its dependents; a changed value does.
  (let ((s (make-sheet)))
    (setf *evals* 0)
    (set-cell s "A1" 5)
    (set-cell s "A2" '(if (> (cell "A1") 0) 1 -1))          ; sign of A1
    (set-cell s "A3" '(progn (incf *evals*) (* 10 (cell "A2"))))
    (let ((n *evals*))
      (set-cell s "A1" 8)               ; A1 changes, but A2 (sign) stays 1
      (check (get-value s "A3") 10)     ; A3 value unchanged
      (check *evals* n)                 ; ...and A3 was NOT recomputed
      (set-cell s "A1" -3)              ; now A2 flips to -1
      (check (get-value s "A3") -10)    ; A3 recomputed
      (check (> *evals* n) t)))

  ;; property: incremental recompute always equals a full RECALC-ALL, over many
  ;; random acyclic sheets and edit sequences (guards the short-circuit).
  (check (property-incremental=full) t)

  ;; insert-row shifts cells down and rewrites references to keep them valid
  (let ((s (make-sheet)))
    (set-cell s "A1" 10)
    (set-cell s "A2" 20)
    (set-cell s "A3" '(+ (cell "A1") (cell "A2")))       ; 30
    (insert-row s 2)                                     ; blank row before row 2
    (check (get-value s "A1") 10)                        ; above the insert: fixed
    (check (get-value s "A2") nil)                       ; the new blank row
    (check (get-value s "A3") 20)                        ; old A2 moved down
    (check (get-formula s "A4") '(+ (cell "A1") (cell "A3")))  ; refs rewritten
    (check (get-value s "A4") 30))                       ; and still correct

  ;; delete-row shifts up; a reference to the deleted row becomes #REF! (errors)
  (let ((s (make-sheet)))
    (set-cell s "A1" 10)
    (set-cell s "A2" 20)
    (set-cell s "A3" '(* (cell "A2") 2))                 ; reads the doomed A2
    (set-cell s "A4" '(cell "A1"))                       ; reads A1 (safe)
    (delete-row s 2)                                     ; remove row 2
    (check (get-value s "A1") 10)
    (check (and (nth-value 1 (get-value s "A2")) t) t)   ; old A3 -> #REF! error
    (check (get-value s "A3") 10))                       ; old A4, still reads A1

  ;; insert-column shifts columns right and rewrites references
  (let ((s (make-sheet)))
    (set-cell s "A1" 5)
    (set-cell s "B1" '(* (cell "A1") 3))                 ; 15
    (insert-column s 1)                                  ; blank column before A
    (check (get-value s "A1") nil)                       ; new blank column
    (check (get-value s "B1") 5)                         ; old A1 moved right
    (check (get-formula s "C1") '(* (cell "B1") 3))      ; refs rewritten
    (check (get-value s "C1") 15))

  ;; delete-column shifts left; references to the deleted column become #REF!
  (let ((s (make-sheet)))
    (set-cell s "A1" 7)
    (set-cell s "B1" '(+ (cell "A1") 1))                 ; reads doomed A1
    (set-cell s "C1" 100)
    (delete-column s 1)                                  ; remove column A
    (check (and (nth-value 1 (get-value s "A1")) t) t)   ; old B1 -> #REF!
    (check (get-value s "B1") 100))                      ; old C1 moved left

  ;; a registry attribute (volatile) follows its cell across a structural edit
  (let ((s (make-sheet)))
    (set-cell s "A1" 1)
    (set-cell s "A2" 2 :volatile t)
    (insert-row s 1)                                     ; everything shifts down
    (check (volatile-p s "A3") t)                        ; volatile followed to A3
    (check (volatile-p s "A2") nil))

  ;; parse-ref accepts $ markers (they only annotate copy/paste absoluteness)
  (check (parse-ref "$A$1") '(0 . 0))
  (check (parse-ref "$B2") '(1 . 1))

  ;; copy-cell: relative references shift by the source->dest offset
  (let ((s (make-sheet)))
    (set-cell s "A1" 10) (set-cell s "A2" 100)
    (set-cell s "B1" '(* (cell "A1") 2))                 ; relative ref
    (copy-cell s "B1" "B2")                              ; paste one row down
    (check (get-formula s "B2") '(* (cell "A2") 2))      ; A1 -> A2
    (check (get-value s "B2") 200))

  ;; absolute ($) references do NOT shift on copy
  (let ((s (make-sheet)))
    (set-cell s "A1" 10) (set-cell s "A2" 100)
    (set-cell s "B1" '(* (cell "$A$1") 2))               ; absolute
    (copy-cell s "B1" "B2")
    (check (get-formula s "B2") '(* (cell "$A$1") 2))    ; unchanged
    (check (get-value s "B2") 20))                       ; still A1*2

  ;; mixed reference $A1 (absolute column, relative row)
  (let ((s (make-sheet)))
    (set-cell s "A1" 1) (set-cell s "A2" 2)
    (set-cell s "C1" '(cell "$A1"))                      ; abs col A, rel row
    (copy-cell s "C1" "D2")                              ; offset +1 row, +1 col
    (check (get-formula s "D2") '(cell "$A2"))           ; col stays A; row 1 -> 2
    (check (get-value s "D2") 2))

  ;; fill-range: copy a template across a rectangle, each adjusted relatively
  (let ((s (make-sheet)))
    (set-cell s "A1" 1) (set-cell s "A2" 2) (set-cell s "A3" 3)
    (set-cell s "B1" '(* (cell "A1") 10))                ; template
    (fill-range s "B1" "B2" "B3")                        ; fill B2:B3
    (check (get-value s "B1") 10)
    (check (get-value s "B2") 20)                        ; A2*10
    (check (get-value s "B3") 30))

  ;; named cells: a name aliases a ref and resolves in formulas
  (let ((s (make-sheet)))
    (set-cell s "A1" 10)
    (set-name s "price" "A1")
    (set-cell s "B1" '(* (cell "price") 2))              ; formula uses the name
    (check (get-value s "B1") 20)
    (check (name-ref s "price") '(0 . 0))
    (set-cell s "A1" 15)                                 ; edit via the ref
    (check (get-value s "B1") 30))                       ; name tracks the cell

  ;; a name follows its target cell across a structural edit
  (let ((s (make-sheet)))
    (set-cell s "A2" 7)
    (set-name s "total" "A2")
    (set-cell s "B1" '(cell "total"))                   ; = 7
    (insert-row s 1)                                     ; A2 -> A3, B1 -> B2
    (check (name-ref s "total") '(2 . 0))               ; name retargeted to A3
    (check (get-value s "B2") 7))                        ; still reads "total"

  ;; names round-trip through serialization
  (let ((s1 (make-sheet)))
    (set-cell s1 "A1" 5)
    (set-name s1 "x" "A1")
    (set-cell s1 "A2" '(+ (cell "x") 1))                ; 6
    (let* ((text (with-output-to-string (o) (write-sheet s1 o)))
           (s2 (with-input-from-string (i text) (read-sheet i))))
      (check (name-ref s2 "x") '(0 . 0))
      (check (get-value s2 "A2") 6)))

  ;; undo/redo of a formula edit, cascading to dependents
  (let ((s (make-sheet)))
    (set-cell s "A1" 1)
    (set-cell s "A2" '(* (cell "A1") 10))
    (set-cell s "A1" 5)                                  ; A2 -> 50
    (check (get-value s "A2") 50)
    (check (undo s) t)                                   ; revert A1 to 1
    (check (get-value s "A1") 1)
    (check (get-value s "A2") 10)                        ; dependent recomputed
    (check (redo s) t)                                   ; A1 back to 5
    (check (get-value s "A1") 5)
    (check (get-value s "A2") 50))

  ;; undo of a brand-new cell removes it; nothing-to-undo returns NIL
  (let ((s (make-sheet)))
    (set-cell s "A1" 7)
    (check (undo s) t)
    (multiple-value-bind (v e) (get-value s "A1")
      (check v nil) (check e nil))                       ; absent again
    (check (undo s) nil))                                ; stack empty

  ;; undo of clear-cell recreates the cell
  (let ((s (make-sheet)))
    (set-cell s "A1" 9)
    (clear-cell s "A1")
    (check (get-value s "A1") nil)
    (undo s)
    (check (get-value s "A1") 9))

  ;; undo of a set-cells batch reverts every cell at once
  (let ((s (make-sheet)))
    (set-cell s "A1" 1)
    (set-cells s '(("A1" 2) ("A2" 3)))
    (undo s)
    (check (get-value s "A1") 1)                         ; A1 reverted
    (multiple-value-bind (v e) (get-value s "A2")
      (check v nil) (check e nil)))                      ; A2 (new) removed

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

  ;; volatile cells (RAND()/NOW() model): recompute on EVERY sweep even when
  ;; no precedent changed; plain cells don't. Volatility is a subclass the
  ;; behavior dispatches on, toggled in place via CHANGE-CLASS.
  (let ((s (make-sheet)))
    (setf *vcount* 0 *pcount* 0)
    (set-cell s "A1" 1)
    (set-cell s "V1" '(incf *vcount*) :volatile t)   ; volatile
    (set-cell s "P1" '(incf *pcount*))               ; plain, identical shape
    (set-cell s "D1" '(cell "V1"))                   ; depends on the volatile
    (check (and (member (parse-ref "V1") (volatile-refs s) :test 'equal) t) t)
    (check (volatile-p s "V1") t)
    (check (volatile-p s "P1") nil)
    (let ((v (get-value s "V1")) (p (get-value s "P1")))
      (set-cell s "A1" 2)                            ; unrelated change
      (check (> (get-value s "V1") v) t)             ; V1 recomputed anyway
      (check (get-value s "P1") p)                   ; P1 did NOT recompute
      (check (get-value s "D1") (get-value s "V1"))) ; dependent tracks V1
    ;; demote V1 to a plain cell; it stops recomputing on unrelated sweeps
    (set-cell s "V1" '(incf *vcount*) :volatile nil)
    (check (volatile-p s "V1") nil)
    (let ((v (get-value s "V1")))
      (set-cell s "A1" 3)                            ; unrelated change
      (check (get-value s "V1") v)))                 ; now frozen

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

  ;; external cell: value comes from a thunk, re-pulled on recompute
  (let ((s (make-sheet)) (feed 10))
    (set-external s "A1" (lambda () feed))
    (set-cell s "A2" '(* 2 (cell "A1")))
    (check (get-value s "A1") 10)
    (check (get-value s "A2") 20)
    (setf feed 15)
    (recalc s "A1")                       ; re-pull the source
    (check (get-value s "A1") 15)
    (check (get-value s "A2") 30))         ; dependent followed

  ;; async cell: non-blocking read of the last value; DELIVER-ASYNC pushes a
  ;; new value out-of-band and recomputes dependents.
  (let ((s (make-sheet)) (captured nil))
    (set-async s "A1" (lambda (deliver) (setf captured deliver)) :initial 0)
    (set-cell s "A2" '(+ 100 (cell "A1")))
    (check (get-value s "A1") 0)          ; initial
    (check (get-value s "A2") 100)
    (refresh-async s "A1")                ; fetcher stashes the callback
    (check (get-value s "A1") 0)          ; still last value (non-blocking)
    (funcall captured 42)                 ; the value "arrives"
    (check (get-value s "A1") 42)
    (check (get-value s "A2") 142))       ; dependent recomputed

  ;; async delivery from a real worker thread, serialized by the sheet lock
  (let ((s (make-sheet)) (th nil))
    (set-async s "B1"
               (lambda (deliver)
                 (setf th (bt:make-thread (lambda () (funcall deliver 7)))))
               :initial 0)
    (set-cell s "B2" '(* 10 (cell "B1")))
    (refresh-async s "B1")
    (bt:join-thread th)                   ; wait for the delivery to land
    (check (get-value s "B1") 7)
    (check (get-value s "B2") 70))

  ;; observed cell: subscribers fire after a sweep only when the value changed
  (let* ((s (make-sheet)) (log '())
         (cb (lambda (v) (push v log))))
    (set-cell s "A1" 1)
    (set-cell s "A2" '(* 10 (cell "A1")))
    (observe s "A2" cb)
    (set-cell s "A1" 2) (check (first log) 20)    ; A2 20 -> notify
    (set-cell s "A1" 2) (check (length log) 1)    ; unchanged -> no notify
    (set-cell s "A1" 3) (check (first log) 30) (check (length log) 2)
    (unobserve s "A2" cb)
    (set-cell s "A1" 4) (check (length log) 2))   ; unobserved -> silent
  ;; OBSERVE composes with any cell kind: observing an EXTERNAL cell promotes
  ;; it to a combined class carrying OBSERVABLE-MIXIN, keeping its value source.
  (let ((s (make-sheet)) (feed 5) (log '()))
    (set-external s "A1" (lambda () feed))
    (observe s "A1" (lambda (v) (push v log)))
    (let ((cell (cellisp::find-cell s (parse-ref "A1"))))
      (check (and (typep cell 'observable-mixin) t) t)             ; observation added
      (check (and (typep cell 'external-cell) t) t))               ; source preserved
    (recalc s "A1") (check (first log) 5)                   ; sweep -> first notify
    (setf feed 8) (recalc s "A1")
    (check (get-value s "A1") 8)                            ; still external
    (check (first log) 8))                                  ; and notifies

  ;; the three axes compose at once: external source + volatile + observed.
  (let ((s (make-sheet)) (tick 0) (log '()))
    (set-external s "A1" (lambda () (incf tick)))           ; value source
    (set-volatile s "A1" t)                                 ; recompute cadence
    (observe s "A1" (lambda (v) (push v log)))              ; notification
    (let ((cell (cellisp::find-cell s (parse-ref "A1"))))
      (check (and (typep cell 'external-cell) t) t)
      (check (and (typep cell 'observable-mixin) t) t)
      (check (volatile-p s "A1") t))
    (let ((before (length log)))
      (recalc-all s)                                        ; volatile -> re-pulls
      (recalc-all s)
      (check (> (length log) before) t)))                  ; external+volatile+observed

  ;; change-class morphs a cell in place, preserving value and links
  (let ((s (make-sheet)))
    (set-cell s "A1" 5)
    (set-cell s "A2" '(* 2 (cell "A1")))
    (observe s "A2" (lambda (v) (declare (ignore v))))   ; plain -> observable
    (check (and (typep (cellisp::find-cell s (parse-ref "A2")) 'observable-mixin) t) t)
    (check (get-value s "A2") 10)                          ; value survived
    (check (and (member (parse-ref "A1") (precedents s "A2") :test 'equal) t) t)
    (set-cell s "A1" 7)
    (check (get-value s "A2") 14))                         ; still recomputes

  ;; COMBINED-CLASS generalizes to any number of mixins, order-independent:
  ;; a set of mixins over a base maps to one memoized class regardless of the
  ;; order given, and instances are TYPEP every constituent. (DEMO-MIXIN-A/B
  ;; are defined at top level, below.)
  (let ((c1 (cellisp::combined-class
             'cell '(demo-mixin-a demo-mixin-b observable-mixin)))
        (c2 (cellisp::combined-class
             'cell '(observable-mixin demo-mixin-b demo-mixin-a))))
    (check (eq c1 c2) t)                                   ; permutation -> one class
    (let ((inst (make-instance c1)))
      (check (and (typep inst 'demo-mixin-a) t) t)
      (check (and (typep inst 'demo-mixin-b) t) t)
      (check (and (typep inst 'observable-mixin) t) t)
      (check (and (typep inst 'cell) t) t)))

  ;; a value-source change PRESERVES existing mixins: observe a plain cell,
  ;; then make it external — it stays observable and becomes external.
  (let ((s (make-sheet)) (feed 3) (log '()))
    (set-cell s "A1" 0)
    (observe s "A1" (lambda (v) (push v log)))            ; observable + cell
    (set-external s "A1" (lambda () feed))                ; -> observable + external
    (let ((cell (cellisp::find-cell s (parse-ref "A1"))))
      (check (and (typep cell 'observable-mixin) t) t)            ; mixin kept
      (check (and (typep cell 'external-cell) t) t))              ; source changed
    (recalc s "A1")
    (check (get-value s "A1") 3)
    (check (first log) 3))                                 ; still notifies

  ;; unobserving the last subscriber drops OBSERVABLE-MIXIN (via REMOVE-MIXIN),
  ;; leaving the value source intact.
  (let ((s (make-sheet)) (cb (lambda (v) (declare (ignore v)))))
    (set-external s "A1" (lambda () 9))
    (observe s "A1" cb)
    (check (and (typep (cellisp::find-cell s (parse-ref "A1")) 'observable-mixin) t) t)
    (unobserve s "A1" cb)
    (let ((cell (cellisp::find-cell s (parse-ref "A1"))))
      (check (typep cell 'observable-mixin) nil)          ; mixin removed
      (check (and (typep cell 'external-cell) t) t))              ; source preserved
    (check (get-value s "A1") 9))

  ;; readonly-mixin: locks user reassignment, but the cell still recomputes
  ;; from its precedents. SET-READONLY toggles it.
  (let ((s (make-sheet)))
    (set-cell s "A1" 5)
    (set-cell s "A2" '(* 2 (cell "A1")))
    (set-readonly s "A2" t)
    (check (and (typep (cellisp::find-cell s (parse-ref "A2")) 'readonly-mixin) t) t)
    (check (cell-writable-p (cellisp::find-cell s (parse-ref "A2"))) nil)
    (check-signals readonly-cell (set-cell s "A2" 99))    ; can't reassign
    (check-signals readonly-cell (clear-cell s "A2"))     ; can't clear
    (check (get-value s "A2") 10)                          ; unchanged
    (set-cell s "A1" 7)                                    ; precedent changes
    (check (get-value s "A2") 14)                          ; still recomputes
    (set-readonly s "A2" nil)                              ; unlock
    (set-cell s "A2" 99)                                   ; now allowed
    (check (get-value s "A2") 99))

  ;; TWO real mixins compose on one cell: readonly + observable, each guarding
  ;; a different generic (cell-writable-p vs cell-swept).
  (let ((s (make-sheet)) (log '()))
    (set-cell s "A1" 1)
    (set-cell s "A2" '(* 10 (cell "A1")))
    (observe s "A2" (lambda (v) (push v log)))            ; observable
    (set-readonly s "A2" t)                               ; + readonly
    (let ((cell (cellisp::find-cell s (parse-ref "A2"))))
      (check (and (typep cell 'observable-mixin) t) t)
      (check (and (typep cell 'readonly-mixin) t) t))             ; both present
    (check-signals readonly-cell (set-cell s "A2" 5))     ; readonly guards
    (set-cell s "A1" 3)                                    ; recompute -> 30
    (check (get-value s "A2") 30)
    (check (first log) 30))                                ; observer still fires

  ;; readonly also blocks changing a cell's value source
  (let ((s (make-sheet)))
    (set-cell s "A1" 1)
    (set-readonly s "A1" t)
    (check-signals readonly-cell (set-external s "A1" (lambda () 9))))

  ;; logged-mixin: records the value history, collapsing consecutive dups
  (let ((s (make-sheet)))
    (set-cell s "A1" 1)
    (set-cell s "A2" '(* 10 (cell "A1")))
    (set-logged s "A2" t)
    (set-cell s "A1" 2)                   ; A2 -> 20
    (set-cell s "A1" 2)                   ; A2 -> 20 again (not re-logged)
    (set-cell s "A1" 3)                   ; A2 -> 30
    (check (cell-log s "A2") '(20 30)))   ; oldest first, deduped

  ;; logged + observable both hook CELL-SWEPT (an :after and a primary method)
  ;; and BOTH fire — composition via CLOS method combination, not override.
  (let ((s (make-sheet)) (log '()))
    (set-cell s "A1" 1)
    (set-cell s "A2" '(* 10 (cell "A1")))
    (observe s "A2" (lambda (v) (push v log)))   ; primary cell-swept
    (set-logged s "A2" t)                        ; :after cell-swept
    (let ((cell (cellisp::find-cell s (parse-ref "A2"))))
      (check (and (typep cell 'observable-mixin) t) t)
      (check (and (typep cell 'logged-mixin) t) t))
    (set-cell s "A1" 4)                          ; A2 -> 40
    (check (first log) 40)                        ; observer fired
    (check (cell-log s "A2") '(40)))              ; logger recorded

  ;; three mixins on one cell at once: observable + readonly + logged, all
  ;; active while the cell recomputes from its precedent.
  (let ((s (make-sheet)) (log '()))
    (set-cell s "B1" 1)
    (set-cell s "A1" '(* 100 (cell "B1")))
    (observe s "A1" (lambda (v) (push v log)))
    (set-readonly s "A1" t)
    (set-logged s "A1" t)
    (let ((c (cellisp::find-cell s (parse-ref "A1"))))
      (check (and (typep c 'observable-mixin) t) t)
      (check (and (typep c 'readonly-mixin) t) t)
      (check (and (typep c 'logged-mixin) t) t))
    (check-signals readonly-cell (set-cell s "A1" 0))   ; readonly guards
    (set-cell s "B1" 2)                                 ; A1 -> 200 (recompute)
    (check (first log) 200)                              ; observer fired
    (check (cell-log s "A1") '(200)))                    ; logger recorded

  ;; cached-mixin: memoizes via an :AROUND on compute-value — the real
  ;; computation runs only when a precedent changed. The precedent link
  ;; survives a cache hit, so a later input change still recomputes.
  (let ((s (make-sheet)))
    (setf *ccount* 0)
    (set-cell s "A1" 5)
    (set-cell s "A2" '(progn (incf *ccount*) (* 2 (cell "A1"))))
    (set-cached s "A2" t)
    (recalc s "A2")                        ; first cached run -> snapshot inputs
    (let ((n *ccount*))
      (recalc s "A2")                      ; inputs unchanged -> primary SKIPPED
      (check *ccount* n)                   ; counter did not advance
      (check (get-value s "A2") 10))       ; value still correct
    (set-cell s "A1" 7)                    ; input changed -> recompute
    (check (get-value s "A2") 14)          ; link survived the cache hits
    (check (> *ccount* 1) t))              ; primary ran again on the change

  ;; cached composes with observable: the :around (compute-value) and the
  ;; primary (cell-swept) live on different generics, so both apply — a cache
  ;; hit skips recompute AND (value unchanged) fires no observer.
  (let ((s (make-sheet)) (log '()))
    (setf *ccount* 0)
    (set-cell s "A1" 1)
    (set-cell s "A2" '(progn (incf *ccount*) (* 10 (cell "A1"))))
    (observe s "A2" (lambda (v) (push v log)))
    (set-cached s "A2" t)
    (recalc s "A2")                        ; baseline
    (let ((n *ccount*) (len (length log)))
      (recalc s "A2")                      ; unchanged -> skip + no notify
      (check *ccount* n)
      (check (length log) len))
    (set-cell s "A1" 3)                    ; changed -> recompute + notify
    (check (get-value s "A2") 30)
    (check (first log) 30))

  ;; debounced-mixin: a burst of changes coalesces into ONE trailing fire of
  ;; the settled value. Injected scheduler queues thunks so we settle by hand.
  (let ((s (make-sheet)) (fired '()) (pending '()))
    (set-cell s "A1" 0)
    (set-cell s "A2" '(* 10 (cell "A1")))
    (debounce s "A2" (lambda (v) (push v fired))
              :scheduler (lambda (thunk) (push thunk pending)))
    (set-cell s "A1" 1)                    ; A2 -> 10, deferred fire #1
    (set-cell s "A1" 2)                    ; A2 -> 20, deferred fire #2
    (set-cell s "A1" 3)                    ; A2 -> 30, deferred fire #3
    (check (length pending) 3)             ; three fires queued
    (check fired '())                      ; none fired yet (all deferred)
    (dolist (th (reverse pending)) (funcall th))  ; settle: run them in order
    (check fired '(30)))                    ; only the latest generation fired

  ;; debounced fire arriving on a real worker thread, serialized by the lock
  (let ((s (make-sheet)) (fired '()) (th nil))
    (set-cell s "A1" 1)
    (set-cell s "A2" '(* 10 (cell "A1")))
    (debounce s "A2" (lambda (v) (push v fired))
              :scheduler (lambda (thunk) (setf th (bt:make-thread thunk))))
    (set-cell s "A1" 5)                    ; A2 -> 50, fires on the worker
    (bt:join-thread th)
    (check (first fired) 50))

  ;; observe + debounce on one cell keep SEPARATE subscriber lists (mixin
  ;; slots must not merge): the observer fires immediately each change while
  ;; the debounced notification stays deferred and coalesced.
  (let ((s (make-sheet)) (obs '()) (deb '()) (queue '()))
    (set-cell s "A1" 0)
    (set-cell s "A2" '(* 10 (cell "A1")))
    (observe s "A2" (lambda (v) (push v obs)))
    (debounce s "A2" (lambda (v) (push v deb))
              :scheduler (lambda (th) (push th queue)))
    (set-cell s "A1" 1)
    (set-cell s "A1" 2)
    (check obs '(20 10))                   ; observer fired on both changes
    (check deb '())                        ; debounced fired nothing yet
    (dolist (th (reverse queue)) (funcall th))
    (check deb '(20)))                      ; debounced fired once, settled value

  ;; default-mixin: computation errors fall back to a default value
  (let ((s (make-sheet)))
    (set-cell s "A1" 0)
    (set-default s "A2" -1)
    (set-cell s "A2" '(/ 10 (cell "A1")))       ; divide by zero
    (check (get-value s "A2") -1)               ; default, not an error
    (set-cell s "A1" 5)
    (check (get-value s "A2") 2))               ; recovers to the real value

  ;; transformed-mixin: post-process the value (here, clamp to 0..100)
  (let ((s (make-sheet)))
    (set-transform s "A2" (lambda (v) (min 100 (max 0 v))))
    (set-cell s "A1" 150)
    (set-cell s "A2" '(cell "A1"))
    (check (get-value s "A2") 100)
    (set-cell s "A1" -5)
    (check (get-value s "A2") 0))

  ;; validated-mixin: an out-of-spec value signals INVALID-VALUE
  (let ((s (make-sheet)))
    (set-validator s "A1" #'evenp)
    (set-cell s "A1" 4)
    (check (get-value s "A1") 4)
    (check-signals invalid-value (set-cell s "A1" 3)))

  ;; timed-mixin: accumulates run count across recomputes
  (let ((s (make-sheet)))
    (set-cell s "A1" 1)
    (set-timed s "A2" t)
    (set-cell s "A2" '(cell "A1"))              ; run 1
    (set-cell s "A1" 2)                         ; run 2
    (multiple-value-bind (total count) (cell-timing s "A2")
      (check (integerp total) t)
      (check count 2)))

  ;; retry-mixin: a transient error is retried until it succeeds
  (let ((s (make-sheet)) (tries 0))
    (set-retry s "A1" 3)
    (set-external s "A1" (lambda () (incf tries) (if (< tries 3) (error "flaky") 42)))
    (check (get-value s "A1") 42)
    (check (>= tries 3) t))

  ;; ttl-cached-mixin: reuse the value within a TTL (injected clock)
  (let ((s (make-sheet)) (clock 0))
    (setf *ccount* 0)
    (set-cell s "A1" 5)
    (set-ttl s "A2" 10 :clock (lambda () clock))
    (set-cell s "A2" '(progn (incf *ccount*) (* 2 (cell "A1"))))
    (check *ccount* 1)
    (setf clock 5) (recalc s "A2") (check *ccount* 1)   ; within TTL -> reuse
    (setf clock 20) (recalc s "A2") (check *ccount* 2)  ; expired -> recompute
    (check (get-value s "A2") 10))

  ;; throttled-mixin: leading-edge — fire, then suppress for the interval
  (let ((s (make-sheet)) (clock 0) (fired '()))
    (set-cell s "A1" 0)
    (set-cell s "A2" '(cell "A1"))
    (throttle s "A2" (lambda (v) (push v fired)) :interval 10 :clock (lambda () clock))
    (set-cell s "A1" 1)                          ; fire (leading)
    (setf clock 5) (set-cell s "A1" 2)           ; within interval -> suppressed
    (setf clock 15) (set-cell s "A1" 3)          ; interval passed -> fire
    (check fired '(3 1)))

  ;; threshold-mixin: fire only on crossing the level
  (let ((s (make-sheet)) (events '()))
    (set-cell s "A1" 0)
    (set-cell s "A2" '(cell "A1"))
    (on-threshold s "A2" 10 (lambda (side v) (push (list side v) events)))
    (set-cell s "A1" 5)                          ; still below -> no fire
    (set-cell s "A1" 15)                         ; crosses up -> fire
    (set-cell s "A1" 12)                         ; still above -> no fire
    (set-cell s "A1" 3)                          ; crosses down -> fire
    (check events '((:below 3) (:above 15))))

  ;; stats-mixin: running count/sum/min/max/mean over the values taken
  (let ((s (make-sheet)))
    (set-cell s "A1" 1)
    (set-stats s "A2" t)
    (set-cell s "A2" '(* 10 (cell "A1")))       ; 10
    (set-cell s "A1" 2)                          ; 20
    (set-cell s "A1" 3)                          ; 30
    (let ((st (cell-stats s "A2")))
      (check (getf st :count) 3)
      (check (getf st :min) 10)
      (check (getf st :max) 30)
      (check (getf st :mean) 20)))

  ;; persisted-mixin: a sink is called with each new value
  (let ((s (make-sheet)) (store '()))
    (set-cell s "A1" 1)
    (set-persist s "A2" (lambda (v) (push v store)))
    (set-cell s "A2" '(* 10 (cell "A1")))       ; 10 -> sink
    (set-cell s "A1" 2)                          ; 20 -> sink
    (set-cell s "A1" 2)                          ; unchanged -> no sink
    (check store '(20 10)))

  ;; logged with :limit keeps only the most recent N values
  (let ((s (make-sheet)))
    (set-cell s "A1" 0)
    (set-cell s "A2" '(cell "A1"))
    (set-logged s "A2" t :limit 2)
    (dolist (n '(1 2 3 4)) (set-cell s "A1" n))
    (check (cell-log s "A2") '(3 4)))            ; last two, oldest first

  ;; append-only-mixin: the formula can be set once, then not changed
  (let ((s (make-sheet)))
    (set-append-only s "A1" t)
    (set-cell s "A1" 5)                          ; first write OK
    (check (get-value s "A1") 5)
    (check-signals readonly-cell (set-cell s "A1" 9)))

  ;; typed-input-mixin: a set whose formula fails the predicate is rejected
  (let ((s (make-sheet)))
    (set-typed-input s "A1" #'numberp)
    (set-cell s "A1" 5)
    (check (get-value s "A1") 5)
    (check-signals readonly-cell (set-cell s "A1" "hi")))

  ;; frozen (a registry attribute): held at its value, skipped on recompute
  (let ((s (make-sheet)))
    (set-cell s "A1" 5)
    (set-cell s "A2" '(* 2 (cell "A1")))
    (check (get-value s "A2") 10)
    (set-frozen s "A2" t)
    (set-cell s "A1" 100)                        ; A1 changes...
    (check (get-value s "A2") 10)               ; ...but frozen A2 is held
    (check (frozen-p s "A2") t)
    (set-frozen s "A2" nil)
    (recalc s "A2")
    (check (get-value s "A2") 200))              ; unfrozen -> recomputes

  ;; versioned-mixin: records the formula-edit history via NOTE-SET
  (let ((s (make-sheet)))
    (set-cell s "A1" 1)
    (set-versioned s "A1" t)                     ; seed with current formula
    (set-cell s "A1" 2)
    (set-cell s "A1" '(+ 1 1))
    (check (cell-versions s "A1") '(1 2 (+ 1 1))))

  ;; audited-mixin: full provenance — WITH-ACTOR supplies the author, the
  ;; injectable *audit-clock* supplies deterministic timestamps.
  (let ((s (make-sheet)) (tick 100))
    (let ((*audit-clock* (lambda () (incf tick))))
      (set-audited s "A1" t)
      (with-actor ("alice") (set-cell s "A1" 1))       ; time 101
      (with-actor ("bob")   (set-cell s "A1" 2))       ; time 102
      (let ((trail (cell-audit s "A1")))
        (check (length trail) 2)
        (check (getf (first trail) :actor) "alice" #'string=)
        (check (getf (first trail) :formula) 1)
        (check (getf (first trail) :time) 101)
        (check (getf (second trail) :actor) "bob" #'string=)
        (check (getf (second trail) :time) 102))))

  ;; audited + versioned compose: both NOTE-SET :after methods run
  (let ((s (make-sheet)))
    (let ((*audit-clock* (constantly 0)))
      (set-audited s "A1" t)
      (set-versioned s "A1" t)
      (with-actor ("carol") (set-cell s "A1" 7))
      (check (cell-versions s "A1") '(7))              ; versioned recorded
      (check (getf (first (cell-audit s "A1")) :actor) "carol" #'string=)))

  ;; throttled + audited on one cell dispatch on independent seams: every set
  ;; is audited (NOTE-SET) while value-change alerts are throttled (CELL-SWEPT).
  ;; One logical clock drives both the timestamps and the throttle window.
  (let ((s (make-sheet)) (tick 0) (alerts '()))
    (let ((*audit-clock* (lambda () tick)))
      (set-audited s "A1" t)
      (throttle s "A1" (lambda (v) (push (list tick v) alerts))
                :interval 10 :clock (lambda () tick))
      (flet ((adjust (who val) (with-actor (who) (set-cell s "A1" val))))
        (setf tick 1)  (adjust "alice" 50)   ; change -> alert (leading edge)
        (setf tick 3)  (adjust "bob"   60)   ; within window -> throttled
        (setf tick 5)  (adjust "alice" 70)   ; within window -> throttled
        (setf tick 15) (adjust "carol" 80))) ; window elapsed -> alert
    (check (reverse alerts) '((1 50) (15 80)))         ; two alerts, four changes
    (let ((trail (cell-audit s "A1")))
      (check (length trail) 4)                          ; but all four audited
      (check (getf (first trail) :actor) "alice" #'string=)
      (check (getf (first trail) :time) 1)
      (check (getf (fourth trail) :actor) "carol" #'string=)
      (check (getf (fourth trail) :formula) 80)))

  ;; stats + persisted both hook CELL-SWEPT :after, so each reading is written
  ;; to the sink AND folded into the running stats in the same sweep.
  (let ((s (make-sheet)) (store '()))
    (set-cell s "A1" 20)
    (set-cell s "A2" '(cell "A1"))                     ; A2 mirrors the reading
    (set-stats   s "A2" t)                             ; (attached after the initial 20)
    (set-persist s "A2" (lambda (v) (push v store)))
    (dolist (n '(25 18 30 22 27)) (set-cell s "A1" n))
    (check (reverse store) '(25 18 30 22 27))          ; every reading persisted
    (let ((st (cell-stats s "A2")))
      (check (getf st :count) 5)
      (check (getf st :min) 18)
      (check (getf st :max) 30)
      (check (getf st :sum) 122)
      (check (getf st :mean) 122/5)))                   ; 24.4

  ;; cached + validated both wrap COMPUTE-VALUE (:around), so they CHAIN:
  ;; cached is outermost, so a cache hit skips revalidation; an invalid value
  ;; signals inside cached's call-next-method and is never cached.
  (let ((s (make-sheet)))
    (setf *ccount* 0)
    (set-validator s "A2" #'evenp)
    (set-cached s "A2" t)
    (set-cell s "A1" 4)
    (set-cell s "A2" '(progn (incf *ccount*) (cell "A1")))
    (check (get-value s "A2") 4) (check *ccount* 1)     ; compute + validate + cache
    (recalc s "A2")
    (check (get-value s "A2") 4) (check *ccount* 1)     ; cache hit -> no recompute
    (set-cell s "A1" 5)                                 ; odd -> validation fails
    (multiple-value-bind (v e) (get-value s "A2")
      (check v nil)
      (check (and (typep e 'invalid-value) t) t))
    (check *ccount* 2)                                  ; missed cache, computed once
    (set-cell s "A1" 8)                                 ; even again
    (check (get-value s "A2") 8) (check *ccount* 3))    ; invalid was not cached

  ;; debounced + logged both hook CELL-SWEPT :after but with opposite intent:
  ;; logged keeps EVERY change immediately, debounced coalesces the burst into
  ;; one settled fire. Manual scheduler makes the settle explicit.
  (let ((s (make-sheet)) (settled '()) (queue '()))
    (set-cell s "A1" 0)
    (set-cell s "A2" '(* 10 (cell "A1")))
    (set-logged s "A2" t)
    (debounce s "A2" (lambda (v) (push v settled))
              :scheduler (lambda (th) (push th queue)))
    (dolist (n '(1 2 3 4)) (set-cell s "A1" n))         ; A2: 10,20,30,40
    (check (cell-log s "A2") '(10 20 30 40))            ; logged every change
    (check settled '())                                 ; debounced deferred all
    (dolist (th (reverse queue)) (funcall th))          ; settle
    (check (cell-log s "A2") '(10 20 30 40))            ; history unchanged
    (check settled '(40)))                              ; one settled fire, the endpoint

  ;; default wraps validated (default sorts first, so its :around is outer):
  ;; validated's INVALID-VALUE is caught by default's handler, giving soft
  ;; validation — both a bad value and a compute error fall back to the
  ;; default, and no error surfaces.
  (let ((s (make-sheet)))
    (set-validator s "A2" #'plusp)                      ; must be positive
    (set-default s "A2" -1)                             ; else fall back to -1
    (set-cell s "A1" 4)
    (set-cell s "A2" '(/ 100 (cell "A1")))
    (flet ((val (n) (set-cell s "A1" n)
             (multiple-value-bind (v e) (get-value s "A2")
               (check e nil)                            ; never errors
               v)))
      (check (val 4) 25)                                ; positive -> ok
      (check (val 0) -1)                                ; 100/0 error -> default
      (check (val -5) -1)                               ; -20 fails plusp -> default
      (check (val 2) 50)))                              ; positive -> ok

  ;; retry (outer) wraps timed (inner) on compute-value: retry absorbs the
  ;; flaky source's failures, while timed counts only successful computes —
  ;; so source attempts (3) and timed count (1) differ on the same recompute.
  (let ((s (make-sheet)))
    (setf *ccount* 0)                                   ; reuse as source-attempt counter
    (set-retry s "A1" 3)
    (set-timed s "A1" t)
    (set-external s "A1"
                  (lambda () (incf *ccount*)
                          (if (< *ccount* 3) (error "transient") 42)))
    (check (get-value s "A1") 42)                       ; recovered despite failures
    (check *ccount* 3)                                  ; source tried 3 times
    (check (nth-value 1 (cell-timing s "A1")) 1)        ; timed: 1 successful compute
    (recalc s "A1")                                     ; source healthy now
    (check (get-value s "A1") 42)
    (check *ccount* 4)                                  ; one attempt this time
    (check (nth-value 1 (cell-timing s "A1")) 2))       ; timed accumulates -> 2

  ;; :after CELL-SWEPT sinks skip errored cells: a validation failure nulls the
  ;; value and stores the error, so PERSISTED does not emit a spurious NIL.
  (let ((s (make-sheet)) (store '()))
    (set-cell s "A1" 10)
    (set-validator s "A2" #'plusp)
    (set-persist s "A2" (lambda (v) (push v store)))
    (set-cell s "A2" '(cell "A1"))                       ; 10 valid -> persist
    (set-cell s "A1" 25)                                 ; 25 valid -> persist
    (set-cell s "A1" -3)                                 ; invalid -> errored -> skipped
    (set-cell s "A1" 40)                                 ; 40 valid -> persist
    (check (reverse store) '(10 25 40)))                 ; no NIL gap

  ;; observers likewise are not notified when the cell errors
  (let ((s (make-sheet)) (seen '()))
    (set-cell s "A1" 4)
    (set-validator s "A2" #'evenp)
    (set-cell s "A2" '(cell "A1"))
    (observe s "A2" (lambda (v) (push v seen)))
    (set-cell s "A1" 6)                                  ; even -> notify
    (set-cell s "A1" 5)                                  ; odd -> errored -> silent
    (set-cell s "A1" 8)                                  ; even -> notify
    (check seen '(8 6)))

  ;; three composition modes at once: transformed (:around compute-value) +
  ;; observable (primary cell-swept) + stats (:after cell-swept).
  (let ((s (make-sheet)) (seen '()))
    (set-cell s "A1" 0)
    (set-transform s "A2" (lambda (v) (* v v)))  ; square
    (set-cell s "A2" '(cell "A1"))
    (observe s "A2" (lambda (v) (push v seen)))
    (set-stats s "A2" t)
    (let ((c (cellisp::find-cell s (parse-ref "A2"))))
      (check (and (typep c 'transformed-mixin) t) t)
      (check (and (typep c 'observable-mixin) t) t)
      (check (and (typep c 'stats-mixin) t) t))
    (set-cell s "A1" 3)                          ; A2 = 9 (squared)
    (check (get-value s "A2") 9)
    (check (first seen) 9)
    (check (getf (cell-stats s "A2") :max) 9))

  ;; live redefinition: adding a slot migrates existing instances — the CLOS
  ;; capability that motivates CELL being a class rather than a struct.
  (progn
    (defclass redef-demo (cell) ((a :initform 1)))
    (let ((inst (make-instance 'redef-demo)))
      (check (slot-value inst 'a) 1)
      (defclass redef-demo (cell)                          ; redefine with slot B
        ((a :initform 1) (b :initform 99)))
      (check (slot-value inst 'b) 99)     ; old instance gained B via migration
      (check (slot-value inst 'a) 1)))    ; and kept A

  ;; the sheet lock serializes concurrent writers without corrupting the graph
  (let ((s (make-sheet)))
    (set-cell s "A1" 0)
    (set-cell s "A2" '(cell "A1"))
    (let ((threads (loop repeat 8 collect
                         (bt:make-thread
                          (lambda () (dotimes (i 100) (set-cell s "A1" i)))))))
      (mapc #'bt:join-thread threads))
    (check (integerp (get-value s "A1")) t)
    (check (numberp (get-value s "A2")) t))

  ;; serialization: formulas + environment + declarative attributes round-trip
  ;; through a stream; values recompute on load.
  (let ((s1 (make-sheet :environment '((tax . 1/10)))))
    (set-cells s1 '(("A1" 10) ("A2" 20)
                    ("A3" (+ (cell "A1") (cell "A2")))
                    ("B1" (* (cell "A1") tax))))
    (set-volatile s1 "A1" t)
    (set-readonly s1 "A3" t)
    (set-frozen s1 "B1" t)
    (set-append-only s1 "A2" t)
    (let* ((text (with-output-to-string (o) (write-sheet s1 o)))
           (s2 (with-input-from-string (i text) (read-sheet i))))
      (check (get-value s2 "A3") 30)                        ; recomputed on load
      (check (get-value s2 "B1") 1)                         ; env constant preserved
      (check (get-formula s2 "A3") '(+ (cell "A1") (cell "A2")))
      (check (volatile-p s2 "A1") t)                        ; attributes restored
      (check (frozen-p s2 "B1") t)
      (check-signals readonly-cell (set-cell s2 "A3" 0))    ; readonly restored
      (check-signals readonly-cell (set-cell s2 "A2" 99)))) ; append-only restored

  ;; serialization also captures durable history — audit log, formula
  ;; versions, value log, stats — and the restored mixins are live.
  (let ((s1 (make-sheet)))
    (let ((*audit-clock* (constantly 42)))
      (set-cell s1 "A1" 1)
      (set-versioned s1 "A1" t)
      (set-audited s1 "A1" t)
      (set-logged s1 "A2" t :limit 3)
      (set-stats s1 "A2" t)
      (set-cell s1 "A2" '(* 10 (cell "A1")))
      (with-actor ("alice") (set-cell s1 "A1" 2))
      (with-actor ("bob")   (set-cell s1 "A1" 3)))
    (let* ((text (with-output-to-string (o) (write-sheet s1 o)))
           (s2 (with-input-from-string (i text) (read-sheet i))))
      (check (cell-versions s2 "A1") '(1 2 3))              ; formula edits
      (check (cell-log s2 "A2") '(10 20 30))               ; value history
      (let ((st (cell-stats s2 "A2")))
        (check (getf st :count) 3) (check (getf st :sum) 60) (check (getf st :max) 30))
      (let ((audit (cell-audit s2 "A1")))                  ; provenance
        (check (length audit) 2)
        (check (getf (first audit) :actor) "alice" #'string=)
        (check (getf (first audit) :formula) 2)
        (check (getf (first audit) :time) 42))
      (set-cell s2 "A1" 4)                                 ; restored mixin is live
      (check (cell-versions s2 "A1") '(1 2 3 4))))

  ;; symbol-referenced config round-trips: NAMED functions for transform,
  ;; validator, sink, observer, and external source survive and are live.
  (let ((s1 (make-sheet)))
    (set-transform s1 "A2" 'ser-clamp)
    (set-validator s1 "A3" 'ser-even)
    (set-persist s1 "A3" 'ser-sink)
    (observe s1 "A3" 'ser-obs)
    (set-external s1 "A4" 'ser-source)
    (set-cells s1 '(("A1" 150) ("A2" (cell "A1")) ("A3" (cell "A1"))))
    (let* ((text (with-output-to-string (o) (write-sheet s1 o)))
           (s2 (with-input-from-string (i text) (read-sheet i))))
      (check (get-value s2 "A2") 100)                      ; transform restored
      (check (get-value s2 "A3") 150)                      ; validator ok (even)
      (check (get-value s2 "A4") 7)                        ; external source restored
      (setf *ser-sink* '() *ser-obs* '())
      (set-cell s2 "A1" 42)                                ; A3 -> 42 (still even)
      (check *ser-sink* '(42))                             ; sink fired
      (check *ser-obs* '(42))))                            ; observer fired

  ;; an anonymous (lambda) config is NOT serializable — it is skipped, so the
  ;; reloaded cell computes without it (raw value).
  (let ((s1 (make-sheet)))
    (set-transform s1 "A2" (lambda (v) (* v 100)))         ; lambda, not a symbol
    (set-cells s1 '(("A1" 3) ("A2" (cell "A1"))))
    (check (get-value s1 "A2") 300)                        ; live: transformed
    (let* ((text (with-output-to-string (o) (write-sheet s1 o)))
           (s2 (with-input-from-string (i text) (read-sheet i))))
      (check (get-value s2 "A2") 3)))                      ; reloaded: transform lost

  ;; comprehensive file round-trip: formulas + env + forward ref + protected
  ;; cell + history (versions/audit with actors) + named validator, through a
  ;; real file, still live afterward.
  (let ((path (merge-pathnames "cellisp-budget-test.sheet"
                               (uiop:temporary-directory)))
        (s1 (make-sheet :environment '((tax . 1/10)))))
    (set-versioned s1 "A1" t)
    (set-audited s1 "A1" t)
    (set-validator s1 "A2" 'ser-nonneg)
    (set-cells s1 '(("A3" (- (cell "A1") (cell "A2")))    ; forward reference
                    ("A1" 1000) ("A2" 300)
                    ("B1" (* (cell "A3") tax))))
    (set-readonly s1 "A3" t)
    (with-actor ("alice") (set-cell s1 "A1" 1200))
    (save-sheet s1 path)
    (unwind-protect
         (let ((s2 (load-sheet path)))
           (check (get-value s2 "A3") 900)                 ; recomputed
           (check (get-value s2 "B1") 90)                  ; env constant
           (check (cell-versions s2 "A1") '(1000 1200))    ; history restored
           (check (mapcar (lambda (e) (getf e :actor)) (cell-audit s2 "A1"))
                  '(nil "alice"))                          ; provenance restored
           (with-actor ("bob") (set-cell s2 "A1" 1500))    ; live: versioned records
           (check (get-value s2 "A3") 1200)
           (check (cell-versions s2 "A1") '(1000 1200 1500))
           (check-signals readonly-cell (set-cell s2 "A3" 0))   ; readonly restored
           (check-signals invalid-value (set-cell s2 "A2" -5))) ; validator restored
      (when (probe-file path) (delete-file path))))

  ;; a serialized input is a formula, not a stored value: editing the input in
  ;; the saved form and reloading recomputes every dependent cell.
  (let ((s1 (make-sheet :environment '((tax . 1/10)))))
    (set-cells s1 '(("A1" 1000) ("A2" 300)                ; income, expenses
                    ("A3" (- (cell "A1") (cell "A2")))    ; net (derived)
                    ("B1" (* (cell "A3") tax))))          ; tax (derived)
    (check (get-value s1 "A3") 700) (check (get-value s1 "B1") 70)
    (let ((form (sheet->form s1)))
      (dolist (pl (getf (cdr form) :cells))               ; edit income 1000 -> 2500
        (when (equal (getf pl :ref) "A1") (setf (getf pl :formula) 2500)))
      (let ((s2 (form->sheet form)))
        (check (get-value s2 "A1") 2500)
        (check (get-value s2 "A3") 2200)                  ; net recomputed
        (check (get-value s2 "B1") 220))))                ; tax recomputed

  ;; the environment constant is serialized too (it is not a cell): editing it
  ;; in the saved form and reloading reprices every formula that uses it.
  (let ((s1 (make-sheet :environment '((tax . 1/10)))))
    (set-cells s1 '(("A1" 200)                            ; price (a cell)
                    ("B1" (* (cell "A1") tax))            ; tax due (uses constant)
                    ("C1" (+ (cell "A1") (cell "B1")))))  ; total (derived)
    (check (get-value s1 "B1") 20) (check (get-value s1 "C1") 220)
    (let ((form (sheet->form s1)))
      (setf (cdr (first (getf (cdr form) :environment))) 1/5)  ; tax 1/10 -> 1/5
      (let ((s2 (form->sheet form)))
        (check (get-value s2 "A1") 200)                   ; price unchanged
        (check (get-value s2 "B1") 40)                    ; tax repriced
        (check (get-value s2 "C1") 240))))                ; total repriced

  ;; a broken formula in the saved form doesn't abort the load: the bad cell
  ;; and its dependents error, unrelated cells are fine, and fixing the bad
  ;; cell recovers both (the on-error dependency-link commit at work).
  (let ((s1 (make-sheet :environment '((tax . 1/10)))))
    (set-cells s1 '(("A1" 1000) ("A2" 300)
                    ("A3" (- (cell "A1") (cell "A2")))    ; net
                    ("B1" (* (cell "A3") tax))))          ; depends on net
    (let ((form (sheet->form s1)))
      (dolist (pl (getf (cdr form) :cells))               ; break A3: divide by zero
        (when (equal (getf pl :ref) "A3")
          (setf (getf pl :formula) '(/ (cell "A1") 0))))
      (let ((s2 (form->sheet form)))                      ; load must not crash
        (check (get-value s2 "A1") 1000)                  ; unrelated cells fine
        (check (get-value s2 "A2") 300)
        (check (and (nth-value 1 (get-value s2 "A3")) t) t)  ; A3 errored
        (check (and (nth-value 1 (get-value s2 "B1")) t) t)  ; B1 errored (reads A3)
        (set-cell s2 "A3" '(- (cell "A1") (cell "A2")))   ; fix it
        (check (get-value s2 "A3") 700)                   ; recovered
        (check (get-value s2 "B1") 70))))                 ; dependent recovered too

  ;; a cyclic formula in the saved form is caught per-cell on load (cycle
  ;; detection runs in the load sweep), the load succeeds, and breaking the
  ;; cycle recovers the cells.
  (let ((s1 (make-sheet :environment '((tax . 1/10)))))
    (set-cells s1 '(("A1" 1000) ("A2" 300)
                    ("A3" (- (cell "A1") (cell "A2")))
                    ("B1" (* (cell "A3") tax))))
    (let ((form (sheet->form s1)))
      (dolist (pl (getf (cdr form) :cells))               ; A3 now reads B1 (reads A3)
        (when (equal (getf pl :ref) "A3")
          (setf (getf pl :formula) '(+ (cell "B1") 1))))
      (let ((s2 (form->sheet form)))                      ; must not crash
        (check (get-value s2 "A1") 1000)                  ; unrelated cells fine
        (check (and (typep (nth-value 1 (get-value s2 "A3")) 'cyclic-reference) t) t)
        (check (and (typep (nth-value 1 (get-value s2 "B1")) 'cyclic-reference) t) t)
        (set-cell s2 "A3" '(- (cell "A1") (cell "A2")))   ; break the cycle
        (check (get-value s2 "A3") 700)                   ; recovered
        (check (get-value s2 "B1") 70))))                 ; dependent recovered

  ;; a formula referencing a cell not in the sheet loads with an UNBOUND-CELL
  ;; error per-cell; SUPPLYING the missing cell (not editing the formula)
  ;; recovers it, via the committed link to the empty precedent.
  (let ((s1 (make-sheet :environment '((tax . 1/10)))))
    (set-cells s1 '(("A1" 1000) ("A2" 300)
                    ("A3" (- (cell "A1") (cell "A2")))
                    ("B1" (* (cell "A3") tax))))
    (let ((form (sheet->form s1)))
      (dolist (pl (getf (cdr form) :cells))               ; A3 references missing Z9
        (when (equal (getf pl :ref) "A3")
          (setf (getf pl :formula) '(+ (cell "A1") (cell "Z9")))))
      (let ((s2 (form->sheet form)))
        (check (get-value s2 "A1") 1000)                  ; unrelated cells fine
        (check (and (typep (nth-value 1 (get-value s2 "A3")) 'unbound-cell) t) t)
        (check (and (typep (nth-value 1 (get-value s2 "B1")) 'unbound-cell) t) t)
        (set-cell s2 "Z9" 500)                            ; supply the missing cell
        (check (get-value s2 "A3") 1500)                  ; recovered (1000 + 500)
        (check (get-value s2 "B1") 150))))                ; dependent recovered

  ;; save-sheet / load-sheet round-trip through an actual file
  (let ((path (merge-pathnames "cellisp-roundtrip-test.sheet"
                               (uiop:temporary-directory)))
        (s1 (make-sheet)))
    (set-cells s1 '(("A1" 5) ("A2" (* (cell "A1") 3))))
    (save-sheet s1 path)
    (unwind-protect
         (let ((s2 (load-sheet path)))
           (check (get-value s2 "A2") 15))
      (when (probe-file path) (delete-file path))))

  ;; a non-sheet form is rejected
  (check-signals sheet-error (form->sheet '(:not-a-sheet)))

  (format t "~&~D checks, ~D failures.~%" *count* *fails*)
  (when (plusp *fails*) (error "Test failures: ~D" *fails*))
  t)
