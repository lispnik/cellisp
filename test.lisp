(defpackage #:cellisp/test
  (:use #:cl #:cellisp)
  (:export #:run-tests))
(in-package #:cellisp/test)

(defvar *fails* 0)
(defvar *count* 0)
(defvar *evals* 0)   ; counts formula-body evaluations, for the dedup test
(defvar *vcount* 0)  ; volatile-cell recompute counter
(defvar *pcount* 0)  ; plain-cell recompute counter (contrast)

;; test-only mixins for the COMBINED-CLASS multi-mixin test (top level so
;; their types are known when RUN-TESTS is compiled)
(defclass demo-mixin-a () ((xa :initform :a)))
(defclass demo-mixin-b () ((xb :initform :b)))

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
      (check (typep cell 'observable-mixin) t)             ; observation added
      (check (typep cell 'external-cell) t))               ; source preserved
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
      (check (typep cell 'external-cell) t)
      (check (typep cell 'observable-mixin) t)
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
    (check (typep (cellisp::find-cell s (parse-ref "A2")) 'observable-mixin) t)
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
      (check (typep inst 'demo-mixin-a) t)
      (check (typep inst 'demo-mixin-b) t)
      (check (typep inst 'observable-mixin) t)
      (check (typep inst 'cell) t)))

  ;; a value-source change PRESERVES existing mixins: observe a plain cell,
  ;; then make it external — it stays observable and becomes external.
  (let ((s (make-sheet)) (feed 3) (log '()))
    (set-cell s "A1" 0)
    (observe s "A1" (lambda (v) (push v log)))            ; observable + cell
    (set-external s "A1" (lambda () feed))                ; -> observable + external
    (let ((cell (cellisp::find-cell s (parse-ref "A1"))))
      (check (typep cell 'observable-mixin) t)            ; mixin kept
      (check (typep cell 'external-cell) t))              ; source changed
    (recalc s "A1")
    (check (get-value s "A1") 3)
    (check (first log) 3))                                 ; still notifies

  ;; unobserving the last subscriber drops OBSERVABLE-MIXIN (via REMOVE-MIXIN),
  ;; leaving the value source intact.
  (let ((s (make-sheet)) (cb (lambda (v) (declare (ignore v)))))
    (set-external s "A1" (lambda () 9))
    (observe s "A1" cb)
    (check (typep (cellisp::find-cell s (parse-ref "A1")) 'observable-mixin) t)
    (unobserve s "A1" cb)
    (let ((cell (cellisp::find-cell s (parse-ref "A1"))))
      (check (typep cell 'observable-mixin) nil)          ; mixin removed
      (check (typep cell 'external-cell) t))              ; source preserved
    (check (get-value s "A1") 9))

  ;; readonly-mixin: locks user reassignment, but the cell still recomputes
  ;; from its precedents. SET-READONLY toggles it.
  (let ((s (make-sheet)))
    (set-cell s "A1" 5)
    (set-cell s "A2" '(* 2 (cell "A1")))
    (set-readonly s "A2" t)
    (check (typep (cellisp::find-cell s (parse-ref "A2")) 'readonly-mixin) t)
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
      (check (typep cell 'observable-mixin) t)
      (check (typep cell 'readonly-mixin) t))             ; both present
    (check-signals readonly-cell (set-cell s "A2" 5))     ; readonly guards
    (set-cell s "A1" 3)                                    ; recompute -> 30
    (check (get-value s "A2") 30)
    (check (first log) 30))                                ; observer still fires

  ;; readonly also blocks changing a cell's value source
  (let ((s (make-sheet)))
    (set-cell s "A1" 1)
    (set-readonly s "A1" t)
    (check-signals readonly-cell (set-external s "A1" (lambda () 9))))

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

  (format t "~&~D checks, ~D failures.~%" *count* *fails*)
  (when (plusp *fails*) (error "Test failures: ~D" *fails*))
  t)
