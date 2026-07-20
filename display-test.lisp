;;;; Tests for the cellisp/display rendering layer. Self-contained harness
;;;; mirroring test.lisp: CHECK / RUN-TESTS print "N checks, M failures." and
;;;; RUN-TESTS signals an error if any check fails (so ASDF test-op fails).

(defpackage #:cellisp/display-test
  (:use #:cl #:cellisp #:cellisp/display)
  (:export #:run-tests))

(in-package #:cellisp/display-test)

(defvar *count* 0)
(defvar *fails* 0)

(defmacro check (form expected &optional (test '#'equal))
  (let ((got (gensym)) (exp (gensym)))
    `(progn
       (incf *count*)
       (let ((,got ,form) (,exp ,expected))
         (unless (funcall ,test ,got ,exp)
           (incf *fails*)
           (format t "~&FAIL: ~S~%  got:      ~S~%  expected: ~S~%"
                   ',form ,got ,exp))))))

(defun run-tests ()
  (setf *count* 0 *fails* 0)

  ;; --- error-token on directly constructed conditions (deterministic) ------
  (check (error-token (make-condition 'cyclic-reference
                                      :cells (list (parse-ref "A1")))) "#CYCLE!")
  (check (error-token (make-condition 'unbound-cell :ref (parse-ref "A1"))) "#REF!")
  (check (error-token (make-condition 'invalid-value :ref (parse-ref "A1")
                                                     :value -1)) "#VALUE!")
  (check (error-token (make-condition 'cell-eval-error :ref (parse-ref "A1")
                        :original (make-condition 'division-by-zero
                                                  :operation '/ :operands '(1 0))))
         "#DIV/0!")
  (check (error-token (make-condition 'cell-eval-error :ref (parse-ref "A1")
                        :original (make-condition 'type-error :datum "x"
                                                  :expected-type 'number)))
         "#VALUE!")
  ;; a dangling ref from a structural delete surfaces as a base sheet-error
  ;; whose report carries the literal "#REF!"
  (check (error-token (make-condition 'sheet-error
                        :format-control "Malformed reference ~S"
                        :format-arguments (list "#REF!"))) "#REF!")
  (check (error-token (make-condition 'sheet-error
                        :format-control "No sheet named ~S in the workbook"
                        :format-arguments (list "Nope"))) "#NAME?")
  (check (error-token (make-condition 'sheet-error
                        :format-control "Malformed reference ~S"
                        :format-arguments (list "notaname"))) "#NAME?")
  ;; a reference shifted off the grid by a structural delete
  (check (error-token (make-condition 'sheet-error
                        :format-control "Row must be >= 1 in ~S"
                        :format-arguments (list "A0"))) "#REF!")
  (check (error-token (make-condition 'sheet-error
                        :format-control "Bad column letter ~S"
                        :format-arguments (list #\#))) "#REF!")
  (check (error-token (make-condition 'sheet-error
                        :format-control "some other problem")) "#ERR!")

  ;; --- display-value end to end -------------------------------------------

  ;; a plain value, and an empty cell
  (let ((s (make-sheet)))
    (set-cell s "A1" 42)
    (check (display-value s "A1") "42")
    (check (display-value s "B9") ""))           ; empty -> blank

  ;; division by zero -> #DIV/0!
  (let ((s (make-sheet)))
    (set-cell s "A1" 0)
    (ignore-errors (set-cell s "A2" '(/ 1 (cell "A1"))))
    (check (display-value s "A2") "#DIV/0!"))

  ;; a type error -> #VALUE! (read a string cell so the constant isn't visible
  ;; to the compiler, which would otherwise warn at build time)
  (let ((s (make-sheet)))
    (set-cell s "A1" "x")
    (ignore-errors (set-cell s "A2" '(+ 1 (cell "A1"))))
    (check (display-value s "A2") "#VALUE!"))

  ;; a reference cycle -> #CYCLE!
  (let ((s (make-sheet)))
    (ignore-errors (set-cell s "A1" '(cell "A2")))
    (ignore-errors (set-cell s "A2" '(cell "A1")))
    (check (display-value s "A1") "#CYCLE!"))

  ;; a formula that reads an empty cell -> #REF!
  (let ((s (make-sheet)))
    (ignore-errors (set-cell s "A1" '(cell "Z9")))
    (check (display-value s "A1") "#REF!"))

  ;; a dangling reference left by a structural delete -> #REF!
  (let ((s (make-sheet)))
    (set-cell s "A1" 10)
    (set-cell s "A2" '(+ (cell "A1") 1))
    (delete-row s 0)                             ; A1 deleted; A2 -> A1, ref dangles
    (check (display-value s "A1") "#REF!"))

  ;; an unknown sheet in a workbook -> #NAME?
  (let* ((wb (make-workbook)) (s (add-sheet wb "Only")))
    (ignore-errors (set-cell s "A1" '(cell "Nope!A1")))
    (check (display-value s "A1") "#NAME?"))

  ;; a validator rejection surfaces (whatever the engine stores) as a token
  (let ((s (make-sheet)))
    (set-cell s "A1" 5)
    (set-validator s "A1" #'plusp)
    (ignore-errors (set-cell s "A1" -3))
    ;; either the cell now holds an invalid-value error (-> #VALUE!) or the old
    ;; valid value was kept (-> "5"); both are acceptable, neither should crash.
    (check (member (display-value s "A1") '("#VALUE!" "5") :test #'string=)
           t (lambda (a b) (eq (and a t) b))))

  ;; --- format-value: value kinds and specs --------------------------------
  (check (format-value 42) "42")
  (check (format-value 42 '(:fixed 2)) "42.00")
  (check (format-value 1/4) "0.25")               ; ratio -> natural decimal
  (check (format-value 1/2) "0.5")
  (check (format-value 3.0) "3")                  ; trailing .0 trimmed
  (check (format-value 3.5) "3.5")
  (check (format-value 1/4 '(:percent 0)) "25%")
  (check (format-value 1/8 '(:percent 1)) "12.5%")
  (check (format-value 5 '(:currency "$" 2)) "$5.00")
  (check (format-value 5 :integer) "5")
  (check (format-value 27/10 :integer) "3")       ; 2.7 rounds to 3
  (check (format-value "hello") "hello")
  (check (format-value nil) "")
  (check (format-value '(10 25 30)) "10, 25, 30") ; a list (from CELLS)
  (check (format-value 42 (lambda (v) (* v 2))) "84")  ; function spec

  ;; --- format registry: precedence cell > column > general ----------------
  (let ((f (make-formats)))
    (set-column-format f "B" '(:fixed 1))         ; column B default
    (set-format f "B2" '(:fixed 3))               ; cell B2 override
    (check (format-for f "B5") '(:fixed 1))       ; column default applies
    (check (format-for f "B2") '(:fixed 3))       ; cell wins over column
    (check (format-for f "C1") :general))         ; nothing set -> general

  ;; display-value honoring a registry
  (let ((s (make-sheet)) (f (make-formats)))
    (set-cell s "B1" 1/2)
    (set-column-format f "B" '(:percent 0))
    (check (display-value s "B1" :formats f) "50%")
    (check (display-value s "B1") "0.5"))         ; no registry -> general

  ;; --- conditional formatting ---------------------------------------

  ;; a rule overrides the static format for matching values; first match wins
  (let ((s (make-sheet)) (f (make-formats)))
    (set-cell s "A1" -5)
    (set-cell s "A2" 5)
    (set-format f "A1" '(:fixed 2))               ; static format for A1
    (set-format f "A2" '(:fixed 2))
    ;; negatives render via a function spec (parenthesized); overrides :fixed
    (add-conditional f #'minusp (lambda (v) (format nil "(~A)" (abs v))))
    (check (display-value s "A1" :formats f) "(5)")   ; rule matched, overrode
    (check (display-value s "A2" :formats f) "5.00")) ; no match -> static :fixed

  ;; a column-scoped rule only fires in its column
  (let ((s (make-sheet)) (f (make-formats)))
    (set-cell s "B1" 0) (set-cell s "C1" 0)
    (add-conditional f #'zerop "—" :column "B")   ; blank-out zeros, column B only
    (check (display-value s "B1" :formats f) "—")
    (check (display-value s "C1" :formats f) "0"))  ; column C untouched

  ;; a rule that errors on a value simply doesn't match (defensive)
  (let ((s (make-sheet)) (f (make-formats)))
    (set-cell s "A1" "text")
    (add-conditional f #'plusp '(:fixed 1))       ; plusp of a string errors -> skip
    (check (display-value s "A1" :formats f) "text"))

  (format t "~&~D checks, ~D failures.~%" *count* *fails*)
  (when (plusp *fails*) (error "Display test failures: ~D" *fails*))
  t)
