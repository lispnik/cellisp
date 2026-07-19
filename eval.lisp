(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Formula language
;;;;
;;;; A formula is an arbitrary Common Lisp form. Inside a formula, other
;;;; cells are read with the operators CELL and CELLS:
;;;;
;;;;   (cell "A1")            -> value of A1
;;;;   (cell 'a1)            -> same (symbol designator)
;;;;   (cells "A1" "B3")     -> list of values over the rectangle A1:B3
;;;;
;;;; SUM, AVERAGE and CNT are convenience aggregates that accept either
;;;; loose values/lists or a range expressed as two corner designators.
;;;;
;;;; A literal (non-cons that is not a symbol bound in the env, or a
;;;; quoted/self-evaluating object) is stored and returned as-is.
;;;; ------------------------------------------------------------------

(defvar *sheet* nil
  "The sheet currently being evaluated; bound during RECALC.")
(defvar *eval-stack* '()
  "Refs currently being evaluated, innermost first; for cycle detection.")
(defvar *collected-precedents* nil
  "When non-nil (a hash-table), CELL/CELLS record the refs they touch.")
(defvar *fresh* nil
  "When bound (a hash-table) for the duration of one recompute sweep, holds
the refs already computed this sweep so each cell is computed at most once.")
(defvar *changed* nil
  "When bound (a hash-table) for a sweep, COMPUTE-CELL records refs whose
value/error actually changed — RECOMPUTE-CLOSURE uses this to short-circuit
recompute of subtrees whose inputs did not change.")
(defvar *actor* nil
  "Identity of whoever is making the current mutation, recorded by
AUDITED-MIXIN. Bind it with WITH-ACTOR.")
(defvar *audit-clock* #'get-universal-time
  "Thunk returning the timestamp stamped on audit/version entries. Rebind for
deterministic tests.")

;;; --- reference reading primitives, callable from formulas -----------

(defun note-precedent (ref)
  (when *collected-precedents*
    (setf (gethash ref *collected-precedents*) t)))

(defun cell (designator)
  "Read another cell's value. Records the dependency and forces the
referenced cell to be up to date (depth-first, with cycle detection)."
  (let ((ref (parse-ref designator)))
    (note-precedent ref)
    (evaluate-ref *sheet* ref)))

(defun cells (top-left bottom-right)
  "Return a list of the values in the rectangle spanned by the two
corner designators, row-major."
  (let* ((a (parse-ref top-left))
         (b (parse-ref bottom-right))
         (r0 (min (ref-row a) (ref-row b)))
         (r1 (max (ref-row a) (ref-row b)))
         (c0 (min (ref-col a) (ref-col b)))
         (c1 (max (ref-col a) (ref-col b)))
         (out '()))
    (loop for r from r0 to r1 do
      (loop for c from c0 to c1
            for ref = (make-ref r c)
            do (note-precedent ref)
               (push (evaluate-ref *sheet* ref) out)))
    (nreverse out)))

;;; --- aggregates -----------------------------------------------------

(defun flatten-numbers (args)
  "Collect every number in ARGS, descending into nested lists. Non-numeric
values (strings, symbols, NIL) are dropped, so the aggregates below all
operate on numbers only, spreadsheet-style."
  (loop for a in args
        append (cond ((null a) '())
                     ((listp a) (flatten-numbers a))
                     ((numberp a) (list a))
                     (t '()))))

(defun sum (&rest args)
  "Sum of every number in ARGS (non-numbers ignored). Empty -> 0."
  (reduce #'+ (flatten-numbers args) :initial-value 0))
(defun cnt (&rest args)
  "Count of the numeric values in ARGS (non-numbers ignored)."
  (length (flatten-numbers args)))
(defun average (&rest args)
  "Mean of the numbers in ARGS (non-numbers ignored). Signals SHEET-ERROR
when there are no numbers, rather than dividing by zero (cf. #DIV/0)."
  (let ((ns (flatten-numbers args)))
    (if ns
        (/ (reduce #'+ ns) (length ns))
        (error 'sheet-error
               :format-control "AVERAGE of no numeric values"))))

;;; --- dependency extraction ------------------------------------------
;;;
;;; We discover a formula's precedents by evaluating it once with
;;; *collected-precedents* active. This is dynamic (handles conditional
;;; refs correctly for the current state) and re-run on every recalc.

(defun environment-bindings (sheet)
  "Build a let-list from the sheet environment alist. Values are QUOTEd so
they are treated as data, not spliced in as code to evaluate (a list or
symbol value would otherwise be run as a form)."
  (loop for (name . val) in (sheet-environment sheet)
        collect (list name (list 'quote val))))

(defun cell-thunk (cell sheet formula)
  "Return a compiled thunk for FORMULA under SHEET's environment, cached on
CELL. Recompiles only when the formula changes; the environment is assumed
fixed for the life of the sheet, so formula identity (EQ) is a sufficient
cache key. Mutating SHEET-ENVIRONMENT after a formula has been evaluated
leaves cached thunks holding the old constants — don't."
  (unless (eq (cell-compiled-from cell) formula)
    (let ((bindings (environment-bindings sheet)))
      (setf (cell-compiled cell)
            (compile nil `(lambda () (let ,bindings
                                       (declare (ignorable
                                                 ,@(mapcar #'car bindings)))
                                       ,formula)))
            (cell-compiled-from cell) formula)))
  (cell-compiled cell))

(defun eval-formula (sheet cell)
  "Evaluate CELL's formula as arbitrary Lisp (NOT sandboxed). Literals
self-evaluate."
  (let ((formula (cell-formula cell)))
    (if (atom formula)
        ;; literal: number, string, symbol-as-data... return as-is unless
        ;; it is a symbol bound in the environment.
        (let ((binding (and (symbolp formula)
                            (assoc formula (sheet-environment sheet)))))
          (if binding (cdr binding) formula))
        ;; a cons: a Lisp form. With an environment we evaluate a compiled,
        ;; cached thunk (bindings in scope); otherwise plain EVAL.
        (if (sheet-environment sheet)
            (funcall (cell-thunk cell sheet formula))
            (eval formula)))))

;;; --- per-class extension points -------------------------------------
;;;
;;; COMPUTE-VALUE is the seam by which a cell subclass produces its value:
;;; the base cell evaluates its formula, but external/async cells override
;;; it. Reads of other cells inside a method go through CELL/CELLS, so
;;; precedent tracking works regardless of how the value is produced.
;;;
;;; CELL-SWEPT is called once per cell that was (re)computed in a sweep,
;;; after the sweep's compute loop finishes — the hook observed cells use to
;;; fire change notifications on settled values. Both default to inert.

(defgeneric compute-value (cell sheet ref)
  (:documentation "Produce and return CELL's current value.")
  (:method ((cell cell) sheet ref)
    (declare (ignore ref))
    (eval-formula sheet cell)))

(defgeneric cell-swept (cell sheet ref)
  (:documentation "Called after a sweep for each cell computed in it.")
  (:method ((cell cell) sheet ref)
    (declare (ignore sheet ref))
    nil))

(defgeneric cell-writable-p (cell &optional new-formula)
  (:documentation "True if the user API may reassign CELL, optionally to
NEW-FORMULA (nil for a clear or a source change). A guard on the public
mutators; internal recomputation ignores it.")
  (:method ((cell cell) &optional new-formula)
    (declare (ignore new-formula))
    t))

(defgeneric note-set (cell sheet ref new-formula actor time)
  (:documentation "Called on the public set path just before CELL's formula is
replaced with NEW-FORMULA, carrying the ACTOR (*actor*) and TIME (*audit-clock*)
of the change — a hook for edit history / auditing. Inert base.")
  (:method ((cell cell) sheet ref new-formula actor time)
    (declare (ignore sheet ref new-formula actor time))
    nil))

;;; --- the recalculation core -----------------------------------------

(defun evaluate-ref (sheet ref)
  "Return the current value of REF, computing it (and its precedents)
if needed. Signals CYCLIC-REFERENCE on a cycle, UNBOUND-CELL for empty
cells read by a formula."
  (let ((cell (find-cell sheet ref)))
    (cond
      ((null cell)
       (error 'unbound-cell :ref ref))
      ;; already computed this sweep: reuse its result (re-signal a stored
      ;; error) instead of recomputing. Checked before the cycle guard: a
      ;; cell mid-computation is not yet fresh, so a re-entrant read still
      ;; falls through to the *eval-stack* cycle check below.
      ((and *fresh* (gethash ref *fresh*))
       (if (cell-err cell) (error (cell-err cell)) (cell-value cell)))
      ;; cycle: this ref is already on the active evaluation stack.
      ((member ref *eval-stack* :test 'equal)
       (error 'cyclic-reference
              :cells (reverse (cons ref (ldiff *eval-stack*
                                               (member ref *eval-stack*
                                                       :test 'equal))))))
      (t
       (compute-cell sheet ref cell)))))

(defun compute-cell (sheet ref cell)
  "(Re)compute CELL, refreshing its value/error and precedent set."
  ;; A frozen cell is held at its current value: skip recomputation entirely,
  ;; leaving its cached value and dependency links untouched.
  (when (gethash ref (sheet-frozen sheet))
    (return-from compute-cell (cell-value cell)))
  (let ((*eval-stack* (cons ref *eval-stack*))
        (*collected-precedents* (make-hash-table :test 'equal))
        (old-value (cell-value cell))
        (old-err-p (and (cell-err cell) t)))
    ;; Commit the freshly observed precedents and back-links even when the
    ;; formula errors: the refs it touched before failing are already in
    ;; *collected-precedents*, and a dependent that dropped out of the
    ;; graph on error would never be revisited when its inputs recover.
    (unwind-protect
         (handler-case
             (let ((val (compute-value cell sheet ref)))
               (setf (cell-value cell) val
                     (cell-err cell) nil))
           (sheet-error (e)
             (setf (cell-err cell) e (cell-value cell) nil)
             (error e))
           (error (e)
             (let ((wrapped (make-condition 'cell-eval-error :ref ref :original e)))
               (setf (cell-err cell) wrapped (cell-value cell) nil)
               (error wrapped))))
      (update-dependency-links sheet ref cell *collected-precedents*)
      ;; mark computed-this-sweep (success or stored error) so readers and
      ;; the recompute loop reuse the result instead of recomputing.
      (when *fresh* (setf (gethash ref *fresh*) t))
      ;; record whether the output changed, for propagation short-circuiting.
      (when (and *changed*
                 (not (and (equal old-value (cell-value cell))
                           (eq old-err-p (and (cell-err cell) t)))))
        (setf (gethash ref *changed*) t)))
    (cell-value cell)))

(defun update-dependency-links (sheet ref cell new-precedents-table)
  (let ((new-precs (loop for p being the hash-keys of new-precedents-table
                         collect p)))
    ;; remove this ref from the dependents list of any dropped precedent.
    (dolist (old (cell-precedents cell))
      (unless (gethash old new-precedents-table)
        (let ((c (find-cell sheet old)))
          (when c (setf (cell-dependents c)
                        (remove ref (cell-dependents c) :test 'equal))))))
    ;; add this ref to the dependents of each current precedent.
    (dolist (p new-precs)
      (let ((c (ensure-cell sheet p)))
        (pushnew ref (cell-dependents c) :test 'equal)))
    (setf (cell-precedents cell) new-precs)))
