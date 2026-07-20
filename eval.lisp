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
(defvar *collected-foreign* nil
  "When bound (a hash-table) during COMPUTE-CELL of a cell in a workbook sheet,
holds the cross-sheet grefs (sheet . ref) the running formula reads. NIL for a
standalone sheet, so single-sheet evaluation allocates nothing extra.")
(defvar *deferred* nil
  "Inside WITH-TRANSACTION this is a hash-table collecting the seed refs of every
edit, so recomputation is deferred until the transaction commits (one sweep) and
per-edit undo is replaced by one combined entry. NIL outside a transaction.")
(defvar *sticky* nil
  "When bound (a hash-table) for the whole span of ONE edit — across every
per-sheet sweep of a cross-sheet cascade — COMPUTE-CELL records the global cell
handles (sheet . ref) whose value/error changed at ANY point. A cascade can sweep
one sheet several times (once per producer that feeds it); a change made in an
early sweep (or by an on-demand pull) would be invisible to the per-sweep
*CHANGED* of a later sweep, so RECOMPUTE-LOCAL also consults *STICKY* to decide
whether a cell's precedent changed. NIL outside a workbook cascade.")
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

(defun note-foreign (target ref)
  "Record a cross-sheet read of TARGET's REF by the formula being computed."
  (when *collected-foreign*
    (setf (gethash (cons target ref) *collected-foreign*) t)))

(defun split-sheet-designator (designator)
  "For a string/symbol like \"Data!A1\", return (values \"Data\" \"A1\"). With no
'!' separator — or for a ref cons — return (values NIL designator)."
  (if (typep designator '(or string symbol))
      (let* ((s (string designator))
             (bang (position #\! s)))
        (if bang
            (values (subseq s 0 bang) (subseq s (1+ bang)))
            (values nil designator)))
      (values nil designator)))

(defun require-sheet (name)
  "The sheet named NAME in *SHEET*'s workbook, or a SHEET-ERROR if there is no
workbook or no such sheet."
  (let ((wb (and *sheet* (sheet-workbook *sheet*))))
    (or (and wb (find-sheet wb name))
        (error 'sheet-error
               :format-control "No sheet named ~S in the workbook"
               :format-arguments (list name)))))

(defun read-foreign (target ref)
  "Read TARGET's current cached value of REF without recomputing it — cross-sheet
reads see the producer's settled value, and the workbook cascade keeps producers
current. An empty cell signals UNBOUND-CELL; a stored error re-signals."
  (let ((cell (find-cell target ref)))
    (cond ((null cell) (error 'unbound-cell :ref ref))
          ((cell-err cell) (error (cell-err cell)))
          (t (cell-value cell)))))

(defun read-cell-value (target ref)
  "Read REF from TARGET, recording the dependency. When TARGET is the sheet being
evaluated this is an ordinary on-demand pull; otherwise it is a cross-sheet read
(non-recursive, recorded as a foreign precedent)."
  (if (eq target *sheet*)
      (progn (note-precedent ref) (evaluate-ref *sheet* ref))
      (progn (note-foreign target ref) (read-foreign target ref))))

(defun read-cell-blank (target ref)
  "Like READ-CELL-VALUE but an EMPTY cell reads as NIL (blank) rather than
signaling UNBOUND-CELL — so a range read tolerates gaps, spreadsheet-style. An
*existing* cell that holds an error still propagates it (use SAFE-CELLS to
tolerate errors too). The dependency on REF is recorded either way, so filling a
blank later re-fires the reader."
  (if (eq target *sheet*)
      (progn (note-precedent ref)
             (if (find-cell target ref) (evaluate-ref target ref) nil))
      (progn (note-foreign target ref)
             (if (find-cell target ref) (read-foreign target ref) nil))))

(defun %lookup-name (designator)
  "The value DESIGNATOR is bound to in *SHEET*'s names table (a ref or a range
cons), or NIL. Skipped entirely when the sheet has no names — keeps the hot path
fast — and only strings/symbols can name anything."
  (and *sheet*
       (plusp (hash-table-count (sheet-names *sheet*)))
       (typep designator '(or string symbol))
       (gethash (%name-key designator) (sheet-names *sheet*))))

(defun resolve-ref (designator)
  "Resolve DESIGNATOR to a single ref: a registered name on *SHEET* takes
precedence, otherwise it is parsed as an A1 reference. A range name resolves to
its top-left corner, so (cell RANGE) reads the block's first cell."
  (let ((named (%lookup-name designator)))
    (cond ((null named) (parse-ref designator))
          ((%range-value-p named) (car named)) ; range name -> its top-left ref
          (t named))))                          ; single-cell name -> its ref

(defun cell (designator)
  "Read another cell's value. Records the dependency and forces the referenced
cell up to date (depth-first, with cycle detection). DESIGNATOR may be a
registered name or A1 ref, and — inside a workbook — may be qualified with a
sheet name, as in \"Data!A1\", to read another sheet."
  (multiple-value-bind (sheet-name local) (split-sheet-designator designator)
    (if sheet-name
        (let ((target (require-sheet sheet-name)))
          ;; resolve names/refs in the TARGET sheet's namespace
          (read-cell-value target (let ((*sheet* target)) (resolve-ref local))))
        (read-cell-value *sheet* (resolve-ref designator)))))

(defun range-corners (top-left bottom-right)
  "Two values, the top-left and bottom-right refs CELLS should span. With
BOTTOM-RIGHT supplied, resolve each corner. With only TOP-LEFT, it may be a
range name (expands to its two stored corners) or a single cell (a 1x1 range)."
  (if bottom-right
      (values (resolve-ref top-left) (resolve-ref bottom-right))
      (let ((named (%lookup-name top-left)))
        (if (and named (%range-value-p named))
            (values (car named) (cdr named))
            (let ((r (resolve-ref top-left))) (values r r))))))

(defun resolve-range (top-left bottom-right)
  "Resolve a CELLS/GRID range spec to (values target-sheet r0 r1 c0 c1): the sheet
to read and the inclusive row/column bounds. Handles a sheet qualifier
(\"Data!A1\"), a range name, and the one-corner (1x1 or named-range) forms."
  (multiple-value-bind (sheet-name tl) (split-sheet-designator top-left)
    (let* ((target (if sheet-name (require-sheet sheet-name) *sheet*))
           ;; a qualified bottom-right's sheet part is redundant; use its local
           (br (if (and bottom-right sheet-name)
                   (nth-value 1 (split-sheet-designator bottom-right))
                   bottom-right)))
      (multiple-value-bind (a b)
          ;; resolve corners (and any range name) in the TARGET sheet's namespace
          (let ((*sheet* target)) (range-corners (if sheet-name tl top-left) br))
        (values target
                (min (ref-row a) (ref-row b)) (max (ref-row a) (ref-row b))
                (min (ref-col a) (ref-col b)) (max (ref-col a) (ref-col b)))))))

(defun cells (top-left &optional bottom-right)
  "Return a list of the values in a rectangle, row-major. Two corner designators
span it explicitly; a single argument may be a range name (or a single cell).
Either form may be sheet-qualified (\"Data!A1\" \"Data!A10\") to read across a
workbook; the rectangle is taken from that one target sheet.

Empty cells in the rectangle read as NIL (blank), so a range with gaps is fine
and the numeric aggregates (which ignore non-numbers) sum/average just the values
present. An existing cell that holds an error still propagates it; use SAFE-CELLS
to skip errored cells too. (A single-cell read via CELL stays strict — reading an
empty cell there signals, preserving error propagation.)"
  (multiple-value-bind (target r0 r1 c0 c1) (resolve-range top-left bottom-right)
    (let ((out '()))
      (loop for r from r0 to r1 do
        (loop for c from c0 to c1
              for ref = (make-ref r c)
              do (push (read-cell-blank target ref) out)))
      (nreverse out))))

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
        ;; only a workbook sheet can read across sheets — allocate the foreign
        ;; table only then, so single-sheet compute stays allocation-clean.
        (*collected-foreign* (and (sheet-workbook sheet)
                                  (make-hash-table :test 'equal)))
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
      ;; commit cross-sheet links too, when this sheet is in a workbook (or had
      ;; foreign links to tear down after leaving one).
      (when (or *collected-foreign* (cell-foreign-precedents cell))
        (update-foreign-links sheet ref cell *collected-foreign*))
      ;; mark computed-this-sweep (success or stored error) so readers and
      ;; the recompute loop reuse the result instead of recomputing.
      (when *fresh* (setf (gethash ref *fresh*) t))
      ;; record whether the output changed, for propagation short-circuiting.
      (when (not (and (equal old-value (cell-value cell))
                      (eq old-err-p (and (cell-err cell) t))))
        (when *changed* (setf (gethash ref *changed*) t))
        ;; also record it stickily (global handle) so a later sweep of this sheet
        ;; in the same cross-sheet cascade still sees the change (see *STICKY*).
        (when *sticky* (setf (gethash (cons sheet ref) *sticky*) t))))
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

;;; --- cross-sheet dependency links -----------------------------------
;;;
;;; A gref (global ref) is a (sheet . ref) cons naming a cell in a specific
;;; sheet. A consumer cell's FOREIGN-PRECEDENTS list holds the grefs it reads
;;; elsewhere; each producer sheet's FOREIGN-DEPENDENTS table maps a local ref to
;;; the consumer grefs that read it. The two are kept in sync so the cascade can,
;;; from a changed producer cell, find exactly the foreign consumers to refresh.

(defun gref= (a b)
  "Equality on grefs: same sheet (identity) and same ref."
  (and (eq (car a) (car b)) (equal (cdr a) (cdr b))))

(defun %add-foreign-dependent (producer-sheet producer-ref consumer-gref)
  (pushnew consumer-gref
           (gethash producer-ref (sheet-foreign-dependents producer-sheet))
           :test #'gref=))

(defun %remove-foreign-dependent (producer-sheet producer-ref consumer-gref)
  (let* ((table (sheet-foreign-dependents producer-sheet))
         (rest (remove consumer-gref (gethash producer-ref table) :test #'gref=)))
    (if rest
        (setf (gethash producer-ref table) rest)
        (remhash producer-ref table))))

(defun update-foreign-links (sheet ref cell new-foreign-table)
  "Reconcile CELL's cross-sheet precedents with NEW-FOREIGN-TABLE (grefs it read
this compute, or NIL for none): drop stale producer back-links, add current ones."
  (let ((self (cons sheet ref))
        (new-grefs (and new-foreign-table
                        (loop for g being the hash-keys of new-foreign-table
                              collect g))))
    (dolist (old (cell-foreign-precedents cell))
      (unless (and new-foreign-table (gethash old new-foreign-table))
        (%remove-foreign-dependent (car old) (cdr old) self)))
    (dolist (g new-grefs)
      (%add-foreign-dependent (car g) (cdr g) self))
    (setf (cell-foreign-precedents cell) new-grefs)))

(defun detach-foreign (sheet ref cell)
  "Drop all of CELL's cross-sheet producer back-links (used when clearing it)."
  (let ((self (cons sheet ref)))
    (dolist (old (cell-foreign-precedents cell))
      (%remove-foreign-dependent (car old) (cdr old) self))
    (setf (cell-foreign-precedents cell) '())))
