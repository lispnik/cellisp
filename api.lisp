(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Public API
;;;; ------------------------------------------------------------------

(defmacro with-actor ((actor) &body body)
  "Run BODY with ACTOR recorded as the author of any mutations (for
AUDITED-MIXIN / edit provenance)."
  `(let ((*actor* ,actor)) ,@body))

;;; --- undo / redo of formula edits -----------------------------------

(defvar *recording* t
  "When true, SET-CELL/SET-CELLS/CLEAR-CELL log an undo entry. Bound NIL while
UNDO/REDO replay, so restoring doesn't itself record.")

(defun capture-cells (sheet refs)
  "Snapshot REFS as an alist (ref . formula-or-:absent)."
  (loop for ref in refs
        for cell = (find-cell sheet ref)
        collect (cons ref (if cell (cell-formula cell) :absent))))

(defun push-undo (sheet snapshot)
  "Record SNAPSHOT as the next undo step and clear the redo stack (when
recording)."
  (when *recording*
    (push snapshot (sheet-undo-stack sheet))
    (setf (sheet-redo-stack sheet) '())))

(defun restore-cells (sheet snapshot)
  "Restore the formulas captured by CAPTURE-CELLS (absent cells are cleared).
Runs with recording off, and batches the sets into one recompute sweep."
  (let ((*recording* nil) (sets '()))
    (dolist (entry snapshot)
      (if (eq (cdr entry) :absent)
          (clear-cell sheet (car entry))
          (push (list (car entry) (cdr entry)) sets)))
    (when sets (set-cells sheet (nreverse sets)))))

(defun undo (sheet)
  "Undo the last formula edit (SET-CELL/SET-CELLS/CLEAR-CELL). Returns T if one
was undone, NIL if the undo stack was empty."
  (with-sheet-lock (sheet)
    (let ((entry (pop (sheet-undo-stack sheet))))
      (when entry
        (push (capture-cells sheet (mapcar #'car entry)) (sheet-redo-stack sheet))
        (restore-cells sheet entry)
        t))))

(defun redo (sheet)
  "Redo the last undone edit. Returns T if one was redone, else NIL."
  (with-sheet-lock (sheet)
    (let ((entry (pop (sheet-redo-stack sheet))))
      (when entry
        (push (capture-cells sheet (mapcar #'car entry)) (sheet-undo-stack sheet))
        (restore-cells sheet entry)
        t))))

(defun set-cell (sheet designator formula &key (volatile nil volatile-supplied-p))
  "Store FORMULA in the cell at DESIGNATOR and recompute it together
with everything (transitively) depending on it. Returns the new value
of the cell, or signals if its own evaluation fails.

VOLATILE, when supplied, marks (or unmarks) the cell as a volatile cell —
one recomputed on every recalc regardless of whether a precedent changed
(cf. RAND()/NOW()). Volatility is sticky: it only changes when the keyword
is explicitly passed, so re-setting a formula doesn't silently demote it."
  (with-sheet-lock (sheet)
    (let* ((ref (parse-ref designator))
           (snapshot (capture-cells sheet (list ref)))   ; before ENSURE (:absent if new)
           (cell (ensure-cell sheet ref)))
      (unless (cell-writable-p cell formula) (error 'readonly-cell :ref ref))
      (push-undo sheet snapshot)
      (note-set cell sheet ref formula *actor* (funcall *audit-clock*))
      (setf (cell-formula cell) formula)
      (when volatile-supplied-p
        (set-cell-volatile sheet ref volatile))
      (recompute-closure sheet (list ref))
      (if (cell-err cell)
          (error (cell-err cell))
          (cell-value cell)))))

(defun set-cells (sheet bindings)
  "Install several formulas at once, then recompute their combined closure
in a single sweep. BINDINGS is a list of (DESIGNATOR FORMULA) pairs.

Because all formulas are installed *before* any recomputation, cells may
reference one another in any order (forward references resolve without a
transient error), and dependents shared by several seeds recompute once.

Unlike SET-CELL this does not re-signal: a cell whose formula errors stores
its condition as usual (readable via GET-VALUE) and one broken cell does not
abort the batch. Returns the list of resulting values in input order (NIL
for a cell that errored). A later pair for the same cell wins."
  (with-sheet-lock (sheet)
    ;; guard first: refuse the whole batch if any existing target is read-only
    (dolist (pair bindings)
      (let* ((ref (parse-ref (first pair)))
             (existing (find-cell sheet ref)))
        (when (and existing (not (cell-writable-p existing (second pair))))
          (error 'readonly-cell :ref ref))))
    (push-undo sheet (capture-cells sheet (mapcar (lambda (p) (parse-ref (first p)))
                                                  bindings)))
    (let* ((time (funcall *audit-clock*))    ; one timestamp for the whole batch
           (refs (loop for (designator formula) in bindings
                       for ref = (parse-ref designator)
                       for cell = (ensure-cell sheet ref)
                       do (note-set cell sheet ref formula *actor* time)
                          (setf (cell-formula cell) formula)
                       collect ref)))
      (recompute-closure sheet refs)
      (mapcar (lambda (r) (get-value sheet r)) refs))))

(defun clear-cell (sheet designator)
  "Empty a cell and recompute its dependents (which will now error if
they still read it)."
  (with-sheet-lock (sheet)
    (let* ((ref (parse-ref designator))
           (cell (find-cell sheet ref)))
      (when cell
        (unless (cell-writable-p cell) (error 'readonly-cell :ref ref))
        (push-undo sheet (capture-cells sheet (list ref)))
        (let ((deps (cell-dependents cell)))
          ;; detach from our precedents
          (dolist (p (cell-precedents cell))
            (let ((c (find-cell sheet p)))
              (when c (setf (cell-dependents c)
                            (remove ref (cell-dependents c) :test 'equal)))))
          ;; and from any cross-sheet producers we read
          (when (cell-foreign-precedents cell) (detach-foreign sheet ref cell))
          (remhash ref (sheet-cells sheet))
          (remhash ref (sheet-volatiles sheet)) ; drop from the volatile registry
          ;; the cleared cell won't recompute itself, so report it explicitly.
          (recompute-closure sheet deps :extra-changed (list ref))))
      (values))))

(defun get-value (sheet designator)
  "Return (values value error-or-nil) for a cell. Empty cell -> NIL,NIL."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (parse-ref designator))))
      (if cell
          (values (cell-value cell) (cell-err cell))
          (values nil nil)))))

(defun get-formula (sheet designator)
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (parse-ref designator))))
      (and cell (cell-formula cell)))))

(defun dependents (sheet designator)
  "Direct dependents of a cell, as ref conses."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (parse-ref designator))))
      (and cell (copy-list (cell-dependents cell))))))

(defun precedents (sheet designator)
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (parse-ref designator))))
      (and cell (copy-list (cell-precedents cell))))))

;;;; ------------------------------------------------------------------
;;;; Recomputation strategy
;;;;
;;;; When seed cells change, recompute them and the parts of their dependent
;;;; cone that actually change. AFFECTED-CLOSURE collects the whole cone;
;;;; TOPOLOGICAL-ORDER puts precedents before dependents; RECOMPUTE-CLOSURE
;;;; then walks that order and recomputes a cell only if it is a seed/volatile
;;;; or one of its precedents changed value (tracked in *CHANGED* by
;;;; COMPUTE-CELL). A cell that recomputes to an unchanged value does not
;;;; propagate, so its subtree is short-circuited. EVALUATE-REF still pulls
;;;; precedents on demand, so out-of-order reads stay correct; cells outside
;;;; the closure can't have changed (their precedents are stable).
;;;; ------------------------------------------------------------------

(defun affected-closure (sheet seeds)
  "Return SEEDS plus all transitive dependents, as a list of refs."
  (let ((seen (make-hash-table :test 'equal))
        (order '()))
    (labels ((visit (ref)
               (unless (gethash ref seen)
                 (setf (gethash ref seen) t)
                 (push ref order)
                 (let ((cell (find-cell sheet ref)))
                   (when cell
                     (dolist (d (cell-dependents cell)) (visit d)))))))
      (dolist (s seeds) (visit s)))
    order))

(defun topological-order (sheet refs)
  "Order the cells in REFS so each comes after its precedents that are also in
REFS (DFS post-order over the precedent edges; back-edges from cycles are left
in arrival order)."
  (let ((in-set (make-hash-table :test 'equal))
        (state (make-hash-table :test 'equal))   ; nil | :open | :done
        (order '()))
    (dolist (r refs) (setf (gethash r in-set) t))
    (labels ((visit (ref)
               (case (gethash ref state)
                 ((:done :open))                  ; done, or a cycle back-edge
                 (t (setf (gethash ref state) :open)
                    (let ((cell (find-cell sheet ref)))
                      (when cell
                        (dolist (p (cell-precedents cell))
                          (when (gethash p in-set) (visit p)))))
                    (setf (gethash ref state) :done)
                    (push ref order)))))
      (dolist (r refs) (visit r)))
    (nreverse order)))

(defun recompute-local (sheet seeds &key extra-changed)
  "One local recompute sweep on SHEET: recompute SEEDS and the changed part of
their dependent cone (volatile cells folded into the seeds). EXTRA-CHANGED lists
refs to force into the change set even though they weren't recomputed here (e.g.
a cell CLEAR-CELL just removed). Returns the list of local refs whose value or
error changed this sweep (NIL when neither a change hook nor a workbook needs
it), and fires SHEET's change hook with that set, row-major sorted."
  (let* ((*sheet* sheet)
         (*fresh* (make-hash-table :test 'equal))
         (*changed* (make-hash-table :test 'equal))
         (all-seeds (append seeds (volatile-refs sheet)))
         (seed-set (make-hash-table :test 'equal))
         (ordered (topological-order sheet (affected-closure sheet all-seeds)))
         (skipped (make-hash-table :test 'equal)))
    (dolist (s all-seeds) (setf (gethash s seed-set) t))
    (dolist (ref ordered)
      (let ((cell (find-cell sheet ref)))
        (when (and cell (not (gethash ref *fresh*)))
          (if (or (gethash ref seed-set)
                  (some (lambda (p) (gethash p *changed*)) (cell-precedents cell)))
              ;; a seed, or an input changed: recompute (COMPUTE-CELL records
              ;; into *CHANGED* whether the output actually changed).
              (handler-case (compute-cell sheet ref cell)
                (sheet-error () nil))
              ;; inputs unchanged: skip recompute but mark up to date so a
              ;; later reader reuses the value instead of recomputing.
              (progn (setf (gethash ref *fresh*) t)
                     (setf (gethash ref skipped) t))))))
    ;; sweep settled: notify each *recomputed* (fresh, not skipped), non-errored
    ;; cell — skipped cells didn't change, so their :after sinks stay quiet.
    (maphash (lambda (ref present)
               (declare (ignore present))
               (unless (gethash ref skipped)
                 (let ((cell (find-cell sheet ref)))
                   (when (and cell (null (cell-err cell)))
                     (cell-swept cell sheet ref)))))
             *fresh*)
    ;; finally, gather the exact change set: refs whose value/error changed this
    ;; sweep, plus any EXTRA-CHANGED (e.g. cleared cells). Needed for the UI
    ;; repaint hook and for cross-sheet propagation; when neither applies, skip
    ;; the gather entirely so the single-sheet hot path pays nothing.
    (let ((hook (sheet-change-hook sheet)))
      (when (or hook (sheet-workbook sheet))
        (let ((changed '()))
          (maphash (lambda (ref present) (declare (ignore present))
                     (push ref changed))
                   *changed*)
          (dolist (r extra-changed) (pushnew r changed :test #'equal))
          (setf changed (sort changed #'ref-lessp))
          (when hook (funcall hook changed))
          changed)))))

(defun recompute-closure (sheet seeds &key extra-changed)
  "Recompute SEEDS on SHEET, then — if SHEET is in a workbook — cascade the
resulting changes to cross-sheet consumers to a fixpoint. The public mutators all
funnel through here, so every edit path both repaints (via the change hook) and
propagates across sheets."
  (let ((changed (recompute-local sheet seeds :extra-changed extra-changed)))
    (when (sheet-workbook sheet)
      (cascade-foreign sheet changed))
    changed))

(defun %foreign-work (sheet changed)
  "For the CHANGED local refs of SHEET, group the foreign consumers to refresh as
an ordered alist (consumer-sheet . refs)."
  (let ((by-sheet (make-hash-table :test 'eq)) (order '()))
    (dolist (r changed)
      (dolist (g (gethash r (sheet-foreign-dependents sheet)))
        (let ((cs (car g)) (cr (cdr g)))
          (unless (gethash cs by-sheet) (push cs order))
          (pushnew cr (gethash cs by-sheet) :test #'equal))))
    (loop for cs in (nreverse order) collect (cons cs (gethash cs by-sheet)))))

(defun cascade-foreign (origin changed)
  "Propagate CHANGED cells of ORIGIN across sheet boundaries: recompute their
foreign consumers, then those consumers' consumers, to a fixpoint. Producers are
always recomputed before consumers, so cross-sheet reads see settled values. A
per-cell revisit cap breaks cross-sheet reference cycles — cells that exceed it
are flagged with a CYCLIC-REFERENCE error rather than looping forever."
  (let ((queue (%foreign-work origin changed))
        (visits (make-hash-table :test 'equal))
        ;; a value in an N-sheet acyclic graph is recomputed at most ~N times;
        ;; anything past that is a genuine cross-sheet cycle.
        (cap (+ 2 (workbook-sheet-count (sheet-workbook origin)))))
    (loop while queue do
      (let ((batch queue))
        (setf queue '())
        (dolist (item batch)
          (let ((tsheet (car item)) (live '()) (cyclic '()))
            (dolist (r (cdr item))
              (let* ((g (cons tsheet r))
                     (n (1+ (gethash g visits 0))))
                (setf (gethash g visits) n)
                (if (<= n cap) (push r live) (push r cyclic))))
            (when cyclic (%flag-cross-cycle tsheet cyclic))
            (when live
              (let ((tchanged (recompute-local tsheet live)))
                (setf queue (append queue (%foreign-work tsheet tchanged)))))))))))

(defun %flag-cross-cycle (sheet refs)
  "Mark REFS in SHEET as participating in a cross-sheet reference cycle."
  (dolist (r refs)
    (let ((cell (find-cell sheet r)))
      (when cell
        (setf (cell-err cell) (make-condition 'cyclic-reference :cells (list r))
              (cell-value cell) nil))))
  (let ((hook (sheet-change-hook sheet)))
    (when hook (funcall hook (sort (copy-list refs) #'ref-lessp)))))

(defun recompute-workbook (workbook)
  "Recompute every sheet in WORKBOOK to a cross-sheet fixpoint. Used after
loading, when all sheets and their cross-references are finally in place."
  (dotimes (_ (max 2 (1+ (workbook-sheet-count workbook))))
    (dolist (s (workbook-sheets workbook))
      (recalc-all s)))
  (values))

(defun recalc (sheet designator)
  "Force recomputation of one cell and its dependents."
  (with-sheet-lock (sheet)
    (recompute-closure sheet (list (parse-ref designator)))
    (get-value sheet designator)))

(defun recalc-all (sheet)
  "Recompute every cell in the sheet."
  (with-sheet-lock (sheet)
    (let ((all '()))
      (map-cells (lambda (ref cell) (declare (ignore cell)) (push ref all)) sheet)
      (recompute-closure sheet all)))
  (values))
