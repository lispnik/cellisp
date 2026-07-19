(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Public API
;;;; ------------------------------------------------------------------

(defmacro with-actor ((actor) &body body)
  "Run BODY with ACTOR recorded as the author of any mutations (for
AUDITED-MIXIN / edit provenance)."
  `(let ((*actor* ,actor)) ,@body))

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
           (cell (ensure-cell sheet ref)))
      (unless (cell-writable-p cell formula) (error 'readonly-cell :ref ref))
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
        (let ((deps (cell-dependents cell)))
          ;; detach from our precedents
          (dolist (p (cell-precedents cell))
            (let ((c (find-cell sheet p)))
              (when c (setf (cell-dependents c)
                            (remove ref (cell-dependents c) :test 'equal)))))
          (remhash ref (sheet-cells sheet))
          (remhash ref (sheet-volatiles sheet)) ; drop from the volatile registry
          (recompute-closure sheet deps)))
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
;;;; When a set of seed cells change, recompute them and their transitive
;;;; dependents. We compute each in dependency order via DFS over the
;;;; dependent graph; EVALUATE-REF itself pulls precedents on demand, so
;;;; a simple "recompute each affected cell" pass converges as long as we
;;;; clear caches first. Cycle detection lives in EVALUATE-REF.
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

(defun recompute-closure (sheet seeds)
  "Recompute SEEDS and their dependents. EVALUATE-REF resolves ordering
by pulling precedents, so we just force each affected cell once. Volatile
cells are folded into the seed set so they (and their dependents) recompute
on every sweep even when none of their precedents changed."
  (let* ((*sheet* sheet)
         (*fresh* (make-hash-table :test 'equal))
         (refs (affected-closure sheet (append seeds (volatile-refs sheet)))))
    (dolist (ref refs)
      (let ((cell (find-cell sheet ref)))
        ;; skip cells already (re)computed this sweep by an earlier cell
        ;; pulling them as a precedent — each cell computes at most once.
        (when (and cell (not (gethash ref *fresh*)))
          (handler-case (compute-cell sheet ref cell)
            ;; errors are stored on the cell by COMPUTE-CELL; swallow here
            ;; so one broken cell doesn't abort the whole sweep.
            (sheet-error () nil)))))
    ;; sweep settled: notify each computed cell (observed cells fire here).
    ;; Skip cells that errored this sweep — they have no settled value for the
    ;; :after CELL-SWEPT sinks (observe/log/persist/…) to record or emit.
    (maphash (lambda (ref present)
               (declare (ignore present))
               (let ((cell (find-cell sheet ref)))
                 (when (and cell (null (cell-err cell)))
                   (cell-swept cell sheet ref))))
             *fresh*)))

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
