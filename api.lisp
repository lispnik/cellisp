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

(defun recompute-closure (sheet seeds)
  "Recompute SEEDS and the changed part of their dependent cone. Volatile cells
are folded into the seeds so they refresh every sweep."
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
