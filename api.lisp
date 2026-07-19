(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Public API
;;;; ------------------------------------------------------------------

(defun set-cell (sheet designator formula)
  "Store FORMULA in the cell at DESIGNATOR and recompute it together
with everything (transitively) depending on it. Returns the new value
of the cell, or signals if its own evaluation fails."
  (let* ((ref (parse-ref designator))
         (cell (ensure-cell sheet ref)))
    (setf (cell-formula cell) formula)
    (let ((*sheet* sheet))
      (recompute-closure sheet (list ref)))
    (if (cell-err cell)
        (error (cell-err cell))
        (cell-value cell))))

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
  (let ((refs (loop for (designator formula) in bindings
                    for ref = (parse-ref designator)
                    do (setf (cell-formula (ensure-cell sheet ref)) formula)
                    collect ref)))
    (let ((*sheet* sheet))
      (recompute-closure sheet refs))
    (mapcar (lambda (r) (get-value sheet r)) refs)))

(defun clear-cell (sheet designator)
  "Empty a cell and recompute its dependents (which will now error if
they still read it)."
  (let* ((ref (parse-ref designator))
         (cell (find-cell sheet ref)))
    (when cell
      (let ((deps (cell-dependents cell)))
        ;; detach from our precedents
        (dolist (p (cell-precedents cell))
          (let ((c (find-cell sheet p)))
            (when c (setf (cell-dependents c)
                          (remove ref (cell-dependents c) :test 'equal)))))
        (remhash ref (sheet-cells sheet))
        (let ((*sheet* sheet))
          (recompute-closure sheet deps))))
    (values)))

(defun get-value (sheet designator)
  "Return (values value error-or-nil) for a cell. Empty cell -> NIL,NIL."
  (let ((cell (find-cell sheet (parse-ref designator))))
    (if cell
        (values (cell-value cell) (cell-err cell))
        (values nil nil))))

(defun get-formula (sheet designator)
  (let ((cell (find-cell sheet (parse-ref designator))))
    (and cell (cell-formula cell))))

(defun dependents (sheet designator)
  "Direct dependents of a cell, as ref conses."
  (let ((cell (find-cell sheet (parse-ref designator))))
    (and cell (copy-list (cell-dependents cell)))))

(defun precedents (sheet designator)
  (let ((cell (find-cell sheet (parse-ref designator))))
    (and cell (copy-list (cell-precedents cell)))))

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
by pulling precedents, so we just force each affected cell once."
  (let ((*sheet* sheet)
        (*fresh* (make-hash-table :test 'equal))
        (refs (affected-closure sheet seeds)))
    (dolist (ref refs)
      (let ((cell (find-cell sheet ref)))
        ;; skip cells already (re)computed this sweep by an earlier cell
        ;; pulling them as a precedent — each cell computes at most once.
        (when (and cell (not (gethash ref *fresh*)))
          (handler-case (compute-cell sheet ref cell)
            ;; errors are stored on the cell by COMPUTE-CELL; swallow here
            ;; so one broken cell doesn't abort the whole sweep.
            (sheet-error () nil)))))))

(defun recalc (sheet designator)
  "Force recomputation of one cell and its dependents."
  (recompute-closure sheet (list (parse-ref designator)))
  (get-value sheet designator))

(defun recalc-all (sheet)
  "Recompute every cell in the sheet."
  (let ((all '()))
    (map-cells (lambda (ref cell) (declare (ignore cell)) (push ref all)) sheet)
    (recompute-closure sheet all))
  (values))
