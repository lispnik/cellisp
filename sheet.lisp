(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Conditions
;;;; ------------------------------------------------------------------

(define-condition sheet-error (error)
  ((format-control :initarg :format-control :initform "Sheet error"
                   :reader sheet-error-format-control)
   (format-arguments :initarg :format-arguments :initform '()
                     :reader sheet-error-format-arguments))
  (:report (lambda (c s)
             (apply #'format s (sheet-error-format-control c)
                    (sheet-error-format-arguments c)))))

(define-condition cyclic-reference (sheet-error)
  ((cells :initarg :cells :reader cyclic-reference-cells))
  (:report (lambda (c s)
             (format s "Cyclic reference through: ~{~A~^ -> ~}"
                     (mapcar #'ref-string (cyclic-reference-cells c))))))

(define-condition unbound-cell (sheet-error)
  ((ref :initarg :ref :reader unbound-cell-ref))
  (:report (lambda (c s)
             (format s "Cell ~A is empty" (ref-string (unbound-cell-ref c))))))

(define-condition cell-eval-error (sheet-error)
  ((ref :initarg :ref :reader cell-eval-error-ref)
   (original :initarg :original :reader cell-eval-error-original))
  (:report (lambda (c s)
             (format s "Error evaluating ~A: ~A"
                     (ref-string (cell-eval-error-ref c))
                     (cell-eval-error-original c)))))

(define-condition readonly-cell (sheet-error)
  ((ref :initarg :ref :reader readonly-cell-ref))
  (:report (lambda (c s)
             (format s "Cell ~A is read-only" (ref-string (readonly-cell-ref c))))))

(define-condition invalid-value (sheet-error)
  ((ref :initarg :ref :reader invalid-value-ref)
   (value :initarg :value :reader invalid-value-value))
  (:report (lambda (c s)
             (format s "Invalid value ~S for cell ~A"
                     (invalid-value-value c) (ref-string (invalid-value-ref c))))))

;;;; ------------------------------------------------------------------
;;;; Sheet
;;;; ------------------------------------------------------------------

;;; A sheet mutates shared state in place (the CELLS table, per-cell adjacency
;;; lists) through dynamic vars. Every public entry point takes the sheet's
;;; recursive LOCK so concurrent readers/writers — and out-of-band async
;;; deliveries from other threads — are serialized. The lock is recursive so a
;;; callback fired mid-sweep (e.g. an observer) may re-enter the read API.

(defstruct (sheet (:constructor %make-sheet))
  ;; ref-cons -> cell. Refs are equal-comparable conses, so use EQUAL.
  (cells (make-hash-table :test 'equal) :type hash-table)
  ;; Extra bindings (a plist or alist) exposed to formulas, e.g. constants.
  (environment '() :type list)
  ;; Set of refs whose cells are volatile (recompute every sweep). Kept as a
  ;; registry so RECOMPUTE-CLOSURE can seed them without scanning all cells.
  (volatiles (make-hash-table :test 'equal) :type hash-table)
  ;; Set of refs held frozen (COMPUTE-CELL skips them; see there).
  (frozen (make-hash-table :test 'equal) :type hash-table)
  ;; Named-cell aliases: upcased name string -> ref. RESOLVE-REF consults it.
  (names (make-hash-table :test 'equal) :type hash-table)
  ;; Cell notes/comments: ref -> string. Metadata only — the engine never reads
  ;; them; they follow their cell across structural edits and are serialized.
  (notes (make-hash-table :test 'equal) :type hash-table)
  ;; Merged cells: a list of (top-left . bottom-right) ref rectangles. Metadata
  ;; for a UI — the engine never merges values; the top-left is the anchor.
  (merges '() :type list)
  ;; Undo/redo of formula edits: each entry is a snapshot alist of
  ;; (ref . formula-or-:absent) — the state to restore.
  (undo-stack '() :type list)
  (redo-stack '() :type list)
  ;; Workbook membership: the owning workbook (or NIL when standalone) and this
  ;; sheet's name within it. When WORKBOOK is NIL the sheet behaves exactly as a
  ;; single-sheet engine — no cross-sheet machinery runs.
  (workbook nil)
  (name nil)
  ;; Cross-sheet producer side: local-ref -> list of consumer grefs (sheet . ref)
  ;; in OTHER sheets that read this cell. Seeds cross-sheet propagation.
  (foreign-dependents (make-hash-table :test 'equal) :type hash-table)
  ;; Optional callback invoked after each recompute sweep with the sorted list
  ;; of refs whose value or error changed — the repaint set for a UI. NIL = off.
  ;; Not serialized (a live closure); reattach after LOAD-SHEET.
  (change-hook nil)
  ;; Serializes all public access to this sheet (see comment above).
  (lock (bt:make-recursive-lock "cellisp-sheet")))

(defmacro with-sheet-lock ((sheet) &body body)
  "Run BODY holding SHEET's recursive lock."
  `(bt:with-recursive-lock-held ((sheet-lock ,sheet)) ,@body))

(defun make-sheet (&key environment)
  "Create an empty sheet. ENVIRONMENT is an alist of (symbol . value)
made visible to every formula via let-bindings established by EVAL-FORMULA."
  (%make-sheet :environment environment))

(defun find-cell (sheet ref)
  (gethash ref (sheet-cells sheet)))

(defun ensure-cell (sheet ref)
  (or (find-cell sheet ref)
      (setf (gethash ref (sheet-cells sheet)) (make-instance 'cell))))

(defun map-cells (fn sheet)
  "Call FN with (ref cell) for every non-empty cell."
  (maphash (lambda (ref cell) (funcall fn ref cell)) (sheet-cells sheet)))

(defun ref-lessp (a b)
  "Row-major total order on refs — sort key for deterministic change sets."
  (or (< (ref-row a) (ref-row b))
      (and (= (ref-row a) (ref-row b)) (< (ref-col a) (ref-col b)))))

(defun set-change-hook (sheet fn)
  "Install FN as SHEET's change hook (or NIL to clear it). After every recompute
sweep — from any edit: SET-CELL, SET-CELLS, CLEAR-CELL, RECALC(-ALL), UNDO/REDO,
structural edits — FN is called with the row-major-sorted list of refs whose
value or error changed. The list is empty when an edit changed nothing (e.g. a
cell reset to its current value, whose dependents are short-circuited). FN runs
under the sheet lock; keep it quick, and note UNDO of a mixed edit may fire it
more than once. Returns FN."
  (with-sheet-lock (sheet)
    (setf (sheet-change-hook sheet) fn)))

(defun %cell-content-p (cell)
  "True if CELL carries real content (a formula, value, or error) rather than
being a pure dependency-placeholder created by ENSURE-CELL to hold a back-link to
a referenced-empty cell."
  (or (cell-formula cell) (cell-value cell) (cell-err cell)))

(defun used-range (sheet)
  "The tight bounding box of the cells with content as a (top-left .
bottom-right) ref cons, or NIL when the sheet has none. Pure dependency
-placeholder cells (an empty cell a formula merely references) are ignored, so
the range reflects actual content. Read it with (cells (car r) (cdr r))."
  (with-sheet-lock (sheet)
    (let (minr minc maxr maxc)
      (map-cells (lambda (ref cell)
                   (when (%cell-content-p cell)
                     (let ((r (ref-row ref)) (c (ref-col ref)))
                       (when (or (null minr) (< r minr)) (setf minr r))
                       (when (or (null maxr) (> r maxr)) (setf maxr r))
                       (when (or (null minc) (< c minc)) (setf minc c))
                       (when (or (null maxc) (> c maxc)) (setf maxc c)))))
                 sheet)
      (and minr (cons (cons minr minc) (cons maxr maxc))))))

(defun sheet-dimensions (sheet)
  "Two values: rows and columns needed to contain every non-empty cell — i.e.
(1+ max-row) and (1+ max-col) — or 0, 0 for an empty sheet. A UI grid's extent."
  (let ((r (used-range sheet)))
    (if r
        (values (1+ (ref-row (cdr r))) (1+ (ref-col (cdr r))))
        (values 0 0))))

(defun volatile-refs (sheet)
  "List of refs currently registered volatile on SHEET."
  (loop for ref being the hash-keys of (sheet-volatiles sheet) collect ref))

(defun set-cell-volatile (sheet ref volatile)
  "Add REF to (or remove it from) SHEET's volatile registry. Volatility is a
scheduling attribute independent of the cell's class, so this touches only the
registry — any kind of cell can be volatile."
  (if volatile
      (setf (gethash ref (sheet-volatiles sheet)) t)
      (remhash ref (sheet-volatiles sheet))))

(defun %name-key (name) (string-upcase (string name)))

;;; The names table maps an upcased name to either a single cell (a ref, i.e. an
;;; (integer . integer) cons) or a rectangular range (a (tl-ref . br-ref) cons,
;;; whose CAR is itself a cons). %RANGE-VALUE-P tells them apart everywhere the
;;; distinction matters — RESOLVE-REF, CELLS, structural shifting, serialization.
(defun %range-value-p (val) (consp (car val)))

(defun %lookup-name-in (sheet desig)
  "The value DESIG names in SHEET (a single ref, or a range's (tl . br) cons), or
NIL. Only strings/symbols name anything, and the lookup is skipped when the sheet
has no names — keeping the hot path fast."
  (and (plusp (hash-table-count (sheet-names sheet)))
       (typep desig '(or string symbol))
       (gethash (%name-key desig) (sheet-names sheet))))

(defun resolve-ref-in (sheet desig)
  "Resolve DESIG to a single ref in SHEET: a registered name takes precedence (a
range name yields its top-left corner), otherwise DESIG is parsed as an A1
reference. This is what lets the public API (GET-VALUE, SET-CELL, the notes/
attributes/mixin drivers, …) accept a cell *name* anywhere it accepts an A1 ref
— the same resolution a formula's CELL operator does, but with the sheet passed
explicitly rather than taken from *SHEET*."
  (let ((named (%lookup-name-in sheet desig)))
    (cond ((null named) (parse-ref desig))
          ((%range-value-p named) (car named))   ; range name -> its top-left
          (t named))))

(defun volatile-p (sheet designator)
  "True if DESIGNATOR is registered volatile on SHEET."
  (with-sheet-lock (sheet)
    (and (gethash (resolve-ref-in sheet designator) (sheet-volatiles sheet)) t)))

(defun frozen-p (sheet designator)
  "True if DESIGNATOR is frozen (held at its value, not recomputed)."
  (with-sheet-lock (sheet)
    (and (gethash (resolve-ref-in sheet designator) (sheet-frozen sheet)) t)))

(defun set-name (sheet name designator)
  "Bind NAME (a string or symbol, case-insensitive) as an alias for the cell at
DESIGNATOR, so formulas may write (cell NAME). Returns NAME."
  (with-sheet-lock (sheet)
    (setf (gethash (%name-key name) (sheet-names sheet)) (resolve-ref-in sheet designator))
    name))

(defun set-range (sheet name top-left bottom-right)
  "Bind NAME as an alias for the rectangular range TOP-LEFT..BOTTOM-RIGHT, so a
formula may write (cells NAME) to read the whole block. Returns NAME."
  (with-sheet-lock (sheet)
    (setf (gethash (%name-key name) (sheet-names sheet))
          (cons (parse-ref top-left) (parse-ref bottom-right)))
    name))

(defun range-ref (sheet name)
  "The (top-left . bottom-right) refs NAME spans, or NIL if NAME is unbound or
names a single cell rather than a range."
  (with-sheet-lock (sheet)
    (let ((val (gethash (%name-key name) (sheet-names sheet))))
      (and val (%range-value-p val) val))))

(defun remove-name (sheet name)
  "Remove the alias NAME."
  (with-sheet-lock (sheet)
    (remhash (%name-key name) (sheet-names sheet)))
  (values))

(defun name-ref (sheet name)
  "The ref NAME aliases, or NIL."
  (with-sheet-lock (sheet)
    (gethash (%name-key name) (sheet-names sheet))))

(defun map-names (fn sheet)
  "Call FN with (name value) for every registered name on SHEET. VALUE is a ref
for a single-cell name, or a (top-left . bottom-right) cons for a range name.
Names are the upcased keys."
  (with-sheet-lock (sheet)
    (maphash fn (sheet-names sheet))))

;;; --- cell notes / comments ------------------------------------------

(defun set-note (sheet designator text)
  "Attach a note (comment) to the cell at DESIGNATOR. TEXT is a string, or NIL /
\"\" to remove the note. A note is metadata: it needs no cell to exist, follows
its cell across structural edits, and is serialized. Returns TEXT."
  (with-sheet-lock (sheet)
    (let ((ref (resolve-ref-in sheet designator)))
      (if (or (null text) (and (stringp text) (string= text "")))
          (remhash ref (sheet-notes sheet))
          (setf (gethash ref (sheet-notes sheet)) text)))
    text))

(defun cell-note (sheet designator)
  "The note on the cell at DESIGNATOR, or NIL."
  (with-sheet-lock (sheet)
    (gethash (resolve-ref-in sheet designator) (sheet-notes sheet))))

(defun remove-note (sheet designator)
  "Remove any note on the cell at DESIGNATOR."
  (with-sheet-lock (sheet)
    (remhash (resolve-ref-in sheet designator) (sheet-notes sheet)))
  (values))

(defun map-notes (fn sheet)
  "Call FN with (ref note) for every noted cell."
  (with-sheet-lock (sheet)
    (maphash fn (sheet-notes sheet))))

;;; --- merged cells ---------------------------------------------------

(defun %normalize-rect (top-left bottom-right)
  "A (tl . br) rectangle from two designators, corners ordered."
  (let ((a (parse-ref top-left)) (b (parse-ref bottom-right)))
    (cons (cons (min (ref-row a) (ref-row b)) (min (ref-col a) (ref-col b)))
          (cons (max (ref-row a) (ref-row b)) (max (ref-col a) (ref-col b))))))

(defun ref-in-rect-p (ref rect)
  "True if REF lies within the (tl . br) RECT."
  (and (<= (ref-row (car rect)) (ref-row ref) (ref-row (cdr rect)))
       (<= (ref-col (car rect)) (ref-col ref) (ref-col (cdr rect)))))

(defun rects-overlap-p (a b)
  "True if rectangles A and B (each a (tl . br)) intersect."
  (and (<= (ref-row (car a)) (ref-row (cdr b)))
       (>= (ref-row (cdr a)) (ref-row (car b)))
       (<= (ref-col (car a)) (ref-col (cdr b)))
       (>= (ref-col (cdr a)) (ref-col (car b)))))

(defun merge-cells (sheet top-left bottom-right)
  "Merge the rectangle TOP-LEFT..BOTTOM-RIGHT into one visual cell anchored at its
top-left. Metadata only — values are untouched. Signals SHEET-ERROR if the
rectangle overlaps an existing merge. Returns the (tl . br) merge."
  (with-sheet-lock (sheet)
    (let ((rect (%normalize-rect top-left bottom-right)))
      (dolist (m (sheet-merges sheet))
        (when (rects-overlap-p m rect)
          (error 'sheet-error
                 :format-control "Merge ~A:~A overlaps an existing merge"
                 :format-arguments (list (ref-string (car rect))
                                         (ref-string (cdr rect))))))
      (push rect (sheet-merges sheet))
      rect)))

(defun merged-range (sheet designator)
  "The (tl . br) merge containing the cell at DESIGNATOR, or NIL."
  (with-sheet-lock (sheet)
    (let ((ref (resolve-ref-in sheet designator)))
      (find-if (lambda (m) (ref-in-rect-p ref m)) (sheet-merges sheet)))))

(defun unmerge-cells (sheet designator)
  "Remove the merge covering the cell at DESIGNATOR, if any."
  (with-sheet-lock (sheet)
    (let ((ref (resolve-ref-in sheet designator)))
      (setf (sheet-merges sheet)
            (remove-if (lambda (m) (ref-in-rect-p ref m)) (sheet-merges sheet)))))
  (values))

(defun merges (sheet)
  "The sheet's merges, each a (top-left . bottom-right) ref rectangle."
  (with-sheet-lock (sheet) (copy-list (sheet-merges sheet))))
