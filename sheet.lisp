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

;;; The three below carry no extra slots — they inherit SHEET-ERROR's
;;; format-control machinery — but exist as distinct classes so the display
;;; layer's ERROR-TOKEN can map them by TYPE instead of by parsing report text.
;;; They classify a reference/name/numeric failure at its signalling site.

(define-condition bad-reference (sheet-error) ()
  (:documentation "A reference to no valid grid position: the \"#REF!\" a
structural delete leaves in a formula, or a coordinate shifted off the grid.
The display layer renders it as #REF!."))

(define-condition unknown-name (sheet-error) ()
  (:documentation "An unparseable name, or an unknown sheet in a cross-sheet
reference. Rendered as #NAME?."))

(define-condition numeric-error (sheet-error) ()
  (:documentation "A numeric-domain failure — e.g. an aggregate over no numbers.
Rendered as #NUM!."))

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
  ;; Spill extents: anchor-ref -> (rows . cols) of the last SPILL/RESPILL there,
  ;; so RESPILL can clear a shrunk block. Follows structural edits; serialized.
  (spills (make-hash-table :test 'equal) :type hash-table)
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
  ;; Cross-sheet SPAN producer side: a column/row index -> list of (consumer-gref
  ;; . bound) entries in OTHER sheets that read this whole column/row (via
  ;; "Data!A:A") or its table rows (via "Data!Sales[Amount]"). BOUND is (bmin .
  ;; bmax) on the orthogonal axis for a row-bounded table read, or NIL for a whole
  ;; column/row. The coarse cross-sheet analog of FOREIGN-DEPENDENTS; consulted
  ;; (and bound-gated) by %FOREIGN-WORK.
  (foreign-col-watchers (make-hash-table :test 'eql) :type hash-table)
  (foreign-row-watchers (make-hash-table :test 'eql) :type hash-table)
  ;; Whole-column/row producer side: a column index -> the set of refs whose
  ;; formulas read that whole column (via COL/"A:A"), and likewise for rows.
  ;; Each set is a hash-table used as a ref-set. This is the coarse reverse
  ;; index that lets a change anywhere in column A re-fire A's readers in O(1)
  ;; without an edge per cell (cf. FOREIGN-DEPENDENTS). Rebuilt by RECALC-ALL.
  (col-watchers (make-hash-table :test 'eql) :type hash-table)
  (row-watchers (make-hash-table :test 'eql) :type hash-table)
  ;; Named tables: upcased-name string -> TABLE struct (a header'd rectangular
  ;; region whose columns are referenced by header text). Own slot like
  ;; NAMES/MERGES/SPILLS; serialized and shifted under structural edits.
  (tables (make-hash-table :test 'equal) :type hash-table)
  ;; Optional callback invoked after each recompute sweep with the sorted list
  ;; of refs whose value or error changed — the repaint set for a UI. NIL = off.
  ;; Not serialized (a live closure); reattach after LOAD-SHEET.
  (change-hook nil)
  ;; The recursive lock serializing public access to a STANDALONE sheet. A sheet
  ;; that belongs to a workbook does NOT use this — it shares the workbook's
  ;; single lock (see SHEET-LOCK), so one lock covers every sheet in the workbook
  ;; and a cross-sheet cascade holds it over all the peers it mutates.
  (own-lock (bt:make-recursive-lock "cellisp-sheet")))

;; SHEET-LOCK is defined in workbook.lisp (it needs WORKBOOK-LOCK, defined
;; there); declare it so this file compiles clean when WITH-SHEET-LOCK expands.
(declaim (ftype (function (t) t) sheet-lock))

(defmacro with-sheet-lock ((sheet) &body body)
  "Run BODY holding SHEET's serializing lock — the workbook's shared lock when
SHEET is in a workbook, otherwise the sheet's own (see SHEET-LOCK). The lock is
recursive, so re-entry (a cross-sheet cascade, an observer that reads a cell) is
safe."
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

;;; Whole-column/row watcher index — the coarse producer-side reverse map for
;;; span (COL/ROW/"A:A") dependencies. AXIS is :COL or :ROW; INDEX is a 0-based
;;; column or row index; each entry is a hash-set of the reader refs.

(defun %watchers-table (sheet axis)
  (ecase axis
    (:col (sheet-col-watchers sheet))
    (:row (sheet-row-watchers sheet))))

(defun watchers-of (sheet axis index)
  "List of reader refs whose formulas read the whole AXIS line INDEX (empty when
none) — the coarse dependents seeded when a cell on that line changes."
  (let ((set (gethash index (%watchers-table sheet axis))))
    (and set (loop for r being the hash-keys of set collect r))))

(defun add-watcher (sheet axis index ref)
  "Register REF as a whole-line reader of AXIS line INDEX."
  (let* ((table (%watchers-table sheet axis))
         (set (or (gethash index table)
                  (setf (gethash index table) (make-hash-table :test 'equal)))))
    (setf (gethash ref set) t)))

(defun remove-watcher (sheet axis index ref)
  "Unregister REF as a reader of AXIS line INDEX, dropping the line's set when it
empties."
  (let ((set (gethash index (%watchers-table sheet axis))))
    (when set
      (remhash ref set)
      (when (zerop (hash-table-count set))
        (remhash index (%watchers-table sheet axis))))))

(defun clear-watchers (sheet)
  "Drop every whole-column/row watcher — used before a structural edit rebuilds
the graph from scratch via RECALC-ALL."
  (clrhash (sheet-col-watchers sheet))
  (clrhash (sheet-row-watchers sheet)))

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

;;; Named tables — a header'd rectangular region whose columns are referenced by
;;; header text (Sales[Amount] / (table-col "Sales" "Amount")). Own registry (the
;;; TABLES slot), separate from NAMES; the TABLE struct is in cell.lisp.

(defun set-table (sheet name top-left bottom-right &key (headers t) totals)
  "Define a table NAME over the rectangle TOP-LEFT..BOTTOM-RIGHT (corners ordered).
HEADERS (default T) marks the first row as the header row (column names); TOTALS
marks the last row as a totals row (excluded from data reads). Signals if the
region overlaps a *different* existing table (redefining the same name replaces
it). Returns NAME."
  (with-sheet-lock (sheet)
    (let ((region (%normalize-rect top-left bottom-right))
          (key (%name-key name)))
      (maphash (lambda (k tbl)
                 (unless (string= k key)
                   (when (rects-overlap-p region (table-region tbl))
                     (error 'sheet-error
                            :format-control "Table ~S overlaps existing table ~S"
                            :format-arguments (list name (table-name tbl))))))
               (sheet-tables sheet))
      (setf (gethash key (sheet-tables sheet))
            (%make-table name region headers totals))
      name)))

(defun table-ref (sheet name)
  "The (top-left . bottom-right) region of table NAME, or NIL if unbound."
  (with-sheet-lock (sheet)
    (let ((tbl (gethash (%name-key name) (sheet-tables sheet))))
      (and tbl (copy-tree (table-region tbl))))))

(defun remove-table (sheet name)
  "Remove table NAME (the underlying cells are untouched)."
  (with-sheet-lock (sheet)
    (remhash (%name-key name) (sheet-tables sheet)))
  (values))

(defun map-tables (fn sheet)
  "Call FN with (name table-struct) for every table on SHEET (name = user casing)."
  (with-sheet-lock (sheet)
    (maphash (lambda (k tbl) (declare (ignore k)) (funcall fn (table-name tbl) tbl))
             (sheet-tables sheet))))

(defun table-at (sheet ref)
  "The TABLE struct whose region contains REF, or NIL (tables never overlap, so
the first match is the only one)."
  (with-sheet-lock (sheet)
    (maphash (lambda (k tbl)
               (declare (ignore k))
               (when (ref-in-rect-p ref (table-region tbl))
                 (return-from table-at tbl)))
             (sheet-tables sheet))
    nil))

(defun %table-data-rows (table)
  "The inclusive (r0 . r1) row range of TABLE's DATA — excluding the header row
and, when TOTALS-P, the trailing totals row — or NIL when there are no data rows."
  (let* ((region (table-region table))
         (r0 (+ (ref-row (car region)) (if (table-headers-p table) 1 0)))
         (r1 (- (ref-row (cdr region)) (if (table-totals-p table) 1 0))))
    (and (<= r0 r1) (cons r0 r1))))

;; %TABLE-COL-INDEX (header-text column resolution) lives in eval.lisp — it must
;; FORCE the header cells up to date (they may be uncomputed mid-sweep) via
;; EVALUATE-REF, without recording a dependency.

(defun %overlaps-other-table (sheet self-key region)
  "True if REGION overlaps a table other than SELF-KEY."
  (block nil
    (maphash (lambda (k tbl)
               (unless (string= k self-key)
                 (when (rects-overlap-p region (table-region tbl)) (return t))))
             (sheet-tables sheet))
    nil))

(defun %maybe-grow-tables (sheet ref)
  "Auto-expand: if REF sits directly below a table's region (a new data row, when
the table has no totals row capping it) or directly to its right (a new column),
grow that table's region to include it — unless the growth would overlap another
table. Called on the set path BEFORE recompute, so the enlarged region is read
this sweep. No-op when the sheet has no tables."
  (when (plusp (hash-table-count (sheet-tables sheet)))
    (let ((r (ref-row ref)) (c (ref-col ref)))
      (maphash
       (lambda (key tbl)
         (let* ((rg (table-region tbl))
                (r0 (ref-row (car rg))) (c0 (ref-col (car rg)))
                (r1 (ref-row (cdr rg))) (c1 (ref-col (cdr rg)))
                (new (cond
                       ;; a new row directly below (no totals row to cap it)
                       ((and (not (table-totals-p tbl)) (= r (1+ r1)) (<= c0 c c1))
                        (cons (car rg) (make-ref (1+ r1) c1)))
                       ;; a new column directly to the right
                       ((and (= c (1+ c1)) (<= r0 r r1))
                        (cons (car rg) (make-ref r1 (1+ c1)))))))
           (when (and new (not (%overlaps-other-table sheet key new)))
             (setf (table-region tbl) new))))
       (sheet-tables sheet)))))

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
