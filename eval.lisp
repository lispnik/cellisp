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
(defvar *collected-ranges* nil
  "When non-nil (a hash-table keyed EQUAL), COL/ROW/\"A:A\" reads record the SPANs
they touch — the coarse whole-column/row analog of *COLLECTED-PRECEDENTS*.")
(defvar *fresh* nil
  "When bound (a hash-table) for the duration of one recompute sweep, holds
the refs already computed this sweep so each cell is computed at most once.")
(defvar *changed* nil
  "When bound (a hash-table) for a sweep, COMPUTE-CELL records refs whose
value/error actually changed — RECOMPUTE-CLOSURE uses this to short-circuit
recompute of subtrees whose inputs did not change.")
(defvar *changed-cols* nil
  "When bound (a hash-table used as an index-set) for a sweep, holds the column
indices carrying a cell whose value/error changed — so a whole-column reader
(A:A) recomputes without a per-cell edge. Sibling: *CHANGED-ROWS*.")
(defvar *changed-rows* nil
  "Row-index counterpart of *CHANGED-COLS*.")
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

(defun note-range (span)
  "Record a whole-column/row (SPAN) read by the formula being computed — the
coarse counterpart of NOTE-PRECEDENT."
  (when *collected-ranges*
    (setf (gethash span *collected-ranges*) t)))

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
        ;; "#REF!" — the sentinel a structural delete leaves in a formula — also
        ;; contains a '!', but it is a dangling *reference*, not a sheet-qualified
        ;; one; leave it whole so it resolves to a BAD-REFERENCE (#REF!), not a
        ;; missing sheet (#NAME?).
        (if (and bang (not (string= s "#REF!")))
            (values (subseq s 0 bang) (subseq s (1+ bang)))
            (values nil designator)))
      (values nil designator)))

(defun require-sheet (name)
  "The sheet named NAME in *SHEET*'s workbook, or a SHEET-ERROR if there is no
workbook or no such sheet."
  (let ((wb (and *sheet* (sheet-workbook *sheet*))))
    (or (and wb (find-sheet wb name))
        (error 'unknown-name
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

;;; The name-resolution logic lives in sheet.lisp (RESOLVE-REF-IN / %LOOKUP-NAME
;;; -IN, sheet passed explicitly) so the public API can share it. Inside a formula
;;; the sheet is the dynamically-bound *SHEET*, so these thin wrappers just supply
;;; it (and fall back to plain parsing when no sheet is bound).

(defun %lookup-name (designator)
  "The value DESIGNATOR names in *SHEET* (a ref or a range cons), or NIL."
  (and *sheet* (%lookup-name-in *sheet* designator)))

(defun resolve-ref (designator)
  "Resolve DESIGNATOR to a single ref against *SHEET* — a registered name takes
precedence (a range name yields its top-left), else an A1 parse."
  (if *sheet* (resolve-ref-in *sheet* designator) (parse-ref designator)))

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
empty cell there signals, preserving error propagation.)

A single-argument string may instead name a whole column/row — (cells \"A:A\"),
(cells \"1:1\") — read as a SPAN (one coarse dependency, not per cell), or a table
column — (cells \"Sales[Amount]\") — read via TABLE-COL."
  (multiple-value-bind (tname tcol tpart)
      (if bottom-right (values nil nil nil) (%parse-structured-ref top-left))
    (let ((span (and (null bottom-right) (null tname) (%parse-span top-left))))
      (cond
        (tname (table-col tname tcol (or tpart :data)))   ; "Sales[Amount]" / "[@Amount]"
        (span  (read-span *sheet* span))                  ; "A:A" / "1:1"
        (t (multiple-value-bind (target r0 r1 c0 c1) (resolve-range top-left bottom-right)
             (let ((out '()))
               (loop for r from r0 to r1 do
                 (loop for c from c0 to c1
                       for ref = (make-ref r c)
                       do (push (read-cell-blank target ref) out)))
               (nreverse out))))))))

;;; --- whole-column / whole-row reads (spans) -------------------------
;;;
;;; A span read — (col "A"), (row 5), or the colon form (cells "A:A") — depends
;;; on the column/row as a WHOLE, recording one coarse span dependency
;;; (NOTE-RANGE) instead of an edge per cell. See cell.lisp SPAN and the
;;; sheet COL-WATCHERS / ROW-WATCHERS index.

(defun %parse-span (designator)
  "Parse a whole-column/row span designator into a SPAN, or NIL when DESIGNATOR
is not a colon span (so callers fall back to ordinary ref/range parsing). A
column band is letters:letters (\"A:A\", \"A:C\"), a row band is digits:digits
(\"1:1\", \"2:5\"); a mixed form (\"A:2\") is not a span. Bounds normalize lo<=hi."
  (when (typep designator '(or string symbol))
    (let* ((s (string designator))
           (colon (position #\: s)))
      (when colon
        (let ((left (subseq s 0 colon))
              (right (subseq s (1+ colon))))
          (flet ((lettersp (x) (and (plusp (length x)) (every #'alpha-char-p x)))
                 (digitsp  (x) (and (plusp (length x)) (every #'digit-char-p x))))
            (cond
              ((and (lettersp left) (lettersp right))
               (let ((a (col-letters->index left 0 (length left)))
                     (b (col-letters->index right 0 (length right))))
                 (make-span :col (min a b) (max a b))))
              ((and (digitsp left) (digitsp right))
               (let ((a (1- (parse-integer left)))
                     (b (1- (parse-integer right))))
                 (when (or (minusp a) (minusp b))
                   (error 'bad-reference :format-control "Row must be >= 1 in ~S"
                                         :format-arguments (list s)))
                 (make-span :row (min a b) (max a b))))
              (t nil))))))))

(defun read-cell-raw (sheet ref)
  "Force REF up to date and return its value WITHOUT recording a per-cell
precedent — READ-SPAN records a coarse span dependency instead. An existing
cell's stored error re-signals; an empty cell returns NIL."
  (if (find-cell sheet ref) (evaluate-ref sheet ref) nil))

(defun read-span (sheet span &optional tolerant)
  "Read SPAN (a whole column/row) as a flat row-major list of the populated
cells' values, recording ONE coarse dependency on the span (NOTE-RANGE) rather
than a per-cell precedent. Enumeration is bounded by the sheet's used range;
empty cells are skipped. Existing errors propagate unless TOLERANT (then the
errored cell is skipped, like SAFE-CELLS)."
  (note-range span)
  (let ((ur (used-range sheet)))
    (when ur
      (destructuring-bind ((minr . minc) maxr . maxc) ur
        (let ((out '()))
          (flet ((rd (ref) (if tolerant
                               (ignore-errors (read-cell-raw sheet ref))
                               (read-cell-raw sheet ref))))
            (ecase (span-axis span)
              (:col
               (let ((c0 (max minc (span-lo span))) (c1 (min maxc (span-hi span))))
                 (loop for r from minr to maxr do
                   (loop for c from c0 to c1
                         for v = (rd (make-ref r c))
                         do (when v (push v out))))))
              (:row
               (let ((r0 (max minr (span-lo span))) (r1 (min maxr (span-hi span))))
                 (loop for r from r0 to r1 do
                   (loop for c from minc to maxc
                         for v = (rd (make-ref r c))
                         do (when v (push v out))))))))
          (nreverse out))))))

(defun col (designator &optional to)
  "Read a whole column (or band of columns) as a flat list of the populated
cells' values, top to bottom. DESIGNATOR/TO are column letters: (col \"A\") is
column A; (col \"A\" \"C\") is columns A..C. Records ONE whole-column dependency,
not one per cell — so a change anywhere in the column, now or later, re-fires
this formula."
  (flet ((idx (x) (let ((s (string x))) (col-letters->index s 0 (length s)))))
    (let* ((a (idx designator)) (b (and to (idx to)))
           (lo (if b (min a b) a)) (hi (if b (max a b) a)))
      (read-span *sheet* (make-span :col lo hi)))))

(defun row (designator &optional to)
  "Read a whole row (or band of rows) as a flat list of the populated cells'
values, left to right. DESIGNATOR/TO are 1-based row numbers (an integer or
numeric string): (row 5) is row 5; (row 2 5) is rows 2..5. Like COL, records one
whole-row dependency rather than an edge per cell.
NOTE: this is CELLISP:ROW; REVISION:ROW is an unrelated UI layout macro. Formulas
are read in the cellisp package, so there is no clash."
  (flet ((idx (x) (1- (if (integerp x) x (parse-integer (string x))))))
    (let* ((a (idx designator)) (b (and to (idx to)))
           (lo (if b (min a b) a)) (hi (if b (max a b) a)))
      (when (minusp lo)
        (error 'bad-reference :format-control "Row must be >= 1"))
      (read-span *sheet* (make-span :row lo hi)))))

;;; --- structured table references (Sales[Amount] / table-col) --------
;;;
;;; A table-column read bounds a single column to the table's DATA rows (header
;;; and totals excluded) but records the SAME coarse :col span dependency as COL
;;; — so it reuses the whole watcher/recompute machinery and re-fires when the
;;; column changes or grows. Column is resolved by header text.

(defun read-column-cells (sheet col r0 r1 &optional tolerant)
  "The populated values of column COL over rows R0..R1 (inclusive), top to bottom,
blanks skipped. Records NO per-cell precedent — the caller records the coarse span
dependency. Existing errors propagate unless TOLERANT."
  (let ((out '()))
    (loop for r from r0 to r1
          for v = (if tolerant
                      (ignore-errors (read-cell-raw sheet (make-ref r col)))
                      (read-cell-raw sheet (make-ref r col)))
          do (when v (push v out)))
    (nreverse out)))

(defun %table-header-value (sheet table col)
  "TABLE's header-cell value in column COL — forced up to date (it may be
uncomputed mid-sweep) but recording NO dependency; column resolution is
structural, like a name lookup. NIL for an empty/errored header."
  (let ((hrow (ref-row (car (table-region table)))))
    (and (find-cell sheet (make-ref hrow col))
         (ignore-errors (evaluate-ref sheet (make-ref hrow col))))))

(defun %table-col-index (sheet table header)
  "The column index within TABLE whose header cell equals HEADER (case-insensitive),
or NIL."
  (let* ((region (table-region table))
         (c0 (ref-col (car region))) (c1 (ref-col (cdr region)))
         (want (string-downcase (string header))))
    (loop for c from c0 to c1
          for v = (%table-header-value sheet table c)
          when (and (typep v '(or string symbol))
                    (string= want (string-downcase (string v))))
            return c)))

(defun %table-single-cell (sheet table col which)
  "Read the :HEADERS or :TOTALS cell of TABLE's column COL as a scalar, recording a
fine-grained single-cell dependency. Signals if WHICH is :TOTALS and the table has
no totals row."
  (let* ((region (table-region table))
         (row (ecase which
                (:headers (ref-row (car region)))
                (:totals
                 (unless (table-totals-p table)
                   (error 'unknown-name
                          :format-control "Table ~S has no totals row"
                          :format-arguments (list (table-name table))))
                 (ref-row (cdr region))))))
    (read-cell-blank sheet (make-ref row col))))

(defun %table-this-row (sheet table col)
  "Read TABLE's column COL cell on the COMPUTING cell's row — the @ reference used
in a calculated column — as a scalar, with a single-cell dependency. Signals if
there is no computing row or it is outside the table's data rows."
  (let ((here (car *eval-stack*)))               ; the cell being computed
    (unless here
      (error 'unknown-name :format-control "@-reference outside a formula"))
    (let ((r (ref-row here)) (rows (%table-data-rows table)))
      (unless (and rows (<= (car rows) r (cdr rows)))
        (error 'unknown-name
               :format-control "@-reference outside table ~S data rows"
               :format-arguments (list (table-name table))))
      (read-cell-blank sheet (make-ref r col)))))

(defun table-col (name column &optional (part :data))
  "Read table NAME's COLUMN (by header text). PART selects which rows:
  :DATA (default) — the column's data rows as a list (one coarse whole-column
    dependency, like COL, re-firing when the data changes or grows);
  :THIS-ROW — the single cell in COLUMN on the computing cell's row (Sales[@Amount]);
  :TOTALS / :HEADERS — the totals/header cell of COLUMN (a scalar).
Signals UNKNOWN-NAME (=> #NAME?) if the table or header column is unknown."
  (let* ((sheet *sheet*)
         (table (gethash (%name-key name) (sheet-tables sheet))))
    (unless table
      (error 'unknown-name :format-control "No table named ~S"
                           :format-arguments (list name)))
    (let ((col (%table-col-index sheet table column)))
      (unless col
        (error 'unknown-name :format-control "Table ~S has no column ~S"
                             :format-arguments (list name column)))
      (ecase part
        (:data
         ;; coarse dep on the whole physical column; the table's row-bound is
         ;; applied only at read time (a change in the column outside the table
         ;; re-fires but recomputes to the same value).
         (note-range (make-span :col col col))
         (let ((rows (%table-data-rows table)))
           (if rows (read-column-cells sheet col (car rows) (cdr rows)) '())))
        (:this-row (%table-this-row sheet table col))
        (:totals   (%table-single-cell sheet table col :totals))
        (:headers  (%table-single-cell sheet table col :headers))))))

(defun %parse-structured-ref (designator)
  "Parse a structured table reference into (values name column part): \"Name[Column]\"
=> PART :DATA; \"Name[@Column]\" => :THIS-ROW (the computing row's cell). NIL when
DESIGNATOR is not a structured ref, so CELLS falls back to span / range parsing."
  (when (typep designator '(or string symbol))
    (let* ((s (string designator))
           (lb (position #\[ s))
           (len (length s)))
      (when (and lb (plusp lb) (plusp len) (char= (char s (1- len)) #\]))
        (let ((name (subseq s 0 lb))
              (inner (subseq s (1+ lb) (1- len))))
          (when (and (plusp (length name)) (plusp (length inner)))
            (if (char= (char inner 0) #\@)
                (when (> (length inner) 1) (values name (subseq inner 1) :this-row))
                (values name inner :data))))))))

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
        (error 'numeric-error
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
        (*collected-ranges* (make-hash-table :test 'equal))
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
      ;; commit whole-column/row (span) links into the watcher reverse index.
      (update-range-links sheet ref cell *collected-ranges*)
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
        ;; mark this cell's column and row changed so a whole-column/row reader
        ;; (A:A) of them recomputes — the coarse counterpart of *CHANGED*.
        (when *changed-cols* (setf (gethash (ref-col ref) *changed-cols*) t))
        (when *changed-rows* (setf (gethash (ref-row ref) *changed-rows*) t))
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

(defun update-range-links (sheet ref cell new-ranges-table)
  "Reconcile CELL's whole-column/row (SPAN) precedents against NEW-RANGES-TABLE,
mirroring UPDATE-DEPENDENCY-LINKS: drop REF from the watcher sets of spans no
longer read, add it to those now read, and store the new span list on the cell.
A span touches one watcher entry per column/row it spans — never per cell."
  (let ((new-spans (loop for s being the hash-keys of new-ranges-table collect s)))
    (flet ((each-line (span fn)
             (loop for i from (span-lo span) to (span-hi span)
                   do (funcall fn (span-axis span) i))))
      ;; drop watcher entries for spans this cell no longer reads
      (dolist (old (cell-range-precedents cell))
        (unless (gethash old new-ranges-table)
          (each-line old (lambda (axis i) (remove-watcher sheet axis i ref)))))
      ;; register (idempotent) the spans it now reads
      (dolist (s new-spans)
        (each-line s (lambda (axis i) (add-watcher sheet axis i ref)))))
    (setf (cell-range-precedents cell) new-spans)))

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
