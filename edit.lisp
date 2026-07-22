(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Structural editing — insert/delete rows and columns
;;;;
;;;; Inserting or deleting a row/column moves cells to new positions and
;;;; rewrites the *static* references in every formula so they keep pointing at
;;;; the same data. A reference that is computed at runtime (a non-literal arg
;;;; to CELL/CELLS) can't be rewritten and is left as-is. A reference to a
;;;; deleted cell becomes "#REF!", which errors when evaluated — like a
;;;; spreadsheet's #REF!. Dependency links are rebuilt afterwards by RECALC-ALL.
;;;; ------------------------------------------------------------------

(defun ref-literal-p (x)
  "True if X is a literal ref designator inside a formula: a string, or a
quoted symbol (QUOTE SYM)."
  (or (stringp x)
      (and (consp x) (eq (car x) 'quote) (symbolp (second x)))))

(defun literal->string (lit)
  "The ref-string form of a literal ref designator (unwrapping (QUOTE SYM))."
  (cond ((stringp lit) lit)
        ((consp lit) (string (second lit)))
        (t (string lit))))

(defun parse-ref-parts (s)
  "Parse a ref string S (which may carry $ absolute markers) into
(values col-absolute-p col-index row-absolute-p row-index)."
  (let ((i 0) (len (length s)) (col-abs nil) (row-abs nil))
    (when (and (< i len) (char= (char s i) #\$)) (setf col-abs t) (incf i))
    (let ((start i))
      (loop while (and (< i len) (alpha-char-p (char s i))) do (incf i))
      (let ((col (col-letters->index s start i)))
        (when (and (< i len) (char= (char s i) #\$)) (setf row-abs t) (incf i))
        (values col-abs col row-abs (1- (parse-integer s :start i)))))))

(defun render-ref (col-abs col row-abs row)
  "Render an A1 ref string with $ markers, e.g. (render-ref t 0 nil 4) => \"$A5\"."
  (format nil "~:[~;$~]~A~:[~;$~]~D"
          col-abs (index->col-letters col) row-abs (1+ row)))

(defun map-formula-refs (form fn)
  "Return FORM with each literal ref designator of CELL/CELLS forms replaced by
(funcall FN ref-string), a new ref string. Computed refs are left as-is."
  (labels ((rw (form)
             (cond
               ((atom form) form)
               ((and (eq (car form) 'cell) (= (length form) 2)
                     (ref-literal-p (second form)))
                (list 'cell (funcall fn (literal->string (second form)))))
               ((and (eq (car form) 'cells) (= (length form) 3)
                     (ref-literal-p (second form)) (ref-literal-p (third form)))
                (list 'cells (funcall fn (literal->string (second form)))
                             (funcall fn (literal->string (third form)))))
               (t (mapcar #'rw form)))))
    (rw form)))

(defun shift-ref-string (s shift-fn)
  "Apply SHIFT-FN (ref-cons -> ref-or-:deleted) to ref string S, preserving its
$ markers. :deleted -> \"#REF!\"; an already-unparseable S is left unchanged."
  (handler-case
      (multiple-value-bind (col-abs col row-abs row) (parse-ref-parts s)
        (let ((new (funcall shift-fn (make-ref row col))))
          (if (eq new :deleted) "#REF!"
              (render-ref col-abs (ref-col new) row-abs (ref-row new)))))
    (error () s)))

(defun shift-registry (table shift-fn)
  "A copy of the ref-keyed TABLE with keys mapped through SHIFT-FN (dropping
deleted refs)."
  (let ((new (make-hash-table :test 'equal)))
    (maphash (lambda (ref v)
               (let ((nref (funcall shift-fn ref)))
                 (unless (eq nref :deleted) (setf (gethash nref new) v))))
             table)
    new))

(defun shift-merges (merges shift-fn)
  "A copy of the MERGES list ((tl . br) rects) with both corners shifted; a merge
whose top-left or bottom-right lands on a deleted line is dropped."
  (loop for (a . b) in merges
        for na = (funcall shift-fn a)
        for nb = (funcall shift-fn b)
        unless (or (eq na :deleted) (eq nb :deleted))
          collect (cons na nb)))

(defun shift-name-table (table shift-fn)
  "A copy of the names TABLE with every target mapped through SHIFT-FN. A single
-cell name drops if its cell was deleted; a range name (a (tl . br) cons) drops
if either corner was deleted, else keeps its shifted corners."
  (let ((new (make-hash-table :test 'equal)))
    (maphash (lambda (name val)
               (if (%range-value-p val)
                   (let ((tl (funcall shift-fn (car val)))
                         (br (funcall shift-fn (cdr val))))
                     (unless (or (eq tl :deleted) (eq br :deleted))
                       (setf (gethash name new) (cons tl br))))
                   (let ((nref (funcall shift-fn val)))
                     (unless (eq nref :deleted) (setf (gethash name new) nref)))))
             table)
    new))

(defun shift-spills (table shift-fn)
  "Shift a spills registry (anchor-ref -> (rows . cols)) under a structural edit.
Unlike a plain registry shift, the EXTENT is recomputed, not just moved: a row or
column inserted or deleted *inside* a spill changes the rectangle its cells
occupy, so the recorded (rows . cols) must track the new bounding box or RESPILL
would clear the wrong region and orphan a displaced cell. Each spill's cells are
run through SHIFT-FN and the extent is set to their bounding box relative to the
shifted anchor. A spill whose anchor is deleted is dropped."
  (let ((new (make-hash-table :test 'equal)))
    (maphash
     (lambda (anchor extent)
       (let ((nanchor (funcall shift-fn anchor)))
         (unless (eq nanchor :deleted)
           (let ((r0 (ref-row anchor)) (c0 (ref-col anchor))
                 (maxr nil) (maxc nil))
             (loop for i from 0 below (car extent) do
               (loop for j from 0 below (cdr extent)
                     for nref = (funcall shift-fn (make-ref (+ r0 i) (+ c0 j)))
                     unless (eq nref :deleted) do
                       (when (or (null maxr) (> (ref-row nref) maxr))
                         (setf maxr (ref-row nref)))
                       (when (or (null maxc) (> (ref-col nref) maxc))
                         (setf maxc (ref-col nref)))))
             ;; the anchor stays the top-left of the block under any single
             ;; row/column insert or delete, so extent = (maxrow,maxcol) relative
             ;; to the shifted anchor.
             (when maxr
               (setf (gethash nanchor new)
                     (cons (1+ (- maxr (ref-row nanchor)))
                           (1+ (- maxc (ref-col nanchor))))))))))
     table)
    new))

(defun structural-edit (sheet shift-fn)
  "Apply SHIFT-FN (a ref -> ref-or-:deleted map) to SHEET: rewrite static refs
in every formula, move cells to their shifted keys (dropping deleted ones),
shift the volatile/frozen registries, then rebuild the dependency graph and
values via RECALC-ALL."
  (with-sheet-lock (sheet)
    (let ((new (make-hash-table :test 'equal)))
      (maphash (lambda (ref cell)
                 (let ((nref (funcall shift-fn ref)))
                   (unless (eq nref :deleted)
                     (setf (cell-formula cell)
                           (map-formula-refs (cell-formula cell)
                                             (lambda (s) (shift-ref-string s shift-fn)))
                           ;; links are stale after the move; rebuilt by recalc
                           (cell-precedents cell) '()
                           (cell-dependents cell) '())
                     (setf (gethash nref new) cell))))
               (sheet-cells sheet))
      (setf (sheet-cells sheet) new
            (sheet-volatiles sheet) (shift-registry (sheet-volatiles sheet) shift-fn)
            (sheet-frozen sheet)    (shift-registry (sheet-frozen sheet) shift-fn)
            ;; named aliases follow their target cell (dropped if it's deleted)
            (sheet-names sheet)     (shift-name-table (sheet-names sheet) shift-fn)
            ;; notes follow their cell too (dropped if it's deleted)
            (sheet-notes sheet)     (shift-registry (sheet-notes sheet) shift-fn)
            ;; merges shift both corners; a merge whose edge is deleted is dropped
            (sheet-merges sheet)    (shift-merges (sheet-merges sheet) shift-fn)
            ;; spill anchors follow their cell AND the extent is recomputed, so a
            ;; row/column change inside a spill doesn't leave RESPILL clearing the
            ;; wrong rectangle (dropped if the anchor is deleted)
            (sheet-spills sheet)    (shift-spills (sheet-spills sheet) shift-fn))
      (recalc-all sheet)))
  (values))

(defun insert-row (sheet row)
  "Insert a blank row before 1-based ROW; cells at or below shift down one and
references adjust."
  (let ((r (1- row)))
    (structural-edit sheet
      (lambda (ref) (if (>= (ref-row ref) r)
                        (make-ref (1+ (ref-row ref)) (ref-col ref))
                        ref)))))

(defun delete-row (sheet row)
  "Delete 1-based ROW; cells below shift up one, and references to the deleted
row become #REF!."
  (let ((r (1- row)))
    (structural-edit sheet
      (lambda (ref)
        (let ((rr (ref-row ref)))
          (cond ((= rr r) :deleted)
                ((> rr r) (make-ref (1- rr) (ref-col ref)))
                (t ref)))))))

(defun insert-column (sheet col)
  "Insert a blank column before 1-based COL (column A is 1); cells at or to the
right shift over one and references adjust."
  (let ((c (1- col)))
    (structural-edit sheet
      (lambda (ref) (if (>= (ref-col ref) c)
                        (make-ref (ref-row ref) (1+ (ref-col ref)))
                        ref)))))

(defun delete-column (sheet col)
  "Delete 1-based COL; cells to the right shift left one, and references to the
deleted column become #REF!."
  (let ((c (1- col)))
    (structural-edit sheet
      (lambda (ref)
        (let ((cc (ref-col ref)))
          (cond ((= cc c) :deleted)
                ((> cc c) (make-ref (ref-row ref) (1- cc)))
                (t ref)))))))

;;;; ------------------------------------------------------------------
;;;; Copy / paste with relative and absolute ($) references
;;;;
;;;; Copying a formula shifts its *relative* references by the source→dest
;;;; offset; parts marked absolute with $ ($A$1, $A1, A$1) stay fixed. A
;;;; reference shifted off the grid becomes "#REF!".
;;;; ------------------------------------------------------------------

(defun copy-shift-ref (s drow dcol)
  "Shift the RELATIVE parts of ref string S by (DROW, DCOL); absolute ($) parts
stay. Off-grid -> \"#REF!\"; an unparseable S is left unchanged."
  (handler-case
      (multiple-value-bind (col-abs col row-abs row) (parse-ref-parts s)
        (let ((nc (if col-abs col (+ col dcol)))
              (nr (if row-abs row (+ row drow))))
          (if (or (minusp nc) (minusp nr)) "#REF!"
              (render-ref col-abs nc row-abs nr))))
    (error () s)))

(defun copy-cell (sheet src dst)
  "Copy SRC's formula into DST, shifting relative references by the SRC->DST
offset while absolute ($) references stay fixed. Overwrites DST."
  (with-sheet-lock (sheet)
    (let* ((sref (parse-ref src)) (dref (parse-ref dst))
           (drow (- (ref-row dref) (ref-row sref)))
           (dcol (- (ref-col dref) (ref-col sref))))
      (set-cell sheet dst
                (map-formula-refs (get-formula sheet src)
                                  (lambda (s) (copy-shift-ref s drow dcol)))))))

(defun fill-range (sheet src top-left bottom-right)
  "Copy SRC's formula into every cell of the TOP-LEFT..BOTTOM-RIGHT rectangle,
each with its own relative-reference adjustment (a spreadsheet fill). Installs
the whole rectangle in one recompute sweep."
  (with-sheet-lock (sheet)
    (let* ((formula (get-formula sheet src))
           (sref (parse-ref src))
           (a (parse-ref top-left)) (b (parse-ref bottom-right))
           (r0 (min (ref-row a) (ref-row b))) (r1 (max (ref-row a) (ref-row b)))
           (c0 (min (ref-col a) (ref-col b))) (c1 (max (ref-col a) (ref-col b)))
           (bindings '()))
      (loop for r from r0 to r1 do
        (loop for c from c0 to c1
              for drow = (- r (ref-row sref))
              for dcol = (- c (ref-col sref))
              do (push (list (make-ref r c)
                             (map-formula-refs formula
                               (lambda (s) (copy-shift-ref s drow dcol))))
                       bindings)))
      (set-cells sheet (nreverse bindings)))
    (values)))

;;;; ------------------------------------------------------------------
;;;; Spill — populate a rectangle from an array-valued formula
;;;; ------------------------------------------------------------------

(defun eval-in-sheet (sheet formula)
  "Evaluate FORMULA once in SHEET's context (so CELL/CELLS resolve), to learn
an array formula's shape. Uses a throwaway cell; records no dependencies."
  (let ((*sheet* sheet))
    (eval-formula sheet (make-instance 'cell :formula formula))))

(defun spill (sheet anchor formula)
  "Evaluate FORMULA — a list (a column) or a list of lists (a 2D block) — and
populate cells starting at ANCHOR, one per element. Each spilled cell indexes
FORMULA, so the block tracks FORMULA's inputs (wrap an expensive array in a
shared cached cell to avoid recomputing it per element). The shape is fixed at
spill time; use RESPILL to re-fill when it changes and clear a shrunk block.
Returns (rows . cols), and records the extent at ANCHOR for RESPILL."
  (with-sheet-lock (sheet)
    (let* ((aref (parse-ref anchor))
           (arr (eval-in-sheet sheet formula))
           (row0 (ref-row aref)) (col0 (ref-col aref))
           (extent
             (cond
               ((not (listp arr))                ; scalar: a plain single cell
                (set-cell sheet anchor formula)
                (cons 1 1))
               ((and arr (listp (first arr)))    ; 2D: list of rows
                (let ((bindings '()))
                  (loop for i from 0 below (length arr) do
                    (loop for j from 0 below (length (nth i arr)) do
                      (push (list (make-ref (+ row0 i) (+ col0 j))
                                  `(nth ,j (nth ,i ,formula)))
                            bindings)))
                  (set-cells sheet (nreverse bindings))
                  (cons (length arr) (length (first arr)))))
               (t                                 ; 1D: a column
                (let ((bindings '()))
                  (loop for i from 0 below (length arr) do
                    (push (list (make-ref (+ row0 i) col0) `(nth ,i ,formula))
                          bindings))
                  (set-cells sheet (nreverse bindings))
                  (cons (length arr) 1))))))
      (setf (gethash aref (sheet-spills sheet)) extent)
      extent)))

(defun respill (sheet anchor formula)
  "Like SPILL, but first clear the rectangle the previous SPILL/RESPILL wrote at
ANCHOR — so a result with fewer rows or columns leaves no leftover cells. Use it
whenever a spill's size can change (a refreshed data feed, a filtered range).
Returns the new (rows . cols)."
  (with-sheet-lock (sheet)
    (let* ((aref (parse-ref anchor))
           (old (gethash aref (sheet-spills sheet))))
      (when old
        (loop for i from 0 below (car old) do
          (loop for j from 0 below (cdr old)
                for r = (make-ref (+ (ref-row aref) i) (+ (ref-col aref) j))
                when (find-cell sheet r) do (clear-cell sheet r))))
      (spill sheet anchor formula))))
