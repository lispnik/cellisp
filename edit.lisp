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

(defun shift-name-table (table shift-fn)
  "A copy of the name -> ref TABLE with each *value* (ref) mapped through
SHIFT-FN (dropping names whose target was deleted)."
  (let ((new (make-hash-table :test 'equal)))
    (maphash (lambda (name ref)
               (let ((nref (funcall shift-fn ref)))
                 (unless (eq nref :deleted) (setf (gethash name new) nref))))
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
            (sheet-names sheet)     (shift-name-table (sheet-names sheet) shift-fn))
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
