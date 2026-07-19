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

(defun shift-ref-literal (lit shift-fn)
  "Map the literal ref designator LIT through SHIFT-FN, returning a ref STRING
(or \"#REF!\" when the target was deleted)."
  (let* ((designator (if (consp lit) (second lit) lit))   ; unwrap (quote sym)
         (new (funcall shift-fn (parse-ref designator))))
    (if (eq new :deleted) "#REF!" (ref-string new))))

(defun rewrite-formula-refs (form shift-fn)
  "Return FORM with the literal ref args of CELL/CELLS forms mapped through
SHIFT-FN; everything else (including computed refs) is left unchanged."
  (cond
    ((atom form) form)
    ((and (eq (car form) 'cell) (= (length form) 2) (ref-literal-p (second form)))
     (list 'cell (shift-ref-literal (second form) shift-fn)))
    ((and (eq (car form) 'cells) (= (length form) 3)
          (ref-literal-p (second form)) (ref-literal-p (third form)))
     (list 'cells (shift-ref-literal (second form) shift-fn)
                  (shift-ref-literal (third form) shift-fn)))
    (t (mapcar (lambda (x) (rewrite-formula-refs x shift-fn)) form))))

(defun shift-registry (table shift-fn)
  "A copy of the ref-keyed TABLE with keys mapped through SHIFT-FN (dropping
deleted refs)."
  (let ((new (make-hash-table :test 'equal)))
    (maphash (lambda (ref v)
               (let ((nref (funcall shift-fn ref)))
                 (unless (eq nref :deleted) (setf (gethash nref new) v))))
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
                           (rewrite-formula-refs (cell-formula cell) shift-fn)
                           ;; links are stale after the move; rebuilt by recalc
                           (cell-precedents cell) '()
                           (cell-dependents cell) '())
                     (setf (gethash nref new) cell))))
               (sheet-cells sheet))
      (setf (sheet-cells sheet) new
            (sheet-volatiles sheet) (shift-registry (sheet-volatiles sheet) shift-fn)
            (sheet-frozen sheet)    (shift-registry (sheet-frozen sheet) shift-fn))
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
