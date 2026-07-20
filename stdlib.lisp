(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Formula standard library
;;;;
;;;; Helpers callable from a formula, on top of CELL/CELLS and the SUM/AVERAGE/
;;;; CNT aggregates in eval.lisp. A formula is arbitrary Lisp, so CL's own MIN,
;;;; MAX, IF, AND, … are already available on explicit arguments; these add the
;;;; *spreadsheet* conveniences CL lacks — aggregates that ignore blanks/text
;;;; over a range, predicate-filtered aggregates, 2D range access, lookups, and
;;;; IFERROR. The numeric aggregates reuse FLATTEN-NUMBERS, so they accept loose
;;;; values, nested lists, and the list a CELLS range returns, all the same way.
;;;; ------------------------------------------------------------------

(defun flatten-values (args)
  "Every non-NIL value in ARGS, descending into nested lists (keeping non-numbers,
unlike FLATTEN-NUMBERS). Blanks (NIL) are dropped."
  (loop for a in args
        append (cond ((null a) '())
                     ((listp a) (flatten-values a))
                     (t (list a)))))

;;; --- numeric aggregates (ignore non-numbers) ------------------------

(defun minimum (&rest args)
  "Least number in ARGS (non-numbers ignored). SHEET-ERROR if there are none."
  (let ((ns (flatten-numbers args)))
    (if ns (reduce #'min ns)
        (error 'sheet-error :format-control "MINIMUM of no numeric values"))))

(defun maximum (&rest args)
  "Greatest number in ARGS (non-numbers ignored). SHEET-ERROR if there are none."
  (let ((ns (flatten-numbers args)))
    (if ns (reduce #'max ns)
        (error 'sheet-error :format-control "MAXIMUM of no numeric values"))))

(defun product (&rest args)
  "Product of the numbers in ARGS (non-numbers ignored). Empty -> 1."
  (reduce #'* (flatten-numbers args) :initial-value 1))

(defun median (&rest args)
  "Median of the numbers in ARGS (non-numbers ignored). SHEET-ERROR if none."
  (let* ((ns (sort (copy-list (flatten-numbers args)) #'<))
         (n (length ns)))
    (cond ((null ns)
           (error 'sheet-error :format-control "MEDIAN of no numeric values"))
          ((oddp n) (nth (floor n 2) ns))
          (t (/ (+ (nth (1- (floor n 2)) ns) (nth (floor n 2) ns)) 2)))))

;;; --- predicate-filtered aggregates ----------------------------------

(defun countif (predicate &rest args)
  "Count the values in ARGS (blanks dropped) that satisfy PREDICATE. Because a
range may mix numbers and text, PREDICATE is applied defensively: a value it
errors on (e.g. PLUSP of a string) simply doesn't match, rather than aborting."
  (count-if (lambda (v) (ignore-errors (funcall predicate v)))
            (flatten-values args)))

(defun sumif (predicate &rest args)
  "Sum of the numbers in ARGS that satisfy PREDICATE (non-numbers ignored)."
  (reduce #'+ (remove-if-not predicate (flatten-numbers args)) :initial-value 0))

(defun averageif (predicate &rest args)
  "Mean of the numbers in ARGS satisfying PREDICATE. SHEET-ERROR if none match."
  (let ((ns (remove-if-not predicate (flatten-numbers args))))
    (if ns (/ (reduce #'+ ns) (length ns))
        (error 'sheet-error :format-control "AVERAGEIF of no matching numbers"))))

;;; --- 2D range access ------------------------------------------------

(defun grid (top-left &optional bottom-right)
  "Like CELLS, but preserves the rectangle's 2D shape: a list of row lists (each
inner list a row, left to right). Accepts the same designators as CELLS,
including a sheet qualifier and range names, and records the same dependencies."
  (multiple-value-bind (target r0 r1 c0 c1) (resolve-range top-left bottom-right)
    (loop for r from r0 to r1
          collect (loop for c from c0 to c1
                        collect (read-cell-value target (make-ref r c))))))

;;; --- lookups --------------------------------------------------------

(defun match (key sequence &optional (test #'equal))
  "1-based position of KEY in SEQUENCE (spreadsheet MATCH), or NIL if absent."
  (let ((p (position key sequence :test test)))
    (and p (1+ p))))

(defun lookup (key keys values &optional default (test #'equal))
  "Find KEY among KEYS and return the value at the same position in VALUES;
DEFAULT (NIL) if KEY is absent. KEYS/VALUES are parallel lists (e.g. two CELLS
ranges)."
  (loop for k in keys
        for v in values
        when (funcall test k key) do (return v)
        finally (return default)))

(defun vlookup (key table column &optional default (test #'equal))
  "TABLE is a list of rows (from GRID). Return the COLUMN-th (1-based) element of
the first row whose first element matches KEY; DEFAULT if no row matches."
  (loop for row in table
        when (funcall test (first row) key)
          do (return (nth (1- column) row))
        finally (return default)))

(defun hlookup (key table row &optional default (test #'equal))
  "TABLE is a list of rows (from GRID). Match KEY across the first row; return the
ROW-th (1-based) element of that column; DEFAULT if no column matches."
  (let ((pos (position key (first table) :test test)))
    (if pos (nth pos (nth (1- row) table)) default)))

;;; --- logic / blanks -------------------------------------------------

(defun blankp (value)
  "True if VALUE is blank — the NIL an empty cell reads as."
  (null value))

(defmacro iferror (form &optional default)
  "Evaluate FORM; if it signals any error (a division by zero, a read of an empty
cell, a cycle, …) yield DEFAULT instead. The cells FORM touches before failing
are still recorded as precedents, so recovery re-triggers when they change."
  `(handler-case ,form (error () ,default)))
