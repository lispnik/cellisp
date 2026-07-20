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

;;; --- tolerant range reading -----------------------------------------

(defun safe-cells (top-left &optional bottom-right)
  "Like CELLS but tolerant: any cell in the rectangle that is empty or holds an
error contributes nothing (is omitted) instead of aborting the read. So an
aggregate over a sparse or partly-broken range works — (sum (safe-cells \"A1\"
\"A100\")) sums whatever numbers are there. Every cell's dependency is still
recorded, so filling or fixing one re-triggers recompute."
  (multiple-value-bind (target r0 r1 c0 c1) (resolve-range top-left bottom-right)
    (let ((out '()))
      (loop for r from r0 to r1 do
        (loop for c from c0 to c1
              for ref = (make-ref r c)
              ;; read-cell-value records the precedent before it can signal, so
              ;; the dependency stands even when the read errors or is blank.
              for v = (ignore-errors (read-cell-value target ref))
              do (when v (push v out))))
      (nreverse out))))

;;; --- sort / filter / unique (spreadsheet dynamic-array helpers) ------

(defun generic-lessp (a b)
  "A total order usable as a default sort predicate: numbers numerically, strings
lexicographically, anything else by its printed form."
  (cond ((and (realp a) (realp b)) (< a b))
        ((and (stringp a) (stringp b)) (and (string< a b) t))
        (t (and (string< (princ-to-string a) (princ-to-string b)) t))))

(defun sortv (values &optional (predicate #'generic-lessp) (key #'identity))
  "A sorted copy of VALUES (spreadsheet SORT). Default order handles numbers and
strings; pass PREDICATE/KEY to customize. Non-destructive."
  (sort (copy-list values) predicate :key key))

(defun filterv (predicate values)
  "The elements of VALUES satisfying PREDICATE (spreadsheet FILTER)."
  (remove-if-not predicate values))

(defun uniquev (values &optional (test #'equal))
  "VALUES with duplicates removed, first occurrence kept (spreadsheet UNIQUE)."
  (remove-duplicates values :test test :from-end t))

;;; --- 2D range access ------------------------------------------------

(defun grid (top-left &optional bottom-right)
  "Like CELLS, but preserves the rectangle's 2D shape: a list of row lists (each
inner list a row, left to right). Accepts the same designators as CELLS,
including a sheet qualifier and range names, and records the same dependencies.
Empty cells read as NIL (blank), keeping the rectangle's shape."
  (multiple-value-bind (target r0 r1 c0 c1) (resolve-range top-left bottom-right)
    (loop for r from r0 to r1
          collect (loop for c from c0 to c1
                        collect (read-cell-blank target (make-ref r c))))))

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

;;; --- text -----------------------------------------------------------

(defun as-text (x)
  "X coerced to a string: strings verbatim, NIL to \"\", anything else printed."
  (cond ((null x) "") ((stringp x) x) (t (princ-to-string x))))

(defun to-number (x &optional default)
  "Coerce X to a number: a number is returned as-is; a string that parses
*entirely* as a number (integer, ratio, float, or exponent) becomes that number;
anything else — non-numeric text, NIL, a partial match like \"3 apples\" — returns
DEFAULT (NIL). Reads with *READ-EVAL* off and only accepts a number, so it never
evaluates or interns a symbol. Handy for cleaning imported/text data, e.g.
(to-number (cell \"A1\") 0)."
  (cond
    ((numberp x) x)
    ((stringp x)
     (let ((s (string-trim '(#\Space #\Tab #\Return #\Newline) x)))
       (if (and (plusp (length s))
                (let ((c (char s 0)))       ; only bother for number-shaped text
                  (or (digit-char-p c) (member c '(#\- #\+ #\.)))))
           (multiple-value-bind (v pos)
               (let ((*read-eval* nil)) (ignore-errors (read-from-string s nil nil)))
             (if (and (numberp v) (eql pos (length s))) v default))
           default)))
    (t default)))

(defun concat (&rest args)
  "Concatenate ARGS as text (spreadsheet CONCATENATE / &), flattening ranges and
dropping blanks."
  (with-output-to-string (out)
    (labels ((emit (x) (cond ((null x))
                             ((listp x) (mapc #'emit x))
                             (t (write-string (as-text x) out)))))
      (mapc #'emit args))))

(defun text-length (x) "Character count of X as text (spreadsheet LEN)."
  (length (as-text x)))

(defun left (x n) "The first N characters of X as text (spreadsheet LEFT)."
  (let ((s (as-text x))) (subseq s 0 (min n (length s)))))

(defun right (x n) "The last N characters of X as text (spreadsheet RIGHT)."
  (let ((s (as-text x))) (subseq s (max 0 (- (length s) n)))))

(defun mid (x start length)
  "LENGTH characters of X as text from 1-based START (spreadsheet MID)."
  (let* ((s (as-text x)) (a (max 0 (1- start))) (b (min (cl:length s) (+ a length))))
    (if (< a b) (subseq s a b) "")))

(defun upper (x) "X as upper-case text." (string-upcase (as-text x)))
(defun lower (x) "X as lower-case text." (string-downcase (as-text x)))
(defun trim (x) "X as text with surrounding whitespace removed."
  (string-trim '(#\Space #\Tab #\Newline #\Return) (as-text x)))

(defun substitute-text (x old new)
  "X as text with every occurrence of the substring OLD replaced by NEW
(spreadsheet SUBSTITUTE)."
  (let ((s (as-text x)) (o (as-text old)) (n (as-text new)))
    (if (zerop (length o))
        s
        (with-output-to-string (out)
          (loop with start = 0
                for pos = (search o s :start2 start)
                while pos
                do (write-string s out :start start :end pos)
                   (write-string n out)
                   (setf start (+ pos (length o)))
                finally (write-string s out :start start))))))

;;; --- dates (universal-time integers; no separate date type) ---------

(defun date (year month day &optional (hour 0) (minute 0) (second 0))
  "A timestamp (a universal-time integer) for the given calendar fields."
  (encode-universal-time second minute hour day month year))

(defun year (timestamp) "Calendar year of TIMESTAMP." (nth-value 5 (decode-universal-time timestamp)))
(defun month (timestamp) "Calendar month (1-12) of TIMESTAMP." (nth-value 4 (decode-universal-time timestamp)))
(defun day (timestamp) "Day of month (1-31) of TIMESTAMP." (nth-value 3 (decode-universal-time timestamp)))
(defun weekday (timestamp) "Day of week of TIMESTAMP (0 = Monday … 6 = Sunday)."
  (nth-value 6 (decode-universal-time timestamp)))

(defun now ()
  "The current timestamp (universal time). Volatile by nature — put it in a cell
marked with SET-VOLATILE so it refreshes each recalc."
  (get-universal-time))
