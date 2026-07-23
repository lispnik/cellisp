(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Cell references
;;;;
;;;; A reference is an (row . col) cons of zero-based integers.
;;;; The canonical external form is an A1-style string: column letters
;;;; (A, B, ..., Z, AA, AB, ...) followed by a 1-based row number.
;;;; ------------------------------------------------------------------

(deftype ref () '(cons unsigned-byte unsigned-byte))

(declaim (inline make-ref ref-row ref-col))
(defun make-ref (row col) (cons row col))
(defun ref-row (r) (car r))
(defun ref-col (r) (cdr r))

(defun col-letters->index (s start end)
  "Parse column letters in S[start..end) to a 0-based column index."
  (let ((n 0))
    (loop for i from start below end
          for ch = (char-upcase (char s i))
          do (unless (char<= #\A ch #\Z)
               (error 'bad-reference :format-control "Bad column letter ~S" :format-arguments (list ch)))
             (setf n (+ (* n 26) (1+ (- (char-code ch) (char-code #\A))))))
    (1- n)))

(defun index->col-letters (col)
  "Inverse of COL-LETTERS->INDEX: 0 -> \"A\", 26 -> \"AA\"."
  (let ((n (1+ col)) (out '()))
    (loop while (plusp n)
          do (multiple-value-bind (q r) (floor (1- n) 26)
               (push (code-char (+ (char-code #\A) r)) out)
               (setf n q)))
    (coerce out 'string)))

(defun parse-ref (designator)
  "Coerce DESIGNATOR (a ref cons or an A1 string/symbol) into a ref."
  (etypecase designator
    (cons
     ;; a ref cons must be (non-negative-int . non-negative-int); a malformed
     ;; coordinate is an off-grid position -> BAD-REFERENCE (#REF!).
     (unless (typep designator 'ref)
       (error 'bad-reference :format-control "Malformed reference ~S"
                             :format-arguments (list designator)))
     designator)
    ((or string symbol)
     ;; strip $ markers ($A$1, $A1, A$1) — they annotate copy/paste absoluteness
     ;; and are semantically ignored when resolving a reference to a cell.
     (let* ((s (remove #\$ (string designator)))
            (i 0) (len (length s)))
       (loop while (and (< i len) (alpha-char-p (char s i))) do (incf i))
       (when (or (zerop i) (= i len))
         ;; the literal "#REF!" a structural delete leaves is a dangling
         ;; reference (#REF!); any other unparseable token is a bad name (#NAME?).
         (if (string= s "#REF!")
             (error 'bad-reference :format-control "Malformed reference ~S"
                                   :format-arguments (list s))
             (error 'unknown-name :format-control "Malformed reference ~S"
                                  :format-arguments (list s))))
       (multiple-value-bind (n pos) (parse-integer s :start i :junk-allowed t)
         ;; require a row number that consumes the rest of the string, so
         ;; junk like "A1B" / "A1.5" signals a bad NAME rather than a raw
         ;; PARSE-INTEGER error.
         (when (or (null n) (/= pos len))
           (error 'unknown-name :format-control "Malformed reference ~S"
                                :format-arguments (list s)))
         (let ((col (col-letters->index s 0 i))
               (row (1- n)))
           (when (minusp row)
             ;; row 0 / negative: a coordinate off the grid -> #REF!
             (error 'bad-reference :format-control "Row must be >= 1 in ~S"
                                   :format-arguments (list s)))
           (make-ref row col)))))))

(defun ref-string (designator)
  "Return the A1 string form of a reference designator."
  (let ((r (parse-ref designator)))
    (format nil "~A~D" (index->col-letters (ref-col r)) (1+ (ref-row r)))))

;;;; ------------------------------------------------------------------
;;;; Spans — whole-column / whole-row references
;;;;
;;;; A SPAN names an entire column (or band of columns), or an entire row (band
;;;; of rows): AXIS is :COL or :ROW and LO/HI are inclusive 0-based indices on
;;;; that axis (columns for :COL, rows for :ROW). Unlike a finite (tl . br)
;;;; range, a span is unbounded on the *other* axis — "all of column A" — so it
;;;; depends on the column as a whole (see COL-WATCHERS / ROW-WATCHERS in
;;;; sheet.lisp) rather than on individual cells.
;;;;
;;;; Represented as the EQUAL-comparable tagged list (AXIS LO HI), matching the
;;;; cons-based style of REF/range so spans work as hash-table keys and MEMBER
;;;; items without a custom equality.
;;;; ------------------------------------------------------------------

(declaim (inline make-span span-axis span-lo span-hi))
(defun make-span (axis lo hi) (list axis lo hi))
(defun span-axis (s) (first s))
(defun span-lo (s) (second s))
(defun span-hi (s) (third s))

(defun span-p (x)
  "True if X is a span — an (AXIS LO HI) list with AXIS :COL or :ROW."
  (and (consp x) (member (car x) '(:col :row)) t))

(defun span-covers-p (span axis index)
  "True if SPAN is on AXIS and its LO..HI range includes INDEX."
  (and (eq (span-axis span) axis) (<= (span-lo span) index (span-hi span))))

;;;; ------------------------------------------------------------------
;;;; Tables — a named rectangular region with a header row
;;;;
;;;; A TABLE names a rectangle whose columns are referenced by header text
;;;; (Sales[Amount]) rather than by grid coordinate, so the reference tracks the
;;;; column's DATA as it grows/shrinks. REGION is the full (tl . br) rectangle
;;;; including the header row (its first row) and, when TOTALS-P, a trailing
;;;; totals row; the data rows lie between. Registered per-sheet in the TABLES
;;;; slot (see sheet.lisp). NAME keeps the user's casing; the slot key is upcased.
;;;; ------------------------------------------------------------------

(defstruct (table (:constructor %make-table (name region headers-p totals-p)))
  (name nil)
  (region nil :type cons)      ; (tl-ref . br-ref)
  (headers-p t)                ; first REGION row is the header row
  (totals-p nil))              ; last REGION row is a totals row

;;;; ------------------------------------------------------------------
;;;; Cell
;;;; ------------------------------------------------------------------

(defclass cell ()
  (;; The stored formula: either a literal (number/string/etc.) or a Lisp form.
   (formula :initform nil :accessor cell-formula :initarg :formula)
   ;; Cached computed value.
   (value :initform nil :accessor cell-value)
   ;; If evaluation failed, the condition is stored here (value is then nil).
   (err :initform nil :accessor cell-err)
   ;; Refs this cell reads (its precedents) and refs that read it (dependents).
   (precedents :initform '() :accessor cell-precedents :type list)
   (dependents :initform '() :accessor cell-dependents :type list)
   ;; Whole-column/row precedents: SPANs this cell reads (via COL/ROW/"A:A").
   ;; The consumer side lives here; the producer side is the sheet's
   ;; COL-WATCHERS / ROW-WATCHERS reverse index. A span read records ONE entry
   ;; per column/row spanned instead of one edge per cell.
   (range-precedents :initform '() :accessor cell-range-precedents :type list)
   ;; Cross-sheet precedents: cells this one reads in OTHER sheets, as a list of
   ;; grefs (sheet . ref). The producer side is tracked on the sheet's
   ;; FOREIGN-DEPENDENTS table. Empty for a standalone (non-workbook) sheet.
   (foreign-precedents :initform '() :accessor cell-foreign-precedents :type list)
   ;; Compiled-thunk cache for the environment eval path: COMPILED is the
   ;; function compiled from the formula in COMPILED-FROM (compared by EQ).
   (compiled :initform nil :accessor cell-compiled :type (or null function))
   (compiled-from :initform nil :accessor cell-compiled-from))
  (:documentation "A spreadsheet cell: cached value/error plus the dependency
back-links and the compiled-thunk cache."))

;;; Volatility (recompute every sweep, cf. RAND()/NOW()) is NOT a cell class:
;;; it is an orthogonal scheduling attribute, tracked in the sheet's VOLATILES
;;; registry and queried with VOLATILE-P. Keeping it off the class means it
;;; composes freely with every cell kind (a cell can be external AND volatile
;;; AND observed). Only the value-source variants (EXTERNAL-CELL, ASYNC-CELL)
;;; and the OBSERVABLE-MIXIN — the things that actually change dispatch — are
;;; classes; see taxonomy.lisp.
