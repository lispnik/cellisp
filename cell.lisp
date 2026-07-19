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
               (error 'sheet-error :format-control "Bad column letter ~S" :format-arguments (list ch)))
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
     ;; a ref cons must be (non-negative-int . non-negative-int); reject a
     ;; malformed cons at the boundary rather than let it fail deeper.
     (unless (typep designator 'ref)
       (error 'sheet-error :format-control "Malformed reference ~S"
                           :format-arguments (list designator)))
     designator)
    ((or string symbol)
     (let* ((s (string designator))
            (i 0) (len (length s)))
       (loop while (and (< i len) (alpha-char-p (char s i))) do (incf i))
       (when (or (zerop i) (= i len))
         (error 'sheet-error :format-control "Malformed reference ~S"
                             :format-arguments (list s)))
       (multiple-value-bind (n pos) (parse-integer s :start i :junk-allowed t)
         ;; require a row number that consumes the rest of the string, so
         ;; junk like "A1B" / "A1.5" signals SHEET-ERROR rather than a raw
         ;; PARSE-INTEGER error.
         (when (or (null n) (/= pos len))
           (error 'sheet-error :format-control "Malformed reference ~S"
                               :format-arguments (list s)))
         (let ((col (col-letters->index s 0 i))
               (row (1- n)))
           (when (minusp row)
             (error 'sheet-error :format-control "Row must be >= 1 in ~S"
                                 :format-arguments (list s)))
           (make-ref row col)))))))

(defun ref-string (designator)
  "Return the A1 string form of a reference designator."
  (let ((r (parse-ref designator)))
    (format nil "~A~D" (index->col-letters (ref-col r)) (1+ (ref-row r)))))

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
