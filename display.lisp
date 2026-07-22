;;;; ------------------------------------------------------------------
;;;; cellisp/display — a rendering layer over the Cellisp engine
;;;;
;;;; The engine computes values and stores per-cell errors as condition
;;;; objects; GET-VALUE hands back (values value error-or-nil). This optional
;;;; layer (a separate ASDF system, `cellisp/display`) turns that into what a
;;;; cell actually *shows*: a formatted string, or a compact spreadsheet error
;;;; token like #DIV/0! / #REF! / #CYCLE!. It is a pure layer over the public
;;;; API — it never mutates a sheet and the core engine does not depend on it.
;;;; ------------------------------------------------------------------

(defpackage #:cellisp/display
  (:use #:cl #:cellisp)
  (:export #:display-value #:error-token #:format-value
           #:make-formats #:formats-p #:set-format #:set-column-format
           #:format-for #:add-conditional #:conditional-spec
           #:print-sheet #:print-workbook #:formula-string))

(in-package #:cellisp/display)

;;;; --- error tokens ---------------------------------------------------

(defun error-token (condition)
  "Map a stored cell condition to a spreadsheet-style error token string.

Excel-style mapping, dispatched purely on the condition's TYPE — the core signals
a dedicated class (BAD-REFERENCE, UNKNOWN-NAME, NUMERIC-ERROR) at each failure
site, so the display layer no longer parses report text. The subclasses are
listed before the SHEET-ERROR catch-all, since TYPECASE takes the first match."
  (typecase condition
    (cyclic-reference "#CYCLE!")
    (bad-reference    "#REF!")            ; #REF! sentinel / off-grid coordinate
    (unknown-name     "#NAME?")           ; unknown sheet / unparseable name
    (numeric-error    "#NUM!")            ; e.g. an aggregate over no numbers
    (unbound-cell     "#REF!")            ; a formula read an empty cell
    (invalid-value    "#VALUE!")          ; validator / typed-input rejection
    (cell-eval-error                      ; a raw Lisp error, wrapped
     (let ((original (cell-eval-error-original condition)))
       (typecase original
         (division-by-zero "#DIV/0!")
         (type-error       "#VALUE!")
         (arithmetic-error "#NUM!")       ; overflow, domain error, …
         (t                "#VALUE!"))))
    (sheet-error "#ERR!")                 ; any other engine error
    (t "#ERR!")))

;;;; --- value formatting -----------------------------------------------

(defun %fixed (x places)
  "X to PLACES decimals; at 0 places use integer form so there is no bare
trailing dot (\"25\" not \"25.\")."
  (if (zerop places) (format nil "~D" (round x)) (format nil "~,VF" places x)))

(defun %number-string (n spec)
  "Format the number N per SPEC: :GENERAL, :INTEGER, (:FIXED n),
(:PERCENT &optional n), or (:CURRENCY &optional sym n)."
  (cond
    ((eq spec :integer) (format nil "~D" (round n)))
    ((and (consp spec) (eq (first spec) :fixed))
     (%fixed n (or (second spec) 2)))
    ((and (consp spec) (eq (first spec) :percent))
     (concatenate 'string (%fixed (* 100 n) (or (second spec) 0)) "%"))
    ((and (consp spec) (eq (first spec) :currency))
     (concatenate 'string (or (second spec) "$") (%fixed n (or (third spec) 2))))
    ;; :general — integers exact, other reals as a natural decimal.
    ((integerp n) (format nil "~D" n))
    ((rationalp n) (%trim-float (float n 1.0d0)))
    (t (%trim-float n))))

(defun %trim-float (x)
  "A compact decimal string for a float: no exponent, trailing zeros trimmed."
  (let ((s (format nil "~F" x)))
    ;; ~F on a whole-valued float yields e.g. \"3.0\"; leave one decimal off only
    ;; when it is exactly \".0\" so 3.0 shows as \"3\" but 3.5 stays \"3.5\".
    (let ((dot (position #\. s)))
      (if (and dot (every (lambda (c) (char= c #\0)) (subseq s (1+ dot))))
          (subseq s 0 dot)
          s))))

(defun format-value (value &optional (spec :general))
  "Render VALUE (a cell's computed value) as a display string. Handles the value
kinds cells hold: integers, ratios, floats, strings, NIL (empty -> \"\"), and the
lists produced by CELLS (elements joined by \", \"). A non-number handed a numeric
SPEC falls back to :GENERAL. A function SPEC is called with VALUE; a string SPEC
is returned verbatim (a literal replacement, handy for conditional rules)."
  (cond
    ((functionp spec) (princ-to-string (funcall spec value)))
    ((stringp spec) spec)               ; a literal-string spec replaces the value
    ((null value) "")
    ((stringp value) value)
    ((numberp value) (%number-string value spec))
    ((listp value)
     (format nil "~{~A~^, ~}" (mapcar (lambda (v) (format-value v spec)) value)))
    (t (princ-to-string value))))

;;;; --- format registry (display-owned, not serialized) ----------------

(defstruct (formats (:constructor %make-formats))
  ;; key -> spec. A cell key is a ref (row . col) for a global rule, or a
  ;; (sheet-name . ref) cons for a sheet-scoped one; a column key is an index, or
  ;; a (sheet-name . index) cons. Sheet names are upcased. Both tables use EQUAL.
  (cells (make-hash-table :test 'equal) :type hash-table)
  (columns (make-hash-table :test 'equal) :type hash-table)
  ;; conditional rules, in order: each (predicate spec column-or-nil sheet-or-nil).
  (rules '() :type list))

(defun make-formats () "An empty format registry." (%make-formats))

(defun %split-sheet (designator)
  "For a sheet-qualified designator like \"Sales!D5\" return (values \"SALES\"
\"D5\") — sheet name upcased. Otherwise (values NIL designator)."
  (if (typep designator '(or string symbol))
      (let* ((s (string designator)) (bang (position #\! s)))
        (if bang
            (values (string-upcase (subseq s 0 bang)) (subseq s (1+ bang)))
            (values nil designator)))
      (values nil designator)))

(defun %column-index (column)
  (if (integerp column) column
      (ref-col (parse-ref (format nil "~A1" column)))))

(defun set-format (formats designator spec)
  "Set the display SPEC for a single cell. DESIGNATOR is an A1 string or ref cons,
optionally sheet-qualified (\"Sales!D5\") to scope the rule to that sheet — which
matters when one registry styles a whole workbook. Returns SPEC."
  (multiple-value-bind (sheet local) (%split-sheet designator)
    (let ((ref (parse-ref local)))
      (setf (gethash (if sheet (cons sheet ref) ref) (formats-cells formats))
            spec))))

(defun set-column-format (formats column spec)
  "Set a default display SPEC for a whole COLUMN — a 0-based index or a column
letter (\"A\", \"AA\"), optionally sheet-qualified (\"Sales!B\"). Returns SPEC."
  (multiple-value-bind (sheet col) (%split-sheet column)
    (let ((idx (%column-index col)))
      (setf (gethash (if sheet (cons sheet idx) idx) (formats-columns formats))
            spec))))

(defun format-for (formats designator &optional context-sheet)
  "The effective *static* SPEC for DESIGNATOR: most specific wins — a sheet-scoped
cell format, then a global cell format, then a sheet-scoped column default, then a
global column default, then :GENERAL. The sheet is DESIGNATOR's own qualifier if
it has one, otherwise CONTEXT-SHEET (what DISPLAY-VALUE passes)."
  (multiple-value-bind (dsheet local) (%split-sheet designator)
    (let* ((ref (parse-ref local))
           (up (or dsheet (and context-sheet (string-upcase (string context-sheet))))))
      (or (and up (gethash (cons up ref) (formats-cells formats)))
          (gethash ref (formats-cells formats))
          (and up (gethash (cons up (ref-col ref)) (formats-columns formats)))
          (gethash (ref-col ref) (formats-columns formats))
          :general))))

(defun add-conditional (formats predicate spec &key column sheet)
  "Add a conditional-format rule: a cell whose *value* satisfies PREDICATE renders
with SPEC instead of its static format. SPEC is any FORMAT-VALUE spec, including a
function of the value or a literal string. COLUMN (index or letter) and/or SHEET
(name) scope the rule; omit both for the whole workbook. Rules are tried in order,
first match wins. Returns FORMATS."
  (setf (formats-rules formats)
        (append (formats-rules formats)
                (list (list predicate spec
                            (and column (%column-index column))
                            (and sheet (string-upcase (string sheet)))))))
  formats)

(defun conditional-spec (formats designator value &optional context-sheet)
  "The SPEC of the first conditional rule matching VALUE at DESIGNATOR (in its
column and sheet scope), or NIL. PREDICATE is applied defensively, so a rule that
errors on a value just doesn't match."
  (multiple-value-bind (dsheet local) (%split-sheet designator)
    (let ((col (ref-col (parse-ref local)))
          (up (or dsheet (and context-sheet (string-upcase (string context-sheet))))))
      (loop for (predicate spec rule-col rule-sheet) in (formats-rules formats)
            when (and (or (null rule-col) (= rule-col col))
                      (or (null rule-sheet) (equal rule-sheet up))
                      (ignore-errors (funcall predicate value)))
              do (return spec)))))

;;;; --- top level ------------------------------------------------------

(defun display-value (sheet designator &key formats)
  "The display string for a cell: its error token if it errored, \"\" if empty,
otherwise its value formatted per FORMATS (a registry from MAKE-FORMATS) or
:GENERAL when none is given."
  (multiple-value-bind (value error) (get-value sheet designator)
    (cond (error (error-token error))
          ((null value) "")
          (t (format-value
              value
              (if formats
                  ;; sheet-aware: a rule/format scoped to this sheet can match,
                  ;; and a matching conditional rule overrides the static format
                  (let ((name (sheet-name sheet)))
                    (or (conditional-spec formats designator value name)
                        (format-for formats designator name)))
                  :general))))))

;;;; --- console rendering ----------------------------------------------

(defun formula-string (sheet designator)
  "The cell's *formula* as a display string, spreadsheet \"show-formulas\" style:
\"=<form>\" for a real formula, the literal for a constant, \"\" for an empty
cell. Forms print in the CELLISP package so operators/refs read cleanly."
  (let ((f (get-formula sheet designator)))
    (cond ((null f) "")
          ((consp f) (let ((*package* (find-package '#:cellisp))
                           (*print-pretty* nil))   ; keep the form on one line
                       (format nil "=~S" f)))
          ((stringp f) f)
          (t (princ-to-string f)))))

(defun %print-names (sheet stream)
  "Print a sheet's named cells and ranges (name=A1 / name=A1:B3), if any."
  (let ((entries '()))
    (map-names (lambda (name val)
                 (push (if (consp (car val))   ; a (tl . br) range value
                           (format nil "~A=~A:~A" name
                                   (ref-string (car val)) (ref-string (cdr val)))
                           (format nil "~A=~A" name (ref-string val)))
                       entries))
               sheet)
    (when entries
      (format stream "  names: ~{~A~^, ~}~%" (sort entries #'string<)))))

(defun %print-env (sheet stream)
  "Print a sheet's environment constants (name=value), if any."
  (let ((env (sheet-environment sheet)))
    (when env
      (format stream "  env:   ~{~A~^, ~}~%"
              (loop for pair in env
                    collect (if (consp pair)
                                (format nil "~A=~A" (car pair) (cdr pair))
                                (format nil "~A" pair)))))))

(defun %clip (s width)
  "S truncated to WIDTH chars with a trailing … when longer (WIDTH NIL = no clip)."
  (if (and width (> (length s) width))
      (concatenate 'string (subseq s 0 (max 0 (1- width))) "…")
      s))

(defun print-sheet (sheet &key (stream *standard-output*) formats formulas
                            max-col-width (name (sheet-name sheet)))
  "Render SHEET to STREAM as an aligned text grid — column-letter headers, row
numbers, cells shown via DISPLAY-VALUE (numbers right-aligned, everything else
left). With FORMULAS non-NIL each cell shows its formula (\"=<form>\") instead of
its value, everything left-aligned. FORMATS, if given, styles the values.
MAX-COL-WIDTH, if given, truncates each cell to that many characters (with a
trailing …) so a few very wide cells don't blow up the table. NAME (defaulting to
the sheet's own name) prints a heading; NIL prints none. An empty sheet prints
\"(empty)\". Below the grid, any named cells/ranges and environment constants the
sheet carries are listed."
  (when name (format stream "~&~A~%" name))
  (multiple-value-bind (rows cols) (sheet-dimensions sheet)
    (if (or (zerop rows) (zerop cols))
        (format stream "  (empty)~%")
        (let ((rowlabw (length (princ-to-string rows)))
              (headers (loop for c below cols collect (index->col-letters c)))
              (strs (make-array (list rows cols)))
              (nums (make-array (list rows cols)))
              (widths (make-array cols)))
          ;; render every cell, tracking per-column width and numeric-ness
          (dotimes (c cols) (setf (aref widths c) (length (nth c headers))))
          (dotimes (r rows)
            (dotimes (c cols)
              (let ((str (%clip (if formulas
                                    (formula-string sheet (cons r c))
                                    (display-value sheet (cons r c) :formats formats))
                                max-col-width)))
                (setf (aref strs r c) str
                      ;; in formula view everything is text -> left-align
                      (aref nums r c) (and (not formulas)
                                           (numberp (get-value sheet (cons r c))))
                      (aref widths c) (max (aref widths c) (length str))))))
          ;; header row + rule
          (format stream "~v@A |" rowlabw "")
          (dotimes (c cols) (format stream " ~vA |" (aref widths c) (nth c headers)))
          (format stream "~%~v@A-+" rowlabw (make-string rowlabw :initial-element #\-))
          (dotimes (c cols)
            (format stream "~A+" (make-string (+ 2 (aref widths c))
                                              :initial-element #\-)))
          (terpri stream)
          ;; data rows
          (dotimes (r rows)
            (format stream "~v@A |" rowlabw (1+ r))
            (dotimes (c cols)
              (let ((w (aref widths c)) (str (aref strs r c)))
                (if (aref nums r c)
                    (format stream " ~v@A |" w str)   ; right-align numbers
                    (format stream " ~vA |" w str)))) ; left-align text/tokens
            (terpri stream)))))
  ;; footer: named cells/ranges and environment constants (when present)
  (%print-names sheet stream)
  (%print-env sheet stream)
  (values))

(defun print-workbook (workbook &key (stream *standard-output*) formats formulas
                                  max-col-width)
  "Render every sheet of WORKBOOK to STREAM (default stdout), one after another,
each headed by its name. FORMATS styles the values; FORMULAS non-NIL shows each
cell's formula instead of its value; MAX-COL-WIDTH truncates wide cells."
  (loop for s in (workbook-sheets workbook)
        for first = t then nil
        do (unless first (terpri stream))
           (print-sheet s :stream stream :formats formats :formulas formulas
                          :max-col-width max-col-width))
  (values))
