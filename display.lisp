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
           #:print-sheet #:print-workbook))

(in-package #:cellisp/display)

;;;; --- error tokens ---------------------------------------------------

(defun error-token (condition)
  "Map a stored cell condition to a spreadsheet-style error token string.

Excel-style mapping. The engine has no dedicated classes for #REF! (a dangling
reference from a structural delete) or #NAME? (an unknown sheet/name) — both
surface as a base SHEET-ERROR — so they are disambiguated by inspecting the
condition's report text (a delete leaves the literal \"#REF!\" in it; a bad name
reads \"Malformed reference\"/\"No sheet named\"). This report-string check is the
pragmatic cost of keeping the display layer free of core-engine changes."
  (typecase condition
    (cyclic-reference "#CYCLE!")
    (unbound-cell     "#REF!")            ; a formula read an empty cell
    (invalid-value    "#VALUE!")          ; validator / typed-input rejection
    (cell-eval-error                      ; a raw Lisp error, wrapped
     (let ((original (cell-eval-error-original condition)))
       (typecase original
         (division-by-zero "#DIV/0!")
         (type-error       "#VALUE!")
         (arithmetic-error "#NUM!")       ; overflow, domain error, …
         (t                "#VALUE!"))))
    (sheet-error
     ;; No dedicated #REF!/#NAME? classes exist, so read the report text. A
     ;; broken *reference to a position* — the literal "#REF!" a delete leaves,
     ;; or a ref shifted off the grid ("Row must be >= 1", "Bad column letter")
     ;; — is #REF!; an unknown sheet or unparseable name is #NAME?.
     (let ((text (princ-to-string condition)))
       (cond ((search "No sheet named" text) "#NAME?")
             ((or (search "#REF!" text)
                  (search "Row must be" text)
                  (search "Bad column letter" text))
              "#REF!")
             ((search "Malformed reference" text) "#NAME?")
             (t "#ERR!"))))
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
  ;; parse-ref key -> spec, and column-index -> spec (a column default).
  (cells (make-hash-table :test 'equal) :type hash-table)
  (columns (make-hash-table :test 'eql) :type hash-table)
  ;; conditional-format rules, in order: each (predicate spec column-or-nil).
  (rules '() :type list))

(defun make-formats () "An empty format registry." (%make-formats))

(defun set-format (formats designator spec)
  "Set the display SPEC for a single cell (an A1 string or ref cons). Returns SPEC."
  (setf (gethash (parse-ref designator) (formats-cells formats)) spec))

(defun set-column-format (formats column spec)
  "Set a default display SPEC for a whole COLUMN. COLUMN is a 0-based index or a
column-letter string (\"A\", \"AA\"); a single letter is parsed via its A1 form.
Returns SPEC."
  (let ((col (if (integerp column)
                 column
                 (ref-col (parse-ref (format nil "~A1" column))))))
    (setf (gethash col (formats-columns formats)) spec)))

(defun format-for (formats designator)
  "The effective *static* SPEC for DESIGNATOR: a cell-specific format wins over a
column default, which wins over :GENERAL. (Conditional rules are applied
separately, by DISPLAY-VALUE, since they depend on the cell's value.)"
  (let ((ref (parse-ref designator)))
    (or (gethash ref (formats-cells formats))
        (gethash (ref-col ref) (formats-columns formats))
        :general)))

(defun %column-index (column)
  (if (integerp column) column
      (ref-col (parse-ref (format nil "~A1" column)))))

(defun add-conditional (formats predicate spec &key column)
  "Add a conditional-format rule: a cell whose *value* satisfies PREDICATE renders
with SPEC instead of its static format. SPEC is any FORMAT-VALUE spec, including a
function of the value — e.g. wrap negatives in parentheses, or map a status to a
glyph. COLUMN (an index or letter) scopes the rule to one column; omit it for the
whole sheet. Rules are tried in order and the first match wins. Returns FORMATS."
  (setf (formats-rules formats)
        (append (formats-rules formats)
                (list (list predicate spec (and column (%column-index column))))))
  formats)

(defun conditional-spec (formats designator value)
  "The SPEC of the first conditional rule matching VALUE at DESIGNATOR, or NIL.
PREDICATE is applied defensively, so a rule that errors on a value just doesn't
match."
  (let ((col (ref-col (parse-ref designator))))
    (loop for (predicate spec rule-col) in (formats-rules formats)
          when (and (or (null rule-col) (= rule-col col))
                    (ignore-errors (funcall predicate value)))
            do (return spec))))

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
                  ;; a matching conditional rule overrides the static format
                  (or (conditional-spec formats designator value)
                      (format-for formats designator))
                  :general))))))

;;;; --- console rendering ----------------------------------------------

(defun print-sheet (sheet &key (stream *standard-output*) formats
                            (name (sheet-name sheet)))
  "Render SHEET to STREAM as an aligned text grid — column-letter headers, row
numbers, cells shown via DISPLAY-VALUE (numbers right-aligned, everything else
left). FORMATS, if given, styles the values. NAME (defaulting to the sheet's own
name) prints a heading; NIL prints none. An empty sheet prints \"(empty)\"."
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
              (let ((str (display-value sheet (cons r c) :formats formats)))
                (setf (aref strs r c) str
                      (aref nums r c) (numberp (get-value sheet (cons r c)))
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
  (values))

(defun print-workbook (workbook &key (stream *standard-output*) formats)
  "Render every sheet of WORKBOOK to STREAM (default stdout), one after another,
each headed by its name. FORMATS, if given, styles all sheets."
  (loop for s in (workbook-sheets workbook)
        for first = t then nil
        do (unless first (terpri stream))
           (print-sheet s :stream stream :formats formats))
  (values))
