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
           #:format-for))

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
SPEC falls back to :GENERAL. A function SPEC is called with VALUE."
  (cond
    ((functionp spec) (princ-to-string (funcall spec value)))
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
  (columns (make-hash-table :test 'eql) :type hash-table))

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
  "The effective SPEC for DESIGNATOR: a cell-specific format wins over a column
default, which wins over :GENERAL."
  (let ((ref (parse-ref designator)))
    (or (gethash ref (formats-cells formats))
        (gethash (ref-col ref) (formats-columns formats))
        :general)))

;;;; --- top level ------------------------------------------------------

(defun display-value (sheet designator &key formats)
  "The display string for a cell: its error token if it errored, \"\" if empty,
otherwise its value formatted per FORMATS (a registry from MAKE-FORMATS) or
:GENERAL when none is given."
  (multiple-value-bind (value error) (get-value sheet designator)
    (cond (error (error-token error))
          ((null value) "")
          (t (format-value value (if formats
                                     (format-for formats designator)
                                     :general))))))
