(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Workbook — a named collection of sheets
;;;;
;;;; A workbook groups sheets under names and lets their formulas reference one
;;;; another across sheet boundaries: inside a formula, "Data!A1" reads cell A1
;;;; of the sheet named "Data". A cell belongs to at most one workbook; a sheet
;;;; created with MAKE-SHEET is standalone (its WORKBOOK slot is NIL) and pays
;;;; none of the cross-sheet cost. ADD-SHEET is the only way a sheet joins a
;;;; workbook, which stamps its WORKBOOK back-reference and NAME.
;;;;
;;;; Sheet names are matched case-insensitively (spreadsheet convention) but the
;;;; original spelling is kept for display and serialization.
;;;; ------------------------------------------------------------------

(defstruct (workbook (:constructor %make-workbook))
  ;; Ordered alist of (upcased-name-key . sheet), insertion order preserved so
  ;; WORKBOOK-SHEETS / serialization are deterministic.
  (entries '() :type list))

(defun make-workbook ()
  "Create an empty workbook. Add sheets to it with ADD-SHEET."
  (%make-workbook))

(defun %sheet-key (name) (string-upcase (string name)))

(defun find-sheet (workbook name)
  "The sheet named NAME in WORKBOOK (case-insensitive), or NIL."
  (cdr (assoc (%sheet-key name) (workbook-entries workbook) :test #'string=)))

(defun workbook-sheets (workbook)
  "The workbook's sheets, in insertion order."
  (mapcar #'cdr (workbook-entries workbook)))

(defun workbook-names (workbook)
  "The workbook's sheet names, in insertion order (original spelling)."
  (mapcar #'sheet-name (workbook-sheets workbook)))

(defun workbook-sheet-count (workbook) (length (workbook-entries workbook)))

(defun add-sheet (workbook name &key environment)
  "Create a new sheet named NAME, add it to WORKBOOK, and return it. Signals
SHEET-ERROR if a sheet by that name (case-insensitive) already exists. ENVIRONMENT
is passed through to MAKE-SHEET."
  (when (find-sheet workbook name)
    (error 'sheet-error :format-control "A sheet named ~S already exists"
                        :format-arguments (list name)))
  (let ((sheet (make-sheet :environment environment)))
    (setf (sheet-workbook sheet) workbook
          (sheet-name sheet) (string name))
    ;; append, preserving insertion order
    (setf (workbook-entries workbook)
          (append (workbook-entries workbook)
                  (list (cons (%sheet-key name) sheet))))
    sheet))

(defun %attach-sheet (workbook name sheet)
  "Attach an already-built SHEET (e.g. from deserialization) to WORKBOOK under
NAME without recomputing. Internal to workbook loading."
  (setf (sheet-workbook sheet) workbook
        (sheet-name sheet) (string name)
        (workbook-entries workbook)
        (append (workbook-entries workbook) (list (cons (%sheet-key name) sheet))))
  sheet)

(defun remove-sheet (workbook name)
  "Remove the sheet named NAME from WORKBOOK (detaching it back to standalone).
Returns the removed sheet, or NIL if there was none. Cells in other sheets that
referenced it will error (missing sheet) on their next recompute."
  (let* ((key (%sheet-key name))
         (pair (assoc key (workbook-entries workbook) :test #'string=)))
    (when pair
      (setf (workbook-entries workbook)
            (remove pair (workbook-entries workbook))
            (sheet-workbook (cdr pair)) nil
            (sheet-name (cdr pair)) nil)
      (cdr pair))))
