(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Conditions
;;;; ------------------------------------------------------------------

(define-condition sheet-error (error)
  ((format-control :initarg :format-control :initform "Sheet error"
                   :reader sheet-error-format-control)
   (format-arguments :initarg :format-arguments :initform '()
                     :reader sheet-error-format-arguments))
  (:report (lambda (c s)
             (apply #'format s (sheet-error-format-control c)
                    (sheet-error-format-arguments c)))))

(define-condition cyclic-reference (sheet-error)
  ((cells :initarg :cells :reader cyclic-reference-cells))
  (:report (lambda (c s)
             (format s "Cyclic reference through: ~{~A~^ -> ~}"
                     (mapcar #'ref-string (cyclic-reference-cells c))))))

(define-condition unbound-cell (sheet-error)
  ((ref :initarg :ref :reader unbound-cell-ref))
  (:report (lambda (c s)
             (format s "Cell ~A is empty" (ref-string (unbound-cell-ref c))))))

(define-condition cell-eval-error (sheet-error)
  ((ref :initarg :ref :reader cell-eval-error-ref)
   (original :initarg :original :reader cell-eval-error-original))
  (:report (lambda (c s)
             (format s "Error evaluating ~A: ~A"
                     (ref-string (cell-eval-error-ref c))
                     (cell-eval-error-original c)))))

;;;; ------------------------------------------------------------------
;;;; Sheet
;;;; ------------------------------------------------------------------

;;; A sheet is NOT thread-safe: recalculation drives mutable state through
;;; dynamic vars (*sheet*, *eval-stack*, *fresh*, *collected-precedents*)
;;; and mutates the CELLS table plus per-cell adjacency lists in place, with
;;; no locking. Confine all access to one sheet to a single thread, or wrap
;;; the mutators (SET-CELL/SET-CELLS/CLEAR-CELL) and reads in your own lock.

(defstruct (sheet (:constructor %make-sheet))
  ;; ref-cons -> cell. Refs are equal-comparable conses, so use EQUAL.
  (cells (make-hash-table :test 'equal) :type hash-table)
  ;; Extra bindings (a plist or alist) exposed to formulas, e.g. constants.
  (environment '() :type list)
  ;; Set of refs whose cells are volatile (recompute every sweep). Kept as a
  ;; registry so RECOMPUTE-CLOSURE can seed them without scanning all cells.
  (volatiles (make-hash-table :test 'equal) :type hash-table))

(defun make-sheet (&key environment)
  "Create an empty sheet. ENVIRONMENT is an alist of (symbol . value)
made visible to every formula via let-bindings established by EVAL-FORMULA."
  (%make-sheet :environment environment))

(defun find-cell (sheet ref)
  (gethash ref (sheet-cells sheet)))

(defun ensure-cell (sheet ref)
  (or (find-cell sheet ref)
      (setf (gethash ref (sheet-cells sheet)) (make-instance 'cell))))

(defun map-cells (fn sheet)
  "Call FN with (ref cell) for every non-empty cell."
  (maphash (lambda (ref cell) (funcall fn ref cell)) (sheet-cells sheet)))

(defun volatile-refs (sheet)
  "List of refs currently registered volatile on SHEET."
  (loop for ref being the hash-keys of (sheet-volatiles sheet) collect ref))

(defun set-cell-volatile (sheet ref cell volatile)
  "Promote CELL to (or demote from) VOLATILE-CELL in place and keep SHEET's
volatile registry in sync. Uses CHANGE-CLASS, so the cell keeps its value,
error, and dependency links across the transition."
  (cond
    (volatile
     (unless (typep cell 'volatile-cell) (change-class cell 'volatile-cell))
     (setf (gethash ref (sheet-volatiles sheet)) t))
    (t
     (when (typep cell 'volatile-cell) (change-class cell 'cell))
     (remhash ref (sheet-volatiles sheet)))))
