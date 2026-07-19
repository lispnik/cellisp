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

;;; A sheet mutates shared state in place (the CELLS table, per-cell adjacency
;;; lists) through dynamic vars. Every public entry point takes the sheet's
;;; recursive LOCK so concurrent readers/writers — and out-of-band async
;;; deliveries from other threads — are serialized. The lock is recursive so a
;;; callback fired mid-sweep (e.g. an observer) may re-enter the read API.

(defstruct (sheet (:constructor %make-sheet))
  ;; ref-cons -> cell. Refs are equal-comparable conses, so use EQUAL.
  (cells (make-hash-table :test 'equal) :type hash-table)
  ;; Extra bindings (a plist or alist) exposed to formulas, e.g. constants.
  (environment '() :type list)
  ;; Set of refs whose cells are volatile (recompute every sweep). Kept as a
  ;; registry so RECOMPUTE-CLOSURE can seed them without scanning all cells.
  (volatiles (make-hash-table :test 'equal) :type hash-table)
  ;; Serializes all public access to this sheet (see comment above).
  (lock (bt:make-recursive-lock "cellisp-sheet")))

(defmacro with-sheet-lock ((sheet) &body body)
  "Run BODY holding SHEET's recursive lock."
  `(bt:with-recursive-lock-held ((sheet-lock ,sheet)) ,@body))

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

(defun set-cell-volatile (sheet ref volatile)
  "Add REF to (or remove it from) SHEET's volatile registry. Volatility is a
scheduling attribute independent of the cell's class, so this touches only the
registry — any kind of cell can be volatile."
  (if volatile
      (setf (gethash ref (sheet-volatiles sheet)) t)
      (remhash ref (sheet-volatiles sheet))))

(defun volatile-p (sheet designator)
  "True if DESIGNATOR is registered volatile on SHEET."
  (with-sheet-lock (sheet)
    (and (gethash (parse-ref designator) (sheet-volatiles sheet)) t)))
