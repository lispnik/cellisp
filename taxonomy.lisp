(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Cell taxonomy
;;;;
;;;; Extra cell kinds beyond the base formula cell and VOLATILE-CELL, each
;;;; plugging into a different seam of the engine:
;;;;
;;;;   external-cell  - value produced by a Lisp thunk instead of a formula
;;;;                    (overrides COMPUTE-VALUE)
;;;;   async-cell     - non-blocking; value pushed in out-of-band via
;;;;                    DELIVER-ASYNC, which injects a recompute of dependents
;;;;   observed-cell  - fires subscriber callbacks after a sweep when its
;;;;                    settled value changed (overrides CELL-SWEPT)
;;;;
;;;; All the public entry points here take the sheet lock, so an async
;;;; delivery arriving on another thread is serialized against readers/writers.
;;;; ------------------------------------------------------------------

;;; --- external cells -------------------------------------------------

(defclass external-cell (cell)
  ((source :initform nil :initarg :source :accessor cell-source
           :documentation "Thunk called to produce the cell's value."))
  (:documentation "A cell whose value comes from calling SOURCE on each
recompute rather than from evaluating a formula — e.g. a DB row or a
computed system value. It reads no other cells, so it has no precedents;
its dependents track it normally."))

(defmethod compute-value ((cell external-cell) sheet ref)
  (declare (ignore sheet ref))
  (funcall (cell-source cell)))

(defun set-external (sheet designator source)
  "Install (or convert the cell at) DESIGNATOR as an EXTERNAL-CELL whose value
is produced by the SOURCE thunk. Returns the freshly computed value."
  (with-sheet-lock (sheet)
    (let* ((ref (parse-ref designator))
           (cell (ensure-cell sheet ref)))
      (unless (typep cell 'external-cell) (change-class cell 'external-cell))
      (setf (cell-source cell) source)
      (recompute-closure sheet (list ref))
      (cell-value cell))))

;;; --- async cells ----------------------------------------------------

(defclass async-cell (cell)
  ((fetcher :initform nil :initarg :fetcher :accessor async-fetcher
            :documentation "Function of one arg (a DELIVER callback) that
starts a fetch and eventually calls the callback with the value.")
   (pending :initform nil :accessor async-pending))
  (:documentation "A cell whose value arrives out-of-band. COMPUTE-VALUE
returns the last delivered value without blocking; REFRESH-ASYNC triggers the
fetcher, and DELIVER-ASYNC (called when the value is ready, possibly on
another thread) stores it and recomputes dependents."))

(defmethod compute-value ((cell async-cell) sheet ref)
  (declare (ignore sheet ref))
  (cell-value cell))                    ; non-blocking: just the last value

(defun set-async (sheet designator fetcher &key initial)
  "Install (or convert the cell at) DESIGNATOR as an ASYNC-CELL with the given
FETCHER and INITIAL value. Returns the initial value."
  (with-sheet-lock (sheet)
    (let* ((ref (parse-ref designator))
           (cell (ensure-cell sheet ref)))
      (unless (typep cell 'async-cell) (change-class cell 'async-cell))
      (setf (async-fetcher cell) fetcher
            (async-pending cell) nil
            (cell-value cell) initial
            (cell-err cell) nil)
      (recompute-closure sheet (cons ref (cell-dependents cell)))
      (cell-value cell))))

(defun refresh-async (sheet designator)
  "Trigger the async cell's fetcher (unless a fetch is already pending). The
fetcher should start its work and return promptly; it delivers the value by
calling the supplied callback, which routes to DELIVER-ASYNC."
  (with-sheet-lock (sheet)
    (let* ((ref (parse-ref designator))
           (cell (find-cell sheet ref)))
      (when (and (typep cell 'async-cell) (not (async-pending cell)))
        (setf (async-pending cell) t)
        (funcall (async-fetcher cell)
                 (lambda (value) (deliver-async sheet ref value))))))
  (values))

(defun deliver-async (sheet designator value)
  "Store VALUE into an async cell (typically from the fetcher's callback, on
any thread) and recompute its dependents. No-op for a non-async/missing cell."
  (with-sheet-lock (sheet)
    (let* ((ref (parse-ref designator))
           (cell (find-cell sheet ref)))
      (when (typep cell 'async-cell)
        (setf (cell-value cell) value
              (cell-err cell) nil
              (async-pending cell) nil)
        (recompute-closure sheet (cell-dependents cell)))))
  (values))

;;; --- observed cells -------------------------------------------------

(defvar *unset* (list '#:unset)
  "Unique sentinel: an observed cell has notified no value yet.")

(defclass observed-cell (cell)
  ((subscribers :initform '() :accessor cell-subscribers)
   (last-notified :initform *unset* :accessor cell-last-notified))
  (:documentation "A formula/literal cell that notifies subscribers after a
sweep whenever its settled value changed. Firing happens once per sweep in
CELL-SWEPT — never mid-computation — so observers never see a diamond's
intermediate states."))

(defmethod cell-swept ((cell observed-cell) sheet ref)
  (declare (ignore sheet ref))
  (let ((v (cell-value cell)))
    (unless (equal v (cell-last-notified cell))
      (setf (cell-last-notified cell) v)
      (dolist (fn (cell-subscribers cell)) (funcall fn v)))))

(defun observe (sheet designator callback)
  "Register CALLBACK (a function of the new value) to fire whenever
DESIGNATOR's value changes after a sweep. Promotes a plain cell to
OBSERVED-CELL in place; signals SHEET-ERROR for other cell kinds (combining
kinds would need a mixin)."
  (with-sheet-lock (sheet)
    (let* ((ref (parse-ref designator))
           (cell (ensure-cell sheet ref)))
      (unless (typep cell 'observed-cell)
        (unless (eq (class-of cell) (find-class 'cell))
          (error 'sheet-error
                 :format-control "Cannot observe a ~A"
                 :format-arguments (list (class-name (class-of cell)))))
        (change-class cell 'observed-cell))
      (pushnew callback (cell-subscribers cell))
      (values))))

(defun unobserve (sheet designator callback)
  "Remove a previously registered observer CALLBACK."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (parse-ref designator))))
      (when (typep cell 'observed-cell)
        (setf (cell-subscribers cell)
              (remove callback (cell-subscribers cell)))))
    (values)))
