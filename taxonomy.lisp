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

;;; --- volatility (orthogonal, registry-only) -------------------------

(defun set-volatile (sheet designator volatile)
  "Mark (or unmark) DESIGNATOR volatile — recomputed on every sweep, whatever
its kind. Volatility is an attribute in the sheet registry, not a cell class,
so it composes with formula/external/async/observed cells alike."
  (with-sheet-lock (sheet)
    (set-cell-volatile sheet (parse-ref designator) volatile)
    volatile))

;;; --- observation (a composable mixin) -------------------------------
;;;
;;; OBSERVABLE-MIXIN adds notification to *any* value-source class. OBSERVE
;;; combines it with the cell's current class on the fly (COMBINED-CLASS) and
;;; CHANGE-CLASSes into the result, so you can observe a plain, external, or
;;; async cell. The mixin only overrides CELL-SWEPT, and the value-source
;;; classes only override COMPUTE-VALUE, so the two axes never collide.

(defvar *unset* (list '#:unset)
  "Unique sentinel: an observed cell has notified no value yet.")

(defclass observable-mixin ()
  ((subscribers :initform '() :accessor cell-subscribers)
   (last-notified :initform *unset* :accessor cell-last-notified))
  (:documentation "Mixin adding post-sweep change notification to any cell.
Combined with a value-source class by OBSERVE. Fires in CELL-SWEPT — once per
sweep, on settled values — so observers never see diamond intermediates."))

(defmethod cell-swept ((cell observable-mixin) sheet ref)
  (declare (ignore sheet ref))
  (let ((v (cell-value cell)))
    (unless (equal v (cell-last-notified cell))
      (setf (cell-last-notified cell) v)
      (dolist (fn (cell-subscribers cell)) (funcall fn v)))))

(defun combined-class (mixin base-class-name)
  "Find, or lazily define and memoize, the class combining MIXIN over the
class named BASE-CLASS-NAME. The combined class is interned by name, so the
same (mixin . base) pair always maps to one class."
  (let* ((name (intern (format nil "~A+~A" mixin base-class-name) :cellisp))
         (existing (find-class name nil)))
    (or existing
        (progn
          (eval `(defclass ,name (,mixin ,base-class-name) ()))
          (find-class name)))))

(defun observe (sheet designator callback)
  "Register CALLBACK (a function of the new value) to fire whenever
DESIGNATOR's value changes after a sweep. Composes with the cell's existing
kind: a plain, external, or async cell is promoted in place to a combination
class carrying OBSERVABLE-MIXIN, preserving value and dependency links."
  (with-sheet-lock (sheet)
    (let* ((ref (parse-ref designator))
           (cell (ensure-cell sheet ref)))
      (unless (typep cell 'observable-mixin)
        (change-class cell (combined-class 'observable-mixin
                                           (class-name (class-of cell)))))
      (pushnew callback (cell-subscribers cell))
      (values))))

(defun unobserve (sheet designator callback)
  "Remove a previously registered observer CALLBACK. The cell keeps its
observable class (now inert if it has no subscribers)."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (parse-ref designator))))
      (when (typep cell 'observable-mixin)
        (setf (cell-subscribers cell)
              (remove callback (cell-subscribers cell)))))
    (values)))
