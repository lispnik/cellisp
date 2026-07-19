(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Cell taxonomy
;;;;
;;;; Cell behavior splits along orthogonal axes so kinds COMPOSE:
;;;;
;;;;   value source (exclusive) - how the value is produced: formula (base
;;;;     CELL), a Lisp thunk (EXTERNAL-CELL), or pushed in out-of-band
;;;;     (ASYNC-CELL). These override COMPUTE-VALUE.
;;;;   mixins (composable)      - cross-cutting behavior layered onto any
;;;;     value source, e.g. OBSERVABLE-MIXIN (overrides CELL-SWEPT).
;;;;   volatility (attribute)   - a registry flag, not a class at all
;;;;     (see SET-VOLATILE / VOLATILE-P).
;;;;
;;;; A concrete cell is one value-source class plus any set of mixins;
;;;; COMBINED-CLASS builds that combination on the fly (memoized), so a cell
;;;; can be, say, external AND observed AND volatile at once. All public
;;;; drivers take the sheet lock.
;;;; ------------------------------------------------------------------

;;; --- value-source classes (override COMPUTE-VALUE) ------------------

(defclass external-cell (cell)
  ((source :initform nil :initarg :source :accessor cell-source
           :documentation "Thunk called to produce the cell's value."))
  (:documentation "A cell whose value comes from calling SOURCE on each
recompute rather than from evaluating a formula — e.g. a DB row or a computed
system value. It reads no other cells, so it has no precedents."))

(defmethod compute-value ((cell external-cell) sheet ref)
  (declare (ignore sheet ref))
  (funcall (cell-source cell)))

(defclass async-cell (cell)
  ((fetcher :initform nil :initarg :fetcher :accessor async-fetcher
            :documentation "Function of one arg (a DELIVER callback) that
starts a fetch and eventually calls the callback with the value.")
   (pending :initform nil :accessor async-pending))
  (:documentation "A cell whose value arrives out-of-band. COMPUTE-VALUE
returns the last delivered value without blocking; REFRESH-ASYNC triggers the
fetcher, and DELIVER-ASYNC stores the value and recomputes dependents."))

(defmethod compute-value ((cell async-cell) sheet ref)
  (declare (ignore sheet ref))
  (cell-value cell))                    ; non-blocking: just the last value

;;; --- observation mixin (overrides CELL-SWEPT) -----------------------

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

;;; --- read-only mixin (overrides CELL-WRITABLE-P) --------------------

(defclass readonly-mixin () ()
  (:documentation "Mixin that locks a cell against user reassignment: SET-CELL,
SET-CELLS, CLEAR-CELL, SET-EXTERNAL and SET-ASYNC signal READONLY-CELL. The
cell still recomputes from its precedents (internal writes are not guarded),
so a read-only formula cell tracks its inputs but its formula can't be edited.
Dispatches on a different generic than OBSERVABLE-MIXIN, so the two compose."))

(defmethod cell-writable-p ((cell readonly-mixin)) nil)

;;; --- composing value source + mixins --------------------------------

(defparameter *value-source-classes* '(external-cell async-cell cell)
  "Value-source cell classes, most specific first; a cell has exactly one.")

(defparameter *mixin-classes* '(observable-mixin readonly-mixin)
  "Known composable behavior mixins. Add a new cross-cutting axis by defining
a mixin (overriding some generic) and listing it here.")

(defun cell-value-source (cell)
  "The value-source class name underlying CELL."
  (loop for c in *value-source-classes*
        when (typep cell c) return c
        finally (return 'cell)))

(defun cell-mixins (cell)
  "The composable mixins currently on CELL."
  (loop for m in *mixin-classes* when (typep cell m) collect m))

(defun combined-class (base mixins)
  "Find, or lazily define and memoize, the concrete class combining
value-source class BASE with the *set* MIXINS (class names). With no mixins,
returns BASE itself. Mixins are sorted by name, so any permutation of the same
set maps to a single class — combinations of any arity are supported."
  (let ((mix (sort (remove-duplicates (copy-list mixins))
                   #'string< :key #'symbol-name)))
    (if (null mix)
        (find-class base)
        (let* ((name (intern (format nil "~{~A+~}~A" mix base) :cellisp))
               (existing (find-class name nil)))
          (or existing
              (progn
                (eval `(defclass ,name (,@mix ,base) ()))
                (find-class name)))))))

(defun morph-cell (cell base mixins)
  "CHANGE-CLASS CELL to the combination of value-source BASE and the set
MIXINS, unless it is already that class. Slots shared across the change (value,
error, dependency links, and any retained mixin slots) are preserved."
  (let ((target (combined-class base mixins)))
    (unless (eq (class-of cell) target)
      (change-class cell target))))

(defun add-mixin (cell mixin)
  "Add MIXIN to CELL, keeping its value source and any other mixins."
  (morph-cell cell (cell-value-source cell) (adjoin mixin (cell-mixins cell))))

(defun remove-mixin (cell mixin)
  "Remove MIXIN from CELL, keeping its value source and other mixins."
  (morph-cell cell (cell-value-source cell) (remove mixin (cell-mixins cell))))

;;; --- drivers: value source ------------------------------------------

(defun set-external (sheet designator source)
  "Install (or convert the cell at) DESIGNATOR as an EXTERNAL-CELL whose value
is produced by the SOURCE thunk, preserving any mixins it already carries.
Returns the freshly computed value."
  (with-sheet-lock (sheet)
    (let* ((ref (parse-ref designator))
           (cell (ensure-cell sheet ref)))
      (unless (cell-writable-p cell) (error 'readonly-cell :ref ref))
      (morph-cell cell 'external-cell (cell-mixins cell))
      (setf (cell-source cell) source)
      (recompute-closure sheet (list ref))
      (cell-value cell))))

(defun set-async (sheet designator fetcher &key initial)
  "Install (or convert the cell at) DESIGNATOR as an ASYNC-CELL with the given
FETCHER and INITIAL value, preserving any mixins. Returns the initial value."
  (with-sheet-lock (sheet)
    (let* ((ref (parse-ref designator))
           (cell (ensure-cell sheet ref)))
      (unless (cell-writable-p cell) (error 'readonly-cell :ref ref))
      (morph-cell cell 'async-cell (cell-mixins cell))
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

;;; --- drivers: volatility (orthogonal attribute) ---------------------

(defun set-volatile (sheet designator volatile)
  "Mark (or unmark) DESIGNATOR volatile — recomputed on every sweep, whatever
its kind. Volatility is an attribute in the sheet registry, not a cell class,
so it composes with formula/external/async/observed cells alike."
  (with-sheet-lock (sheet)
    (set-cell-volatile sheet (parse-ref designator) volatile)
    volatile))

(defun set-readonly (sheet designator readonly)
  "Lock (or unlock) DESIGNATOR against user reassignment by adding/removing
READONLY-MIXIN. Composes with any value source and other mixins; the cell
still recomputes from its precedents while locked. This driver itself is the
escape hatch and is never blocked."
  (with-sheet-lock (sheet)
    (let ((cell (ensure-cell sheet (parse-ref designator))))
      (if readonly
          (add-mixin cell 'readonly-mixin)
          (remove-mixin cell 'readonly-mixin)))
    readonly))

;;; --- drivers: observation -------------------------------------------

(defun observe (sheet designator callback)
  "Register CALLBACK (a function of the new value) to fire whenever
DESIGNATOR's value changes after a sweep. Composes with the cell's existing
kind: OBSERVABLE-MIXIN is layered on in place, preserving value source, other
mixins, and dependency links."
  (with-sheet-lock (sheet)
    (let* ((ref (parse-ref designator))
           (cell (ensure-cell sheet ref)))
      (add-mixin cell 'observable-mixin)
      (pushnew callback (cell-subscribers cell))
      (values))))

(defun unobserve (sheet designator callback)
  "Remove a previously registered observer CALLBACK; when the last one is
gone, drop OBSERVABLE-MIXIN from the cell (keeping its value source and other
mixins)."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (parse-ref designator))))
      (when (typep cell 'observable-mixin)
        (setf (cell-subscribers cell)
              (remove callback (cell-subscribers cell)))
        (when (null (cell-subscribers cell))
          (remove-mixin cell 'observable-mixin))))
    (values)))
