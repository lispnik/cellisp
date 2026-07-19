(in-package #:cellisp)

;;;; ------------------------------------------------------------------
;;;; Serialization
;;;;
;;;; A sheet is written as a single READABLE form capturing what can be
;;;; reconstructed by recomputation: the environment and, per non-empty
;;;; cell, its formula plus the *declarative* attributes it carries
;;;; (volatile, frozen, readonly, append-only). Values and errors are NOT
;;;; stored — they are recomputed on load.
;;;;
;;;; Out of scope (by nature): closure-based mixin configuration — external
;;;; sources, validators, transforms, callbacks, schedulers, clocks — cannot
;;;; be written to a file; reattach those after loading. Formulas and the
;;;; environment must be READABLY printable (numbers, strings, symbols,
;;;; lists); WRITE-SHEET binds *print-readably* so anything else fails loudly
;;;; rather than round-tripping silently wrong.
;;;; ------------------------------------------------------------------

(defparameter *serialization-version* 1)

(defun cell->plist (sheet ref cell)
  "A serializable plist for one cell: :REF, :FORMULA, its declarative
attributes, and any *durable history* it carries (audit log, formula versions,
value log, stats). Transient mixin state (cache signatures, timers, counters)
and closure-based config are not written."
  (let ((pl (list :ref (ref-string ref) :formula (cell-formula cell))))
    ;; declarative attributes
    (when (gethash ref (sheet-volatiles sheet)) (setf (getf pl :volatile) t))
    (when (gethash ref (sheet-frozen sheet))    (setf (getf pl :frozen) t))
    (when (typep cell 'readonly-mixin)          (setf (getf pl :readonly) t))
    (when (typep cell 'append-only-mixin)       (setf (getf pl :append-only) t))
    ;; durable history (stored oldest-first for readability)
    (when (typep cell 'versioned-mixin)
      (setf (getf pl :versioned) t)
      (setf (getf pl :versions) (reverse (formula-versions cell))))
    (when (typep cell 'audited-mixin)
      (setf (getf pl :audited) t)
      (setf (getf pl :audit) (reverse (audit-log cell))))
    (when (typep cell 'logged-mixin)
      (setf (getf pl :logged) t)
      (setf (getf pl :log) (reverse (cell-history cell)))
      (setf (getf pl :log-limit) (cell-log-limit cell)))
    (when (typep cell 'stats-mixin)
      (setf (getf pl :stats)
            (list :count (stats-count cell) :sum (stats-sum cell)
                  :min (stats-min cell) :max (stats-max cell))))
    pl))

(defun sheet->form (sheet)
  "Return a readable form capturing SHEET's reconstructable state."
  (with-sheet-lock (sheet)
    (let ((cells '()))
      (map-cells (lambda (ref cell) (push (cell->plist sheet ref cell) cells))
                 sheet)
      (list :cellisp-sheet
            :version *serialization-version*
            :environment (sheet-environment sheet)
            :cells (nreverse cells)))))

(defun form->sheet (form)
  "Reconstruct a sheet from a form produced by SHEET->FORM."
  (unless (and (consp form) (eq (car form) :cellisp-sheet))
    (error 'sheet-error :format-control "Not a cellisp sheet form: ~S"
                        :format-arguments (list form)))
  (destructuring-bind (&key version environment cells &allow-other-keys) (cdr form)
    (declare (ignore version))
    (let ((sheet (make-sheet :environment environment)))
      ;; install every formula first (forward references resolve, one sweep)
      (set-cells sheet (loop for pl in cells
                             collect (list (getf pl :ref) (getf pl :formula))))
      ;; then apply declarative attributes; READONLY / APPEND-ONLY go last, so
      ;; locking a cell never blocks installing its own formula above.
      (dolist (pl cells)
        (let ((ref (getf pl :ref)))
          (when (getf pl :volatile)    (set-volatile sheet ref t))
          (when (getf pl :frozen)      (set-frozen sheet ref t))
          (when (getf pl :append-only) (set-append-only sheet ref t))
          (when (getf pl :readonly)    (set-readonly sheet ref t))))
      ;; restore durable history: add the mixin, then load its slot(s). No
      ;; recompute happens after this, so the restored state stands as-is.
      (dolist (pl cells)
        (let ((cell (find-cell sheet (parse-ref (getf pl :ref)))))
          (when cell
            (when (getf pl :versioned)
              (add-mixin cell 'versioned-mixin)
              (setf (formula-versions cell) (reverse (getf pl :versions))))
            (when (getf pl :audited)
              (add-mixin cell 'audited-mixin)
              (setf (audit-log cell) (reverse (getf pl :audit))))
            (when (getf pl :logged)
              (add-mixin cell 'logged-mixin)
              (setf (cell-history cell) (reverse (getf pl :log))
                    (cell-log-limit cell) (getf pl :log-limit)))
            (let ((st (getf pl :stats)))
              (when st
                (add-mixin cell 'stats-mixin)
                (setf (stats-count cell) (getf st :count)
                      (stats-sum cell)   (getf st :sum)
                      (stats-min cell)   (getf st :min)
                      (stats-max cell)   (getf st :max)))))))
      sheet)))

(defun write-sheet (sheet &optional (stream *standard-output*))
  "Write SHEET to STREAM as one readable form. Signals if a formula or
environment value is not readably printable."
  ;; *print-escape* (not *print-readably*) keeps strings clean as "A1" rather
  ;; than SBCL's base-string #A(...) syntax; our data (numbers/strings/symbols/
  ;; lists) round-trips either way, and anything unreadable fails at READ.
  (let ((*package* (find-package '#:cellisp))
        (*print-escape* t)
        (*print-readably* nil)
        (*print-circle* t))
    (prin1 (sheet->form sheet) stream)
    (terpri stream))
  (values))

(defun read-sheet (&optional (stream *standard-input*))
  "Read and reconstruct a sheet previously written by WRITE-SHEET."
  (let ((*package* (find-package '#:cellisp)))
    (form->sheet (read stream))))

(defun save-sheet (sheet path)
  "Write SHEET to the file at PATH, overwriting any existing file."
  (with-open-file (s path :direction :output
                          :if-exists :supersede :if-does-not-exist :create)
    (write-sheet sheet s))
  (values))

(defun load-sheet (path)
  "Load and reconstruct a sheet from the file at PATH."
  (with-open-file (s path :direction :input)
    (read-sheet s)))
