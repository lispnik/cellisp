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
  "A serializable plist for one cell: :REF, :FORMULA, and any declarative
attributes it carries."
  (let ((pl (list :ref (ref-string ref) :formula (cell-formula cell))))
    (when (gethash ref (sheet-volatiles sheet)) (setf (getf pl :volatile) t))
    (when (gethash ref (sheet-frozen sheet))    (setf (getf pl :frozen) t))
    (when (typep cell 'readonly-mixin)          (setf (getf pl :readonly) t))
    (when (typep cell 'append-only-mixin)       (setf (getf pl :append-only) t))
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
