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
;;;; lists). Before writing, the produced form is validated by reading it back
;;;; (see %SERIALIZE-FORM): a value that is not readably printable (a closure,
;;;; hash-table, structure) signals a SHEET-ERROR at save time — before the
;;;; target file is touched — rather than silently writing a file that fails
;;;; only on load. On read, *READ-EVAL* is bound NIL so a #. cannot execute.
;;;; ------------------------------------------------------------------

(defparameter *serialization-version* 1)

(defun %check-version (version what)
  "Signal if VERSION (from a loaded file) is newer than this build can read.
Older or missing versions load as before; a newer one fails loudly rather than
silently discarding fields it doesn't understand."
  (when (and version (integerp version) (> version *serialization-version*))
    (error 'sheet-error
           :format-control "~A file version ~A is newer than this build supports (~A)"
           :format-arguments (list what version *serialization-version*))))

(defun fn-ref (x)
  "X when it is a non-NIL symbol naming a function (serializable as a
reference), else NIL — an anonymous closure can't be written to a file, so
mixin config must use *named* functions to round-trip. FUNCALL already accepts
a symbol, so the mixins consume a symbol slot transparently."
  (and x (symbolp x) x))

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
    ;; symbol-referenced config: written only when the function slot holds a
    ;; named function (a live lambda is skipped — reattach it after loading).
    (when (typep cell 'external-cell)
      (let ((r (fn-ref (cell-source cell)))) (when r (setf (getf pl :external) r))))
    (when (typep cell 'async-cell)
      (let ((r (fn-ref (async-fetcher cell)))) (when r (setf (getf pl :async) r)))
      (setf (getf pl :async-initial) (cell-value cell)))
    (when (typep cell 'validated-mixin)
      (let ((r (fn-ref (cell-validator cell)))) (when r (setf (getf pl :validator) r))))
    (when (typep cell 'transformed-mixin)
      (let ((r (fn-ref (cell-transform cell)))) (when r (setf (getf pl :transform) r))))
    (when (typep cell 'typed-input-mixin)
      (let ((r (fn-ref (input-predicate cell)))) (when r (setf (getf pl :typed-input) r))))
    (when (typep cell 'persisted-mixin)
      (let ((r (fn-ref (persist-sink cell)))) (when r (setf (getf pl :sink) r))))
    (when (typep cell 'observable-mixin)
      (let ((syms (remove nil (remove-if-not #'symbolp (cell-subscribers cell)))))
        (when syms (setf (getf pl :observers) syms))))
    (when (typep cell 'threshold-mixin)
      (let ((syms (remove nil (remove-if-not #'symbolp (threshold-subscribers cell)))))
        (when syms (setf (getf pl :threshold-level) (threshold-level cell)
                         (getf pl :threshold) syms))))
    ;; simple data config (no closures)
    (when (typep cell 'default-mixin)
      (setf (getf pl :default) t (getf pl :default-value) (cell-default cell)))
    (when (typep cell 'retry-mixin)      (setf (getf pl :retry) (retry-max cell)))
    (when (typep cell 'ttl-cached-mixin) (setf (getf pl :ttl) (cell-ttl cell)))
    pl))

(defun sheet->form (sheet)
  "Return a readable form capturing SHEET's reconstructable state."
  (with-sheet-lock (sheet)
    (let ((cells '()))
      (map-cells (lambda (ref cell) (push (cell->plist sheet ref cell) cells))
                 sheet)
      (list :cellisp-sheet
            :version *serialization-version*
            ;; copy the alist so a caller editing the form can't mutate the
            ;; sheet's live environment (or a shared quoted constant)
            :environment (copy-alist (sheet-environment sheet))
            ;; Each entry is (name . "A1") for a single cell, or
            ;; (name "A1" "B3") for a range — form->sheet dispatches on shape.
            :names (let ((acc '()))
                     (maphash (lambda (name val)
                                (push (if (%range-value-p val)
                                          (list name (ref-string (car val))
                                                (ref-string (cdr val)))
                                          (cons name (ref-string val)))
                                      acc))
                              (sheet-names sheet))
                     acc)
            ;; cell notes: (ref-string . "note text")
            :notes (let ((acc '()))
                     (maphash (lambda (ref note)
                                (push (cons (ref-string ref) note) acc))
                              (sheet-notes sheet))
                     acc)
            ;; merges: ("A1" . "B3") corner strings
            :merges (loop for (a . b) in (sheet-merges sheet)
                          collect (cons (ref-string a) (ref-string b)))
            ;; spill extents: ("A1" rows cols) so RESPILL works after a reload
            :spills (let ((acc '()))
                      (maphash (lambda (ref extent)
                                 (push (list (ref-string ref)
                                             (car extent) (cdr extent))
                                       acc))
                               (sheet-spills sheet))
                      acc)
            :cells (nreverse cells)))))

(defun form->sheet (form)
  "Reconstruct a sheet from a form produced by SHEET->FORM."
  (unless (and (consp form) (eq (car form) :cellisp-sheet))
    (error 'sheet-error :format-control "Not a cellisp sheet form: ~S"
                        :format-arguments (list form)))
  (destructuring-bind (&key version environment names notes merges spills cells
                       &allow-other-keys)
      (cdr form)
    (%check-version version "Sheet")
    (let ((sheet (make-sheet :environment environment)))
      (dolist (pair names)
        ;; (name . "A1") is a single cell; (name "A1" "B3") is a range.
        (if (consp (cdr pair))
            (set-range sheet (car pair) (second pair) (third pair))
            (set-name sheet (car pair) (cdr pair))))
      (dolist (pair notes) (set-note sheet (car pair) (cdr pair)))
      (dolist (pair merges) (merge-cells sheet (car pair) (cdr pair)))
      (dolist (sp spills)     ; (anchor-string rows cols)
        (setf (gethash (parse-ref (first sp)) (sheet-spills sheet))
              (cons (second sp) (third sp))))
      ;; 1. create cells and apply value-wrapping / source config FIRST, so
      ;;    they are active when the formulas recompute below.
      (dolist (pl cells)
        (let ((ref (getf pl :ref)))
          (ensure-cell sheet (parse-ref ref))
          (when (getf pl :validator) (set-validator sheet ref (getf pl :validator)))
          (when (getf pl :transform) (set-transform sheet ref (getf pl :transform)))
          (when (getf pl :default)   (set-default sheet ref (getf pl :default-value)))
          (when (getf pl :retry)     (set-retry sheet ref (getf pl :retry)))
          (when (getf pl :ttl)       (set-ttl sheet ref (getf pl :ttl)))
          (when (getf pl :external)  (set-external sheet ref (getf pl :external)))
          (when (getf pl :async)     (set-async sheet ref (getf pl :async)
                                                 :initial (getf pl :async-initial)))))
      ;; 2. install every formula (forward references resolve, one sweep with
      ;;    the wrappers above already in place).
      (set-cells sheet (loop for pl in cells
                             collect (list (getf pl :ref) (getf pl :formula))))
      ;; 3. declarative attributes and write guards; READONLY / APPEND-ONLY /
      ;;    TYPED-INPUT go last, so a guard never blocks installing the cell's
      ;;    own formula above.
      (dolist (pl cells)
        (let ((ref (getf pl :ref)))
          (when (getf pl :volatile)    (set-volatile sheet ref t))
          (when (getf pl :frozen)      (set-frozen sheet ref t))
          (when (getf pl :append-only) (set-append-only sheet ref t))
          (when (getf pl :typed-input) (set-typed-input sheet ref (getf pl :typed-input)))
          (when (getf pl :readonly)    (set-readonly sheet ref t))))
      ;; 4. restore durable history: add the mixin, then load its slot(s). No
      ;;    recompute happens after this, so the restored state stands as-is.
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
      ;; 5. re-attach reactive sinks (persist, observers) last — they hook
      ;;    :after CELL-SWEPT, and no recompute follows, so nothing fires on
      ;;    load; only future changes reach them.
      (dolist (pl cells)
        (let ((ref (getf pl :ref)))
          (when (getf pl :sink) (set-persist sheet ref (getf pl :sink)))
          (dolist (obs (getf pl :observers)) (observe sheet ref obs))
          (dolist (fn (getf pl :threshold))
            (on-threshold sheet ref (getf pl :threshold-level) fn))))
      sheet)))

(defun %serialize-form (form)
  "Print FORM to a string with the serialization printer settings, then verify it
reads back (with *READ-EVAL* NIL) — so a value that is not readably printable (a
closure, hash-table, or structure that slipped into a formula or the environment)
fails loudly HERE, with a clear SHEET-ERROR, instead of silently producing a file
that errors only on load. Returns the validated string.

*PRINT-ESCAPE* (not *PRINT-READABLY*) keeps strings clean as \"A1\" rather than
SBCL's base-string #A(...) syntax; our data (numbers/strings/symbols/lists)
round-trips either way, so the explicit read-back is what enforces readability."
  (let ((s (let ((*package* (find-package '#:cellisp))
                 (*print-escape* t)
                 (*print-readably* nil)
                 (*print-circle* t))
             (prin1-to-string form))))
    (handler-case
        (let ((*package* (find-package '#:cellisp))
              (*read-eval* nil))
          (read-from-string s))
      (error (e)
        (error 'sheet-error
               :format-control "Sheet is not serializable (a value is not ~
readably printable): ~A"
               :format-arguments (list e))))
    s))

(defun write-sheet (sheet &optional (stream *standard-output*))
  "Write SHEET to STREAM as one readable form. Signals a SHEET-ERROR if a formula
or environment value is not readably printable (validated before anything is
written — see %SERIALIZE-FORM)."
  (write-string (%serialize-form (sheet->form sheet)) stream)
  (terpri stream)
  (values))

(defun read-sheet (&optional (stream *standard-input*))
  "Read and reconstruct a sheet previously written by WRITE-SHEET.

A saved sheet is data, not code: *READ-EVAL* is bound to NIL so a #. reader
macro in a hand-crafted or tampered file cannot execute at read time. (Formulas
still evaluate later, on recompute — the documented \"formulas are unsandboxed\"
caveat — but the read step itself stays inert.)"
  (let ((*package* (find-package '#:cellisp))
        (*read-eval* nil))
    (form->sheet (read stream))))

(defun save-sheet (sheet path)
  "Write SHEET to the file at PATH, overwriting any existing file. The sheet is
serialized and validated (see %SERIALIZE-FORM) *before* PATH is opened, so an
unserializable sheet signals without first truncating an existing good file."
  (let ((content (%serialize-form (sheet->form sheet))))
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (write-string content s)
      (terpri s)))
  (values))

(defun load-sheet (path)
  "Load and reconstruct a sheet from the file at PATH."
  (with-open-file (s path :direction :input)
    (read-sheet s)))

;;;; ------------------------------------------------------------------
;;;; Workbook serialization
;;;;
;;;; A workbook is written as its ordered sheets, each reusing SHEET->FORM (so a
;;;; cell's cross-sheet formula "Data!A1" is just its ordinary readable form).
;;;; On load every sheet is rebuilt standalone first — cross-sheet references
;;;; error transiently, harmlessly, since siblings aren't present yet — then all
;;;; are attached and RECOMPUTE-WORKBOOK settles the cross-sheet values.
;;;; ------------------------------------------------------------------

(defun workbook->form (workbook)
  "A readable form capturing WORKBOOK: its sheets in order, each (name . form)."
  (list :cellisp-workbook
        :version *serialization-version*
        :sheets (loop for s in (workbook-sheets workbook)
                      collect (cons (sheet-name s) (sheet->form s)))))

(defun form->workbook (form)
  "Reconstruct a workbook from a form produced by WORKBOOK->FORM."
  (unless (and (consp form) (eq (car form) :cellisp-workbook))
    (error 'sheet-error :format-control "Not a cellisp workbook form: ~S"
                        :format-arguments (list form)))
  (destructuring-bind (&key version sheets &allow-other-keys) (cdr form)
    (%check-version version "Workbook")
    (let ((wb (make-workbook)))
      ;; rebuild each sheet standalone (cross-sheet refs error transiently),
      ;; then attach under its name without a premature recompute.
      (dolist (entry sheets)
        (%attach-sheet wb (car entry) (form->sheet (cdr entry))))
      ;; now that every sheet and cross-reference is present, settle them.
      (recompute-workbook wb)
      wb)))

(defun write-workbook (workbook &optional (stream *standard-output*))
  "Write WORKBOOK to STREAM as one readable form. Signals a SHEET-ERROR (before
writing anything) if any value is not readably printable — see %SERIALIZE-FORM."
  (write-string (%serialize-form (workbook->form workbook)) stream)
  (terpri stream)
  (values))

(defun read-workbook (&optional (stream *standard-input*))
  "Read and reconstruct a workbook previously written by WRITE-WORKBOOK.

As in READ-SHEET, *READ-EVAL* is bound to NIL so a #. in the file cannot execute
at read time."
  (let ((*package* (find-package '#:cellisp))
        (*read-eval* nil))
    (form->workbook (read stream))))

(defun save-workbook (workbook path)
  "Write WORKBOOK to the file at PATH, overwriting any existing file. Serialized
and validated before PATH is opened, so an unserializable workbook signals
without first truncating an existing good file (see SAVE-SHEET)."
  (let ((content (%serialize-form (workbook->form workbook))))
    (with-open-file (s path :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
      (write-string content s)
      (terpri s)))
  (values))

(defun load-workbook (path)
  "Load and reconstruct a workbook from the file at PATH."
  (with-open-file (s path :direction :input)
    (read-workbook s)))
