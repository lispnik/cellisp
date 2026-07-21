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
            :documentation "In manual mode, a function of one arg (a DELIVER
callback) that starts a fetch and eventually calls it. In pooled mode (POOL set),
a plain no-arg blocking thunk the engine runs on a worker and delivers for you.")
   (pending :initform nil :accessor async-pending)
   ;; monotonically-increasing token: each REFRESH-ASYNC bumps it and gates the
   ;; delivery on it, so a cancelled or superseded fetch's late result is dropped.
   (epoch :initform 0 :accessor async-epoch)
   ;; NIL = manual (fetcher owns its thread); an ASYNC-POOL = the engine runs the
   ;; blocking fetcher on that pool and owns the thread lifecycle.
   (pool :initform nil :accessor async-pool)
   ;; When T, REFRESH-ASYNC passes the fetcher a trailing CANCELLED-P predicate it
   ;; can poll to abort the actual work early (not just have its result dropped).
   (cancelable :initform nil :accessor async-cancelable))
  (:documentation "A cell whose value arrives out-of-band. COMPUTE-VALUE returns
the last delivered value without blocking; REFRESH-ASYNC triggers the fetcher,
DELIVER-ASYNC / DELIVER-ERROR-ASYNC store the result, CANCEL-ASYNC drops an
in-flight one."))

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

(defmethod cell-writable-p ((cell readonly-mixin) &optional new-formula)
  (declare (ignore new-formula))
  nil)                                  ; strongest guard: no CALL-NEXT-METHOD

;;; Writable-policy mixins chain via CALL-NEXT-METHOD, so several AND together
;;; (and READONLY-MIXIN's nil short-circuits the whole conjunction).

(defclass append-only-mixin () ()
  (:documentation "Write-once: a set is allowed only while the cell is still
empty, so its formula can be established but never changed."))

(defmethod cell-writable-p ((cell append-only-mixin) &optional new-formula)
  (declare (ignore new-formula))
  (and (null (cell-formula cell)) (call-next-method)))

(defclass typed-input-mixin ()
  ((input-predicate :initform (constantly t) :accessor input-predicate))
  (:documentation "Reject a set whose new formula fails INPUT-PREDICATE (a
clear / source change, with no new formula, is allowed)."))

(defmethod cell-writable-p ((cell typed-input-mixin) &optional new-formula)
  (and (or (null new-formula) (funcall (input-predicate cell) new-formula))
       (call-next-method)))

(defclass versioned-mixin ()
  ((formula-versions :initform '() :accessor formula-versions))
  (:documentation "Record every formula assigned to the cell (most recent
first) via the NOTE-SET hook — an edit history. Read with CELL-VERSIONS."))

(defmethod note-set :after ((cell versioned-mixin) sheet ref new-formula actor time)
  (declare (ignore sheet ref actor time))
  (push new-formula (formula-versions cell)))

(defclass audited-mixin ()
  ((audit-log :initform '() :accessor audit-log))
  (:documentation "Full edit provenance: record (:time :actor :formula) for
every mutation via NOTE-SET, using the ACTOR and TIME threaded from the set
path (*actor* / *audit-clock*). Read with CELL-AUDIT."))

(defmethod note-set :after ((cell audited-mixin) sheet ref new-formula actor time)
  (declare (ignore sheet ref))
  (push (list :time time :actor actor :formula new-formula) (audit-log cell)))

;;; --- logging mixin (an :AFTER method on CELL-SWEPT) -----------------

(defclass logged-mixin ()
  ((history :initform '() :accessor cell-history)
   (log-limit :initform nil :accessor cell-log-limit))
  (:documentation "Mixin recording a cell's value history (most recent first,
consecutive duplicates collapsed; capped at LOG-LIMIT entries when set). Hooks
CELL-SWEPT with an :AFTER, so it stacks with OBSERVABLE-MIXIN's primary
CELL-SWEPT via method combination. Read the history with CELL-LOG."))

(defmethod cell-swept :after ((cell logged-mixin) sheet ref)
  (declare (ignore sheet ref))
  (let ((v (cell-value cell)))
    (when (or (null (cell-history cell))
              (not (equal v (first (cell-history cell)))))
      (push v (cell-history cell))
      (let ((lim (cell-log-limit cell)))
        (when (and lim (> (length (cell-history cell)) lim))
          (setf (cell-history cell) (subseq (cell-history cell) 0 lim)))))))

;;; --- caching mixin (an :AROUND method on COMPUTE-VALUE) -------------

(defclass cached-mixin ()
  ((signature :initform :unset :accessor cached-signature))
  (:documentation "Mixin that memoizes COMPUTE-VALUE: it re-runs the real
computation only when a precedent's value changed since the last run,
otherwise it returns the cached value (re-noting the precedents so dependency
links survive). Hooks COMPUTE-VALUE with an :AROUND method — a third
composition style — so it wraps whatever value source it is combined with."))

(defmethod compute-value :around ((cell cached-mixin) sheet ref)
  (declare (ignore ref))
  (let ((sig (cached-signature cell)))
    (if (and (listp sig)                ; a real snapshot exists (not :unset)
             (loop for (p . old) in sig
                   always (equal old (evaluate-ref sheet p))))
        ;; inputs unchanged: reuse the cached value, but re-note the
        ;; precedents so UPDATE-DEPENDENCY-LINKS keeps this cell in the graph
        (progn
          (dolist (entry sig) (note-precedent (car entry)))
          (cell-value cell))
        ;; changed (or first run): compute for real, then snapshot the freshly
        ;; observed precedents and their values from *COLLECTED-PRECEDENTS*
        (let ((v (call-next-method)))
          (setf (cached-signature cell)
                (loop for p being the hash-keys of *collected-precedents*
                      collect (cons p (let ((c (find-cell sheet p)))
                                        (and c (cell-value c))))))
          v))))

;;; --- debouncing mixin (coalesces bursts of changes) -----------------

(defparameter *debounce-delay* 0.05
  "Default debounce interval, in seconds, for DEFAULT-DEBOUNCE-SCHEDULER.")

(defun default-debounce-scheduler (thunk)
  "Run THUNK after *DEBOUNCE-DELAY* seconds on a background thread."
  (bt:make-thread (lambda () (sleep *debounce-delay*) (funcall thunk))))

(defclass debounced-mixin ()
  ;; Slot names are prefixed so they never merge with another mixin's slot
  ;; when the classes combine — e.g. OBSERVABLE-MIXIN also has a SUBSCRIBERS
  ;; slot, and a shared slot would cross the two subscriber lists.
  ((debounce-scheduler :initform #'default-debounce-scheduler
                       :accessor debounce-scheduler)
   (debounce-generation :initform 0 :accessor debounce-generation)
   (debounce-last-seen :initform *unset* :accessor debounce-last-seen)
   (debounce-subscribers :initform '() :accessor debounce-subscribers))
  (:documentation "Mixin that coalesces a burst of value changes into a single
trailing notification. Each change schedules a deferred fire (via SCHEDULER,
which runs its thunk after the debounce interval) and bumps a generation
counter; a scheduled fire runs only if no newer change arrived meanwhile. Like
LOGGED-MIXIN it hooks CELL-SWEPT with an :AFTER, but it carries scheduler state
and fires across threads under the sheet lock — the most stateful mixin."))

(defmethod cell-swept :after ((cell debounced-mixin) sheet ref)
  (declare (ignore ref))
  (let ((v (cell-value cell)))
    (unless (equal v (debounce-last-seen cell))
      (setf (debounce-last-seen cell) v)
      (let ((g (incf (debounce-generation cell))))
        (funcall (debounce-scheduler cell)
                 (lambda ()
                   (with-sheet-lock (sheet)
                     ;; fire only if this is still the latest change
                     (when (= g (debounce-generation cell))
                       (dolist (fn (debounce-subscribers cell))
                         (funcall fn (cell-value cell)))))))))))

;;; --- reactive sink mixins (:AFTER on CELL-SWEPT) -------------------

(defclass throttled-mixin ()
  ((throttle-interval :initform 0 :accessor throttle-interval)
   (throttle-clock :initform (lambda () (get-internal-real-time))
                   :accessor throttle-clock)
   (throttle-last :initform nil :accessor throttle-last)
   (throttle-last-seen :initform *unset* :accessor throttle-last-seen)
   (throttle-subscribers :initform '() :accessor throttle-subscribers))
  (:documentation "Leading-edge counterpart of DEBOUNCED-MIXIN: fire on a
change immediately, then suppress further fires for THROTTLE-INTERVAL (per
THROTTLE-CLOCK)."))

(defmethod cell-swept :after ((cell throttled-mixin) sheet ref)
  (declare (ignore sheet ref))
  (let ((v (cell-value cell)))
    (unless (equal v (throttle-last-seen cell))
      (setf (throttle-last-seen cell) v)
      (let ((now (funcall (throttle-clock cell))))
        (when (or (null (throttle-last cell))
                  (>= (- now (throttle-last cell)) (throttle-interval cell)))
          (setf (throttle-last cell) now)
          (dolist (fn (throttle-subscribers cell)) (funcall fn v)))))))

(defclass threshold-mixin ()
  ((threshold-level :initform 0 :accessor threshold-level)
   (threshold-side :initform nil :accessor threshold-side)
   (threshold-subscribers :initform '() :accessor threshold-subscribers))
  (:documentation "Fire only when the value crosses THRESHOLD-LEVEL, calling
subscribers with (SIDE VALUE) where SIDE is :ABOVE or :BELOW."))

(defmethod cell-swept :after ((cell threshold-mixin) sheet ref)
  (declare (ignore sheet ref))
  (let* ((v (cell-value cell))
         (side (if (and (realp v) (>= v (threshold-level cell))) :above :below)))
    (unless (eq side (threshold-side cell))
      (setf (threshold-side cell) side)
      (dolist (fn (threshold-subscribers cell)) (funcall fn side v)))))

(defclass stats-mixin ()
  ((stats-count :initform 0 :accessor stats-count)
   (stats-sum :initform 0 :accessor stats-sum)
   (stats-min :initform nil :accessor stats-min)
   (stats-max :initform nil :accessor stats-max))
  (:documentation "Accumulate running count/sum/min/max over the numeric
values the cell takes on each recompute. Read with CELL-STATS."))

(defmethod cell-swept :after ((cell stats-mixin) sheet ref)
  (declare (ignore sheet ref))
  (let ((v (cell-value cell)))
    (when (realp v)
      (incf (stats-count cell))
      (incf (stats-sum cell) v)
      (setf (stats-min cell) (if (stats-min cell) (min (stats-min cell) v) v)
            (stats-max cell) (if (stats-max cell) (max (stats-max cell) v) v)))))

(defclass persisted-mixin ()
  ((persist-sink :initform nil :accessor persist-sink)
   (persist-last-seen :initform *unset* :accessor persist-last-seen))
  (:documentation "A side-effecting output sink: call PERSIST-SINK with the
value whenever it changes (write it to a store, a socket, etc.) — the mirror
of EXTERNAL-CELL's input role."))

(defmethod cell-swept :after ((cell persisted-mixin) sheet ref)
  (declare (ignore sheet ref))
  (let ((v (cell-value cell)))
    (unless (equal v (persist-last-seen cell))
      (setf (persist-last-seen cell) v)
      (when (persist-sink cell) (funcall (persist-sink cell) v)))))

;;; --- value-wrapping mixins (:AROUND on COMPUTE-VALUE) ---------------
;;;
;;; Each wraps value production. Several may stack (they chain via
;;; CALL-NEXT-METHOD in class-precedence order); combine deliberately.

(defclass default-mixin ()
  ((default-value :initform nil :accessor cell-default))
  (:documentation "Return DEFAULT-VALUE instead of storing the error when the
computation fails (cycles still propagate)."))

(defmethod compute-value :around ((cell default-mixin) sheet ref)
  (declare (ignore sheet ref))
  (handler-case (call-next-method)
    (cyclic-reference (e) (error e))
    (error () (cell-default cell))))

(defclass transformed-mixin ()
  ((transform-fn :initform #'identity :accessor cell-transform))
  (:documentation "Post-process the computed value through TRANSFORM-FN — e.g.
clamp, round, coerce."))

(defmethod compute-value :around ((cell transformed-mixin) sheet ref)
  (declare (ignore sheet ref))
  (funcall (cell-transform cell) (call-next-method)))

(defclass validated-mixin ()
  ((validator-fn :initform (constantly t) :accessor cell-validator))
  (:documentation "Signal INVALID-VALUE when the computed value fails
VALIDATOR-FN."))

(defmethod compute-value :around ((cell validated-mixin) sheet ref)
  (let ((v (call-next-method)))
    (unless (funcall (cell-validator cell) v)
      (error 'invalid-value :ref ref :value v))
    v))

(defclass timed-mixin ()
  ((timed-total :initform 0 :accessor timed-total)
   (timed-count :initform 0 :accessor timed-count))
  (:documentation "Accumulate run time and count across recomputes, for
profiling. Read with CELL-TIMING."))

(defmethod compute-value :around ((cell timed-mixin) sheet ref)
  (declare (ignore sheet ref))
  (let ((t0 (get-internal-run-time)))
    (multiple-value-prog1 (call-next-method)
      (incf (timed-total cell) (- (get-internal-run-time) t0))
      (incf (timed-count cell)))))

(defclass retry-mixin ()
  ((retry-max :initform 2 :accessor retry-max))
  (:documentation "Re-run the computation up to RETRY-MAX times on error
before giving up — for transient failures in external/async cells."))

(defmethod compute-value :around ((cell retry-mixin) sheet ref)
  (declare (ignore sheet ref))
  (loop with n = (retry-max cell)
        for attempt from 0
        do (handler-case (return (call-next-method))
             (cyclic-reference (e) (error e))
             (error (e) (when (>= attempt n) (error e))))))

(defclass ttl-cached-mixin ()
  ((ttl :initform 0 :accessor cell-ttl)
   (ttl-clock :initform (lambda () (get-internal-real-time)) :accessor ttl-clock)
   (ttl-stamp :initform nil :accessor ttl-stamp)
   (ttl-precedents :initform '() :accessor ttl-precedents))
  (:documentation "Time-based memoization: reuse the last value for TTL units
(per TTL-CLOCK) before recomputing — e.g. rate-limit an external fetch."))

(defmethod compute-value :around ((cell ttl-cached-mixin) sheet ref)
  (declare (ignore sheet ref))
  (let ((now (funcall (ttl-clock cell))))
    (if (and (ttl-stamp cell) (< (- now (ttl-stamp cell)) (cell-ttl cell)))
        (progn (dolist (p (ttl-precedents cell)) (note-precedent p))
               (cell-value cell))
        (multiple-value-prog1 (call-next-method)
          (setf (ttl-stamp cell) now
                (ttl-precedents cell)
                (loop for p being the hash-keys of *collected-precedents*
                      collect p))))))

;;; --- composing value source + mixins --------------------------------

(defparameter *value-source-classes* '(external-cell async-cell cell)
  "Value-source cell classes, most specific first; a cell has exactly one.")

(defparameter *mixin-classes*
  '(observable-mixin readonly-mixin logged-mixin cached-mixin debounced-mixin
    default-mixin transformed-mixin validated-mixin timed-mixin retry-mixin
    ttl-cached-mixin throttled-mixin threshold-mixin stats-mixin persisted-mixin
    append-only-mixin typed-input-mixin versioned-mixin audited-mixin)
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
    (let* ((ref (resolve-ref-in sheet designator))
           (cell (ensure-cell sheet ref)))
      (unless (cell-writable-p cell) (error 'readonly-cell :ref ref))
      (morph-cell cell 'external-cell (cell-mixins cell))
      (setf (cell-source cell) source)
      (recompute-closure sheet (list ref))
      (cell-value cell))))

;;; --- an engine-owned thread pool for pooled async fetchers ----------
;;;
;;; Opt in with (SET-ASYNC … :POOL T). Then a fetcher is a plain no-arg blocking
;;; thunk, run on one of the pool's workers; the engine delivers its result (or
;;; error), and — unlike a fetcher that spawns its own thread — the engine owns
;;; these threads, so a bounded set is reused and SHUTDOWN-ASYNC-POOL cleans them
;;; up. A tiny FIFO queue (a lock + a counting semaphore) feeds the workers.

(defstruct (async-pool (:constructor %make-async-pool))
  (lock (bt:make-lock "cellisp-async-pool"))
  (sem (bt:make-semaphore))
  (queue '() :type list)
  (workers '() :type list)
  (running t))

(defvar *default-async-pool* nil "The lazily-created shared pool for :POOL T.")

(defun %pool-loop (pool)
  (loop
    (bt:wait-on-semaphore (async-pool-sem pool))
    (let ((task (bt:with-lock-held ((async-pool-lock pool))
                  (pop (async-pool-queue pool)))))
      (cond ((eq task :stop) (return))
            (task (ignore-errors (funcall task)))))))

(defun make-async-pool (&key (size 4))
  "Create an engine-owned pool of SIZE worker threads for pooled async fetchers
(see SET-ASYNC :POOL). Shut it down with SHUTDOWN-ASYNC-POOL."
  (let ((pool (%make-async-pool)))
    (setf (async-pool-workers pool)
          (loop repeat size
                collect (bt:make-thread (lambda () (%pool-loop pool))
                                        :name "cellisp-async-pool")))
    pool))

(defun default-async-pool ()
  (or *default-async-pool* (setf *default-async-pool* (make-async-pool))))

(defun workbook-async-pool (workbook)
  "WORKBOOK's engine-owned async pool, created on first use. Pooled async cells on
its sheets use it; CLOSE-WORKBOOK shuts it down."
  (or (workbook-pool workbook)
      (setf (workbook-pool workbook) (make-async-pool))))

(defun close-workbook (workbook)
  "Release engine-owned resources held by WORKBOOK — currently its async thread
pool (created for pooled async cells). Call at teardown; idempotent. Standalone
sheets (no workbook) use the shared pool instead — close that with
SHUTDOWN-ASYNC-POOL."
  (when (workbook-pool workbook)
    (shutdown-async-pool (workbook-pool workbook))
    (setf (workbook-pool workbook) nil))
  (values))

(defun %pool-submit (pool task)
  (bt:with-lock-held ((async-pool-lock pool))
    (setf (async-pool-queue pool) (nconc (async-pool-queue pool) (list task))))
  (bt:signal-semaphore (async-pool-sem pool)))

(defun shutdown-async-pool (&optional (pool *default-async-pool*))
  "Stop POOL's workers and join them (default: the shared pool). In-flight tasks
finish; queued ones may not run. Call at teardown so engine-owned threads don't
linger. No-op if POOL is NIL."
  (when pool
    (setf (async-pool-running pool) nil)
    (dolist (w (async-pool-workers pool)) (declare (ignore w))
      (%pool-submit pool :stop))                 ; one :stop per worker
    (dolist (w (async-pool-workers pool)) (ignore-errors (bt:join-thread w)))
    (setf (async-pool-workers pool) '())
    (when (eq pool *default-async-pool*) (setf *default-async-pool* nil)))
  (values))

;;; --- async drivers --------------------------------------------------

(defun set-async (sheet designator fetcher &key initial pool cancelable)
  "Install (or convert the cell at) DESIGNATOR as an ASYNC-CELL with FETCHER and
INITIAL value, preserving any mixins. Returns the initial value.

Without :POOL, FETCHER is the *manual* form — a function of one arg (a DELIVER
callback) that starts its own work (a thread, an event loop, …) and eventually
calls it; the fetcher owns that concurrency. With :POOL non-NIL, FETCHER is a
plain no-arg *blocking thunk*: REFRESH-ASYNC runs it on an engine-owned thread
pool and delivers its return value, or its error via DELIVER-ERROR-ASYNC — the
engine owns those worker threads. :POOL T uses the sheet's workbook pool (or the
shared default pool for a standalone sheet); or pass a pool from MAKE-ASYNC-POOL.

With :CANCELABLE T, REFRESH-ASYNC passes the fetcher a trailing CANCELLED-P
predicate (so manual fetchers are 2-arg `(deliver cancelled-p)`, pooled fetchers
1-arg `(cancelled-p)`); the fetcher polls it to abort the *actual work* early,
not merely have its result dropped."
  (with-sheet-lock (sheet)
    (let* ((ref (resolve-ref-in sheet designator))
           (cell (ensure-cell sheet ref)))
      (unless (cell-writable-p cell) (error 'readonly-cell :ref ref))
      (morph-cell cell 'async-cell (cell-mixins cell))
      (setf (async-fetcher cell) fetcher
            (async-pending cell) nil
            (async-cancelable cell) cancelable
            (async-pool cell) (cond ((null pool) nil)
                                    ((eq pool t)
                                     (if (sheet-workbook sheet)
                                         (workbook-async-pool (sheet-workbook sheet))
                                         (default-async-pool)))
                                    (t pool))
            (cell-value cell) initial
            (cell-err cell) nil)
      (recompute-closure sheet (cons ref (cell-dependents cell)))
      (cell-value cell))))

(defun refresh-async (sheet designator)
  "Trigger the async cell's fetcher, superseding any in-flight fetch (its late
result is dropped — last refresh wins). Bumps the cell's epoch and gates the
delivery on it. Manual mode hands the fetcher an epoch-gated DELIVER callback;
pooled mode submits the blocking thunk to the pool and delivers its result/error.
A :CANCELABLE cell's fetcher also gets a CANCELLED-P predicate (true once the
fetch is cancelled or superseded) to abort its own work."
  (with-sheet-lock (sheet)
    (let* ((ref (resolve-ref-in sheet designator))
           (cell (find-cell sheet ref)))
      (when (typep cell 'async-cell)
        (let* ((epoch (incf (async-epoch cell)))
               (pool (async-pool cell))
               (fetcher (async-fetcher cell))
               (cancelable (async-cancelable cell))
               ;; true once this fetch is superseded/cancelled (epoch moved on)
               (cancelled-p (lambda () (/= epoch (async-epoch cell))))
               (deliver (lambda (value) (deliver-async sheet ref value epoch))))
          (setf (async-pending cell) t)
          (if pool
              (%pool-submit pool
                (lambda ()
                  (handler-case
                      (deliver-async sheet ref
                                     (if cancelable (funcall fetcher cancelled-p)
                                         (funcall fetcher))
                                     epoch)
                    (error (e) (deliver-error-async sheet ref e epoch)))))
              (if cancelable
                  (funcall fetcher deliver cancelled-p)
                  (funcall fetcher deliver)))))))
  (values))

(defun cancel-async (sheet designator)
  "Cancel any in-flight fetch for DESIGNATOR: bump its epoch so a late delivery is
dropped, and clear PENDING so a fresh REFRESH-ASYNC can start. This cancels the
*effect* — it doesn't forcibly stop the underlying work, but a pooled fetch's
result is discarded when it arrives. No-op for a non-async/missing cell."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (resolve-ref-in sheet designator))))
      (when (typep cell 'async-cell)
        (incf (async-epoch cell))
        (setf (async-pending cell) nil))))
  (values))

(defun deliver-async (sheet designator value &optional epoch)
  "Store VALUE into an async cell (from the fetcher, any thread) and recompute its
dependents. When EPOCH is given, the delivery is dropped unless it matches the
cell's current epoch — so a cancelled or superseded fetch is ignored. No-op for a
non-async/missing cell."
  (with-sheet-lock (sheet)
    (let* ((ref (resolve-ref-in sheet designator))
           (cell (find-cell sheet ref)))
      (when (and (typep cell 'async-cell)
                 (or (null epoch) (= epoch (async-epoch cell))))
        (setf (cell-value cell) value
              (cell-err cell) nil
              (async-pending cell) nil)
        (recompute-closure sheet (cell-dependents cell)))))
  (values))

(defun deliver-error-async (sheet designator error &optional epoch)
  "Record a fetch FAILURE into an async cell (from the worker, any thread): store
ERROR (a condition or a string) as the cell's error, clear PENDING, and recompute
its dependents (which then error, as when reading any failed cell). Epoch-gated
like DELIVER-ASYNC. No-op for a non-async/missing cell."
  (with-sheet-lock (sheet)
    (let* ((ref (resolve-ref-in sheet designator))
           (cell (find-cell sheet ref)))
      (when (and (typep cell 'async-cell)
                 (or (null epoch) (= epoch (async-epoch cell))))
        (setf (cell-err cell)
              (cond ((typep error 'sheet-error) error)
                    ((typep error 'condition)
                     (make-condition 'cell-eval-error :ref ref :original error))
                    (t (make-condition 'sheet-error :format-control "~A"
                                       :format-arguments (list error))))
              (cell-value cell) nil
              (async-pending cell) nil)
        (recompute-closure sheet (cell-dependents cell)))))
  (values))

(defun async-pending-p (sheet designator)
  "True while a fetch for DESIGNATOR is in flight (started, not yet delivered)."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (resolve-ref-in sheet designator))))
      (and (typep cell 'async-cell) (async-pending cell) t))))

(defun async-status (sheet designator)
  "Two values: an async cell's state — :PENDING, :ERROR, :OK, or :IDLE — and its
value (or error condition). NIL for a non-async/missing cell."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (resolve-ref-in sheet designator))))
      (when (typep cell 'async-cell)
        (cond ((async-pending cell) (values :pending (cell-value cell)))
              ((cell-err cell)       (values :error (cell-err cell)))
              ((cell-value cell)     (values :ok (cell-value cell)))
              (t                     (values :idle nil)))))))

;;; --- drivers: volatility (orthogonal attribute) ---------------------

(defun set-volatile (sheet designator volatile)
  "Mark (or unmark) DESIGNATOR volatile — recomputed on every sweep, whatever
its kind. Volatility is an attribute in the sheet registry, not a cell class,
so it composes with formula/external/async/observed cells alike."
  (with-sheet-lock (sheet)
    (set-cell-volatile sheet (resolve-ref-in sheet designator) volatile)
    volatile))

(defun set-readonly (sheet designator readonly)
  "Lock (or unlock) DESIGNATOR against user reassignment by adding/removing
READONLY-MIXIN. Composes with any value source and other mixins; the cell
still recomputes from its precedents while locked. This driver itself is the
escape hatch and is never blocked."
  (with-sheet-lock (sheet)
    (let ((cell (ensure-cell sheet (resolve-ref-in sheet designator))))
      (if readonly
          (add-mixin cell 'readonly-mixin)
          (remove-mixin cell 'readonly-mixin)))
    readonly))

(defun set-logged (sheet designator logged &key limit)
  "Start (or stop) recording DESIGNATOR's value history, keeping at most LIMIT
entries (NIL = unbounded). Read the history back with CELL-LOG."
  (with-sheet-lock (sheet)
    (let ((cell (ensure-cell sheet (resolve-ref-in sheet designator))))
      (if logged
          (progn (add-mixin cell 'logged-mixin)
                 (setf (cell-log-limit cell) limit))
          (remove-mixin cell 'logged-mixin)))
    logged))

(defun cell-log (sheet designator)
  "DESIGNATOR's recorded value history, oldest first (empty unless the cell is
logged; consecutive duplicate values are collapsed)."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (resolve-ref-in sheet designator))))
      (if (typep cell 'logged-mixin)
          (reverse (cell-history cell))
          '()))))

(defun set-cached (sheet designator cached)
  "Enable (or disable) memoization on DESIGNATOR: while enabled it recomputes
only when a precedent's value changed since its last run. Composes with any
value source or other mixin."
  (with-sheet-lock (sheet)
    (let ((cell (ensure-cell sheet (resolve-ref-in sheet designator))))
      (if cached
          (add-mixin cell 'cached-mixin)
          (remove-mixin cell 'cached-mixin)))
    cached))

(defmacro define-mixin-toggle (name mixin &body setup)
  "Define a driver (NAME sheet designator ARG) that adds MIXIN (running SETUP,
which sees CELL and ARG) or, when ARG is NIL, removes it. Returns ARG."
  `(defun ,name (sheet designator arg)
     (with-sheet-lock (sheet)
       (let ((cell (ensure-cell sheet (resolve-ref-in sheet designator))))
         (cond (arg (add-mixin cell ',mixin) ,@setup)
               (t (remove-mixin cell ',mixin))))
       arg)))

(define-mixin-toggle set-default default-mixin
  (setf (cell-default cell) arg))                 ; arg is the default value

(define-mixin-toggle set-transform transformed-mixin
  (setf (cell-transform cell) arg))               ; arg is the transform fn

(define-mixin-toggle set-validator validated-mixin
  (setf (cell-validator cell) arg))               ; arg is the predicate

(define-mixin-toggle set-retry retry-mixin
  (setf (retry-max cell) arg))                    ; arg is the retry count

(define-mixin-toggle set-typed-input typed-input-mixin
  (setf (input-predicate cell) arg))              ; arg is the input predicate

(defun set-append-only (sheet designator append-only)
  "Make DESIGNATOR write-once (its formula can be set while empty but not
changed thereafter), or lift that restriction."
  (with-sheet-lock (sheet)
    (let ((cell (ensure-cell sheet (resolve-ref-in sheet designator))))
      (if append-only
          (add-mixin cell 'append-only-mixin)
          (remove-mixin cell 'append-only-mixin)))
    append-only))

(defun set-frozen (sheet designator frozen)
  "Freeze (or unfreeze) DESIGNATOR: a frozen cell is held at its current value
and skipped during recomputation, whatever changes around it. Volatility-like,
it is a registry attribute, not a class, so it composes with any cell."
  (with-sheet-lock (sheet)
    (let ((ref (resolve-ref-in sheet designator)))
      (if frozen
          (setf (gethash ref (sheet-frozen sheet)) t)
          (remhash ref (sheet-frozen sheet))))
    frozen))

(defun set-versioned (sheet designator versioned)
  "Start (or stop) recording DESIGNATOR's formula-edit history; seed it with
the current formula. Read the history with CELL-VERSIONS."
  (with-sheet-lock (sheet)
    (let ((cell (ensure-cell sheet (resolve-ref-in sheet designator))))
      (cond (versioned
             (add-mixin cell 'versioned-mixin)
             (when (and (cell-formula cell) (null (formula-versions cell)))
               (setf (formula-versions cell) (list (cell-formula cell)))))
            (t (remove-mixin cell 'versioned-mixin))))
    versioned))

(defun cell-versions (sheet designator)
  "The formulas assigned to DESIGNATOR over time, oldest first (empty unless
versioned)."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (resolve-ref-in sheet designator))))
      (if (typep cell 'versioned-mixin)
          (reverse (formula-versions cell))
          '()))))

(define-mixin-toggle set-audited audited-mixin)   ; arg is just the on/off flag

(defun cell-audit (sheet designator)
  "The audit trail for DESIGNATOR — a list of (:time T :actor A :formula F)
plists, oldest first (empty unless audited)."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (resolve-ref-in sheet designator))))
      (if (typep cell 'audited-mixin)
          (reverse (audit-log cell))
          '()))))

(defun set-timed (sheet designator timed)
  "Enable (or disable) per-recompute timing on DESIGNATOR; read CELL-TIMING."
  (with-sheet-lock (sheet)
    (let ((cell (ensure-cell sheet (resolve-ref-in sheet designator))))
      (if timed (add-mixin cell 'timed-mixin) (remove-mixin cell 'timed-mixin)))
    timed))

(defun cell-timing (sheet designator)
  "Return (values total-run-time run-count) for a timed cell, else NIL,NIL."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (resolve-ref-in sheet designator))))
      (if (typep cell 'timed-mixin)
          (values (timed-total cell) (timed-count cell))
          (values nil nil)))))

(defun set-ttl (sheet designator ttl &key (clock nil clock-supplied))
  "Cache DESIGNATOR's value for TTL time units (0 / NIL disables). CLOCK is a
thunk returning the current time (default: GET-INTERNAL-REAL-TIME)."
  (with-sheet-lock (sheet)
    (let ((cell (ensure-cell sheet (resolve-ref-in sheet designator))))
      (cond ((and ttl (plusp ttl))
             (add-mixin cell 'ttl-cached-mixin)
             (setf (cell-ttl cell) ttl (ttl-stamp cell) nil)
             (when clock-supplied (setf (ttl-clock cell) clock)))
            (t (remove-mixin cell 'ttl-cached-mixin))))
    ttl))

(defun throttle (sheet designator callback
                 &key (interval 0) (clock nil clock-supplied))
  "Subscribe CALLBACK but leading-edge throttle it: fire on a change
immediately, then suppress for INTERVAL (per CLOCK, default real time)."
  (with-sheet-lock (sheet)
    (let ((cell (ensure-cell sheet (resolve-ref-in sheet designator))))
      (add-mixin cell 'throttled-mixin)
      (setf (throttle-interval cell) interval)
      (when clock-supplied (setf (throttle-clock cell) clock))
      (pushnew callback (throttle-subscribers cell))
      (values))))

(defun on-threshold (sheet designator level callback)
  "Subscribe CALLBACK to fire only when DESIGNATOR crosses LEVEL, with
arguments (:above/:below value)."
  (with-sheet-lock (sheet)
    (let ((cell (ensure-cell sheet (resolve-ref-in sheet designator))))
      (add-mixin cell 'threshold-mixin)
      (setf (threshold-level cell) level)
      ;; seed the side from the current value so only genuine crossings fire
      (let ((v (cell-value cell)))
        (setf (threshold-side cell)
              (if (and (realp v) (>= v level)) :above :below)))
      (pushnew callback (threshold-subscribers cell))
      (values))))

(defun set-stats (sheet designator stats)
  "Enable (or disable) running count/sum/min/max on DESIGNATOR; read CELL-STATS."
  (with-sheet-lock (sheet)
    (let ((cell (ensure-cell sheet (resolve-ref-in sheet designator))))
      (if stats (add-mixin cell 'stats-mixin) (remove-mixin cell 'stats-mixin)))
    stats))

(defun cell-stats (sheet designator)
  "Return a plist (:count :sum :min :max :mean) for a stats cell, else NIL."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (resolve-ref-in sheet designator))))
      (when (typep cell 'stats-mixin)
        (list :count (stats-count cell) :sum (stats-sum cell)
              :min (stats-min cell) :max (stats-max cell)
              :mean (when (plusp (stats-count cell))
                      (/ (stats-sum cell) (stats-count cell))))))))

(defun set-persist (sheet designator sink)
  "Call SINK (a function of the value) whenever DESIGNATOR changes; NIL to
disable."
  (with-sheet-lock (sheet)
    (let ((cell (ensure-cell sheet (resolve-ref-in sheet designator))))
      (cond (sink (add-mixin cell 'persisted-mixin)
                  (setf (persist-sink cell) sink))
            (t (remove-mixin cell 'persisted-mixin))))
    sink))

(defun debounce (sheet designator callback
                 &key (scheduler #'default-debounce-scheduler))
  "Subscribe CALLBACK to DESIGNATOR's value, but coalesce bursts: it fires
once, with the settled value, after changes stop — each change (re)schedules
the fire via SCHEDULER (a function of a thunk that runs it after the debounce
interval). Promotes the cell to carry DEBOUNCED-MIXIN."
  (with-sheet-lock (sheet)
    (let ((cell (ensure-cell sheet (resolve-ref-in sheet designator))))
      (add-mixin cell 'debounced-mixin)
      (setf (debounce-scheduler cell) scheduler)
      (pushnew callback (debounce-subscribers cell))
      (values))))

;;; --- drivers: observation -------------------------------------------

(defun observe (sheet designator callback)
  "Register CALLBACK (a function of the new value) to fire whenever
DESIGNATOR's value changes after a sweep. Composes with the cell's existing
kind: OBSERVABLE-MIXIN is layered on in place, preserving value source, other
mixins, and dependency links."
  (with-sheet-lock (sheet)
    (let* ((ref (resolve-ref-in sheet designator))
           (cell (ensure-cell sheet ref)))
      (add-mixin cell 'observable-mixin)
      (pushnew callback (cell-subscribers cell))
      (values))))

(defun unobserve (sheet designator callback)
  "Remove a previously registered observer CALLBACK; when the last one is
gone, drop OBSERVABLE-MIXIN from the cell (keeping its value source and other
mixins)."
  (with-sheet-lock (sheet)
    (let ((cell (find-cell sheet (resolve-ref-in sheet designator))))
      (when (typep cell 'observable-mixin)
        (setf (cell-subscribers cell)
              (remove callback (cell-subscribers cell)))
        (when (null (cell-subscribers cell))
          (remove-mixin cell 'observable-mixin))))
    (values)))
