;;;; async-pool.lisp — the engine-owned async pool + cooperative cancellation.
;;;;
;;;;   sbcl --script examples/async-pool.lisp
;;;;   ecl  --load   examples/async-pool.lisp
;;;;
;;;; With (set-async … :pool P) a fetcher is a plain no-arg BLOCKING thunk: the
;;;; engine runs it on its own bounded thread pool, delivers the result (or its
;;;; error), and — unlike a fetcher that spawns its own thread — owns the thread
;;;; lifecycle, so shutdown-async-pool cleans them up. cancel-async drops an
;;;; in-flight fetch's result (an epoch gate); async-status / async-pending-p
;;;; report state for a UI. No network — a stub thunk stands in.

(require :asdf)
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
#+quicklisp (ql:quickload "bordeaux-threads" :silent t)
(asdf:load-system "cellisp")
(in-package #:cellisp)

(defun settle (sheet ref &optional (timeout 3.0))    ; wait out an in-flight fetch
  (let ((end (+ (get-internal-real-time)
                (round (* timeout internal-time-units-per-second)))))
    (loop while (and (async-pending-p sheet ref) (< (get-internal-real-time) end))
          do (sleep 0.01))))

(let ((s (make-sheet)) (pool (make-async-pool :size 3)))
  (unwind-protect
       (progn
         ;; --- pooled fetch: engine runs the blocking thunk, delivers the result
         (set-async s "A1" (lambda () (sleep 0.1) "fresh data")
                    :initial "(none)" :pool pool)
         (set-cell s "A2" '(concatenate 'string "got: " (cell "A1")))  ; a dependent
         (format t "~2&pooled fetch~%  before refresh : ~S~%" (async-status s "A1"))
         (refresh-async s "A1")
         (format t "  right after   : ~S  [engine ran it on a pool worker]~%"
                 (nth-value 0 (async-status s "A1")))
         (settle s "A1")
         (format t "  delivered     : A1=~S  A2=~S  status=~S~%"
                 (get-value s "A1") (get-value s "A2") (nth-value 0 (async-status s "A1")))

         ;; --- cancellation: a slow fetch's result is DROPPED after cancel
         (set-async s "C1" (lambda () (sleep 0.2) "STALE") :initial "kept" :pool pool)
         (format t "~2&cancellation~%")
         (refresh-async s "C1")
         (cancel-async s "C1")                        ; cancel before it delivers
         (format t "  after cancel  : status=~S  pending=~S~%"
                 (nth-value 0 (async-status s "C1")) (async-pending-p s "C1"))
         (sleep 0.3)                                  ; let the cancelled thunk finish
         (format t "  its late result was DROPPED: C1=~S (never became \"STALE\")~%"
                 (get-value s "C1"))

         ;; --- error delivery: a failing pooled thunk lands as an error
         (set-async s "E1" (lambda () (error "503 upstream error")) :initial nil :pool pool)
         (format t "~2&error delivery~%")
         (refresh-async s "E1")
         (settle s "E1")
         (multiple-value-bind (state err) (async-status s "E1")
           (format t "  failed fetch  : status=~S  error=~A~%" state err)))
    ;; --- teardown: the engine owns these threads, so it can clean them up
    (shutdown-async-pool pool)
    (format t "~2&pool shut down — engine-owned worker threads joined.~%")))

#+sbcl (sb-ext:exit :code 0)
#+ecl  (si:quit 0)
