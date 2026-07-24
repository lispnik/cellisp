;;;; async-pool.lisp — engine-owned async pool, cancellation, and disposal.
;;;;
;;;;   sbcl --script examples/async-pool.lisp
;;;;   ecl  --load   examples/async-pool.lisp
;;;;
;;;; With (set-async … :pool t) a fetcher is a plain BLOCKING thunk the engine
;;;; runs on its own bounded thread pool, delivering the result (or error). :pool
;;;; t uses the sheet's WORKBOOK pool, so the workbook owns those threads and
;;;; close-workbook cleans them up. cancel-async drops an in-flight fetch's
;;;; result (an epoch gate); with :cancelable t the fetcher also gets a
;;;; cancelled-p predicate to poll and abort the *actual work* early. No network —
;;;; a stub thunk stands in.

(require :asdf)
(asdf:initialize-source-registry            ; find cellisp + its ocicl deps under ./
 (list :source-registry (list :tree (uiop:getcwd)) :inherit-configuration))
(asdf:load-system "cellisp")
(in-package #:cellisp)

(defun settle (sheet ref &optional (timeout 3.0))    ; wait out an in-flight fetch
  (let ((end (+ (get-internal-real-time)
                (round (* timeout internal-time-units-per-second)))))
    (loop while (and (async-pending-p sheet ref) (< (get-internal-real-time) end))
          do (sleep 0.01))))

(let* ((wb (make-workbook)) (s (add-sheet wb "Data")))  ; the workbook owns the pool
  (unwind-protect
       (progn
         ;; --- pooled fetch: engine runs the blocking thunk, delivers the result
         (set-async s "A1" (lambda () (sleep 0.1) "fresh data")
                    :initial "(none)" :pool t)           ; :pool t -> workbook pool
         (set-cell s "A2" '(concatenate 'string "got: " (cell "A1")))
         (format t "~2&pooled fetch~%  before refresh : ~S~%" (async-status s "A1"))
         (refresh-async s "A1")
         (format t "  right after   : ~S  [engine ran it on a workbook-pool worker]~%"
                 (nth-value 0 (async-status s "A1")))
         (settle s "A1")
         (format t "  delivered     : A1=~S  A2=~S  status=~S~%"
                 (get-value s "A1") (get-value s "A2") (nth-value 0 (async-status s "A1")))

         ;; --- cancellation: a slow fetch's result is DROPPED after cancel
         (set-async s "C1" (lambda () (sleep 0.2) "STALE") :initial "kept" :pool t)
         (format t "~2&cancel (drop result)~%")
         (refresh-async s "C1")
         (cancel-async s "C1")
         (sleep 0.3)                                     ; let the cancelled thunk finish
         (format t "  late result DROPPED: C1=~S (never became \"STALE\")~%" (get-value s "C1"))

         ;; --- :cancelable: the fetcher polls cancelled-p and ABORTS the work early
         (let ((did-work 0))
           (set-async s "D1"
                      (lambda (cancelled-p)             ; 1-arg (pooled + cancelable)
                        (dotimes (i 1000)
                          (when (funcall cancelled-p) (return))  ; abort the loop
                          (incf did-work) (sleep 0.005))
                        "finished")
                      :initial "idle" :pool t :cancelable t)
           (format t "~2&cancel (abort the work)~%")
           (refresh-async s "D1")
           (sleep 0.05)                                 ; let it grind a few iterations
           (cancel-async s "D1")
           (settle s "D1")
           (sleep 0.03)
           (format t "  fetcher stopped after ~D iterations (not 1000) — real work aborted~%"
                   did-work))

         ;; --- error delivery: a failing pooled thunk lands as an error
         (set-async s "E1" (lambda () (error "503 upstream error")) :initial nil :pool t)
         (format t "~2&error delivery~%")
         (refresh-async s "E1")
         (settle s "E1")
         (multiple-value-bind (state err) (async-status s "E1")
           (format t "  failed fetch  : status=~S  error=~A~%" state err)))
    ;; --- teardown: the workbook owns the pool, so closing it joins the threads
    (close-workbook wb)
    (format t "~2&close-workbook — engine-owned worker threads joined.~%")))

#+sbcl (sb-ext:exit :code 0)
#+ecl  (si:quit 0)
