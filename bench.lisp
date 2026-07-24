;;;; bench.lisp — a standing benchmark harness for Cellisp.
;;;;
;;;; Not part of the test system (this measures speed, not correctness). Run it
;;;; straight from the shell; it loads Quicklisp + the system, then prints a
;;;; timing table. Numbers are wall-to-wall run time and vary by machine and
;;;; implementation — the point is relative baselines, especially the
;;;; change-propagation short-circuit's payoff, which is reported as a speedup.
;;;;
;;;;   sbcl  --script bench.lisp
;;;;   ecl   --load  bench.lisp
;;;;
;;;; Optionally pass a scale factor (default 1) to grow every workload:
;;;;   sbcl --script bench.lisp 4

(asdf:initialize-source-registry            ; find cellisp + its ocicl deps under ./
 (list :source-registry (list :tree (uiop:getcwd)) :inherit-configuration))
(asdf:load-system "cellisp")

(in-package #:cellisp)

;;; --- timing plumbing -------------------------------------------------------

(defun %ms (ticks)
  (/ (* 1000.0d0 ticks) internal-time-units-per-second))

(defparameter *scale*
  ;; First command-line arg, if it parses as a positive real, scales workloads.
  (let* ((args #+sbcl (cdr sb-ext:*posix-argv*)
               #+ecl  (cdr (si:command-args))
               #-(or sbcl ecl) '())
         (n (and args (ignore-errors (read-from-string (first args))))))
    (if (and (realp n) (plusp n)) n 1))
  "Multiplier applied to every workload size; from the command line, default 1.")

(defmacro timed ((&key (reps 1)) &body body)
  "Run BODY REPS times; return internal-time ticks elapsed."
  (let ((r (gensym)) (t0 (gensym)))
    `(let ((,t0 (get-internal-run-time)))
       (dotimes (,r ,reps) ,@body)
       (- (get-internal-run-time) ,t0))))

(defparameter *rows* '())
(defun row (name ticks &optional note)
  ;; TICKS nil => a derived/summary row with no time of its own.
  (push (list name (and ticks (%ms ticks)) note) *rows*))

(defun report ()
  (format t "~&~%~74,,,'-<~>~%")
  (format t "  Cellisp benchmark   (scale=~A, ~A ~A)~%"
          *scale* (lisp-implementation-type) (lisp-implementation-version))
  (format t "~74,,,'-<~>~%")
  (format t "  ~34A ~12@A   ~A~%" "benchmark" "time (ms)" "note")
  (format t "~74,,,'-<~>~%")
  (dolist (r (reverse *rows*))
    (destructuring-bind (name ms note) r
      (format t "  ~34A ~12@A   ~A~%"
              name (if ms (format nil "~,2F" ms) "") (or note ""))))
  (format t "~74,,,'-<~>~%"))

(defun scaled (n) (max 1 (round (* n *scale*))))

;;; --- workloads -------------------------------------------------------------

;; 1. Build throughput: set N independent literal cells (grows the cells table).
(defun bench-build ()
  (let* ((n (scaled 4000))
         (s (make-sheet))
         (ticks (timed ()
                  (dotimes (i n) (set-cell s (make-ref i 0) i)))))
    (row "build: set N literal cells" ticks
         (format nil "N=~D  (~,1F k/s)" n (/ n (max 1d-3 (/ (%ms ticks) 1000)) 1000)))))

;; 2. Chain recompute: a length-L reference chain; edit the head, forcing the
;;    whole chain to recompute each time. Classic propagate-everything cost.
(defun bench-chain ()
  (let* ((len (scaled 300)) (edits (scaled 40))
         (s (make-sheet)))
    (set-cell s (make-ref 0 0) 1)
    (loop for i from 1 below len do
      (set-cell s (make-ref i 0) `(+ (cell ,(ref-string (make-ref (1- i) 0))) 1)))
    (let ((ticks (timed () (dotimes (k edits) (set-cell s (make-ref 0 0) k)))))
      (row "chain: edit head, full propagate" ticks
           (format nil "len=~D x ~D edits" len edits)))))

;; 3. The short-circuit, measured directly. A wide cone of W cells all read a
;;    GATE whose value only changes when the input clears a high threshold.
;;    - pruned regime:      input stays under the gate -> gate value is stable
;;                          -> every cone cell is short-circuited (not recomputed)
;;    - propagate regime:   input clears the gate each edit -> whole cone recomputes
;;    Same edit count both times; the ratio is the short-circuit's payoff.
(defun bench-short-circuit ()
  (let* ((w (scaled 400)) (edits (scaled 60))
         (s (make-sheet)))
    (set-cell s "A1" 0)
    ;; GATE at B1: 0 unless the input is astronomically large.
    (set-cell s "B1" '(if (> (cell "A1") 1000000000) (cell "A1") 0))
    (loop for i from 0 below w do              ; cone: C-column cells read the gate
      (set-cell s (make-ref i 2) '(+ (cell "B1") 1)))
    (let ((pruned
            (timed ()
              ;; inputs 1,2,3,… never clear the gate -> B1 stays 0 -> cone pruned
              (dotimes (k edits) (set-cell s "A1" (1+ k)))))
          (propagate
            (timed ()
              ;; inputs above the gate, each distinct -> B1 changes -> cone recomputes
              (dotimes (k edits) (set-cell s "A1" (+ 2000000000 k))))))
      (row "short-circuit: pruned edits" pruned
           (format nil "cone=~D x ~D edits" w edits))
      (row "short-circuit: propagating edits" propagate
           (format nil "cone=~D x ~D edits" w edits))
      (row "  -> short-circuit speedup" nil
           (format nil "~,1Fx faster when pruned"
                   (/ (max 1 propagate) (max 1 pruned)))))))

;; 4. Full recompute: a grid where each cell sums a small window of the row
;;    above it, then recalc the whole sheet from scratch.
(defun bench-recalc-all ()
  (let* ((n (scaled 60)) (s (make-sheet)))
    (dotimes (c n) (set-cell s (make-ref 0 c) c))
    (loop for r from 1 below n do
      (dotimes (c n)
        (set-cell s (make-ref r c)
                  `(+ (cell ,(ref-string (make-ref (1- r) c)))
                      (cell ,(ref-string (make-ref (1- r) (max 0 (1- c)))))))))
    (let ((ticks (timed (:reps 5) (recalc-all s))))
      (row "recalc-all: NxN window grid" ticks
           (format nil "~Dx~D cells, 5 reps" n n)))))

;;; --- run -------------------------------------------------------------------

(bench-build)
(bench-chain)
(bench-short-circuit)
(bench-recalc-all)
(report)
#+sbcl (sb-ext:exit :code 0)
#+ecl  (si:quit 0)
