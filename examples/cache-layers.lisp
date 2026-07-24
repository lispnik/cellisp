;;;; cache-layers.lisp — stack every caching mixin on one synchronous cell.
;;;;
;;;;   sbcl --script examples/cache-layers.lisp
;;;;   ecl  --load   examples/cache-layers.lisp
;;;;
;;;; Demonstrates the caching/reliability mixins composing on a single "sync
;;;; fetch" cell — external (the source) + retry (survive transient failures) +
;;;; ttl (time-window cache) + timed (profile) + logged (history) — then the two
;;;; other caching tools, set-cached (input-based) and set-frozen (pin). Finally
;;;; it persists the layered cell and reloads it to show what round-trips.
;;;;
;;;; They compose because cached/ttl-cached/retry/timed each hook COMPUTE-VALUE
;;;; with an :AROUND method, so each wraps the next; combined-class stacks them.

(require :asdf)
(asdf:initialize-source-registry            ; find cellisp + its ocicl deps under ./
 (list :source-registry (list :tree (uiop:getcwd)) :inherit-configuration))
(asdf:load-system "cellisp")
(in-package #:cellisp)

;;; A stub "slow, flaky sync API" with a virtual clock (*NOW*), injected failures
;;; (*FAIL*), and a counter of REAL calls (*FETCHES*) so caching + retry show up.
;;; It's a NAMED function (a symbol), which is what lets an external source
;;; serialize and reattach on load — an anonymous lambda could not.
(defvar *now* 0) (defvar *fetches* 0) (defvar *fail* 0)
(defun fetch-price ()
  (incf *fetches*)
  (when (plusp *fail*) (decf *fail*) (error "503 transient upstream error"))
  (format nil "$~D @ t=~D" (+ 100 *now*) *now*))

(defparameter *sheet-file*
  (merge-pathnames "cache-layers.sheet"
                   (or *load-truename* *default-pathname-defaults*)))

(format t "~2&========== ALL LAYERS ON ONE SYNC CELL ==========~%")
(let ((s (make-sheet)))
  (set-external s "A1" 'fetch-price)                 ; 1. sync source (named -> serializable)
  (set-retry    s "A1" 3)                            ; 2. survive transient failures
  (set-ttl      s "A1" 10 :clock (lambda () *now*))  ; 3. cache for 10 clock-units
  (set-timed    s "A1" t)                            ; 4. profile each recompute
  (set-logged   s "A1" t)                            ; 5. record value history
  (setf *fetches* 0)                                 ; reset the counter after setup

  (setf *fail* 2 *now* 0)                            ; inject 2 transient failures
  (recalc s "A1")
  (format t "~&fetch @t0 (2 failures injected): value=~S  real-calls=~D  [RETRY survived them]~%"
          (get-value s "A1") *fetches*)

  (setf *now* 5)                                     ; still inside the 10-unit window
  (dotimes (i 3) (recalc s "A1"))
  (format t "3x recalc @t5 (within TTL):        value=~S  real-calls=~D  [TTL cache hits]~%"
          (get-value s "A1") *fetches*)

  (setf *now* 20)                                    ; window expired
  (recalc s "A1")
  (format t "recalc @t20 (TTL expired):          value=~S  real-calls=~D  [re-fetched]~%"
          (get-value s "A1") *fetches*)

  (multiple-value-bind (total n) (cell-timing s "A1")
    (format t "TIMED : ~D recomputes profiled, ~D total run-time units~%" n total))
  (format t "LOGGED: value history = ~S~%" (cell-log s "A1"))

  ;; --- persist the layered cell, then reload it -------------------------
  (format t "~2&========== PERSIST + RELOAD ==========~%")
  (save-sheet s *sheet-file*)
  (format t "~&saved ~A~%" (file-namestring *sheet-file*))
  (let* ((s2 (load-sheet *sheet-file*))
         (c  (find-cell s2 (parse-ref "A1"))))
    (format t "reloaded A1 config: source=~S  retry=~D  ttl=~D~%"
            (cell-source c) (retry-max c) (cell-ttl c))
    (format t "reloaded logged history: ~S~%" (cell-log s2 "A1"))
    ;; the external source reattached (a named function), so it fetches again;
    ;; TTL's *clock closure* did NOT serialize, so reattach it to keep the window
    (set-ttl s2 "A1" 10 :clock (lambda () *now*))
    (setf *now* 100 *fetches* 0 *fail* 0)
    (recalc s2 "A1")
    (format t "recalc reloaded A1 @t100: value=~S  real-calls=~D  [source reattached]~%"
            (get-value s2 "A1") *fetches*)))

(format t "~2&========== set-cached (input-based cache, the alt to TTL) ==========~%")
(defvar *bcalls* 0)
(defun expensive (x) (incf *bcalls*) (* x 2))
(let ((s (make-sheet)))
  (set-cell s "A1" 10)
  (set-cell s "B1" '(expensive (cell "A1")))
  (set-cached s "B1" t)
  (recalc s "B1") (setf *bcalls* 0)                  ; establish the signature, reset
  (dotimes (i 3) (recalc s "B1"))
  (format t "~&3x recalc, A1 unchanged: B1=~S  expensive-calls=~D  [cached]~%"
          (get-value s "B1") *bcalls*)
  (set-cell s "A1" 25)
  (format t "A1 changed to 25:        B1=~S  expensive-calls=~D  [recomputed]~%"
          (get-value s "B1") *bcalls*))

(format t "~2&========== set-frozen (pin a value outright) ==========~%")
(let ((s (make-sheet)))
  (set-cell s "A1" 5)
  (set-cell s "B1" '(* (cell "A1") 10))
  (set-frozen s "B1" t)
  (set-cell s "A1" 999)
  (format t "~&A1=999 but B1 frozen at ~S  [held, never recomputed]~%" (get-value s "B1")))

#+sbcl (sb-ext:exit :code 0)
#+ecl  (si:quit 0)
