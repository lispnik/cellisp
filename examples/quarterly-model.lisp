;;;; quarterly-model.lisp — build a small multi-sheet financial model, persist it
;;;; as a workbook, reload it to prove the round-trip, and render it.
;;;;
;;;;   sbcl --script examples/quarterly-model.lisp
;;;;   ecl  --load   examples/quarterly-model.lisp
;;;;
;;;; Writes quarterly-model.sheet next to this file. The model exercises: three
;;;; sheets, cross-sheet references (including a cross-sheet *named* cell), an
;;;; environment constant, notes, and a merged title — all of which serialize.
;;;;
;;;; The final section demonstrates a live HTTP-style data source via an external
;;;; cell (with a stub standing in for a real API call); that part is shown live,
;;;; not persisted, since a closure-backed source needs a named function to
;;;; round-trip (see the comment there).

(require :asdf)
(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql)))
#+quicklisp (ql:quickload "bordeaux-threads" :silent t)
(asdf:load-system "cellisp")
(asdf:load-system "cellisp/display")

(in-package #:cellisp)
(use-package '#:cellisp/display)

(defparameter *model-file*
  (merge-pathnames "quarterly-model.sheet"
                   (or *load-truename* *default-pathname-defaults*)))

(defun build-model ()
  "Construct and return the workbook."
  (let* ((wb      (make-workbook))
         (sales   (add-sheet wb "Sales"))
         (costs   (add-sheet wb "Costs"))
         (summary (add-sheet wb "Summary" :environment '((tax . 21/100)))))

    ;; --- Sales: units x price per product, with a revenue total ---------
    (set-range sales "revenue" "D2" "D4")         ; a named RANGE (the revenue col)
    (set-cells sales
      '(("A1" "Product") ("B1" "Units") ("C1" "Price") ("D1" "Revenue")
        ("A2" "Widget")  ("B2" 3000) ("C2" 25) ("D2" (* (cell "B2") (cell "C2")))
        ("A3" "Gadget")  ("B3" 800)  ("C3" 40) ("D3" (* (cell "B3") (cell "C3")))
        ("A4" "Gizmo")   ("B4" 1500) ("C4" 15) ("D4" (* (cell "B4") (cell "C4")))
        ("A5" "Total")   ("D5" (sum (cells "revenue")))))  ; sum the named range
    (set-name  sales "total_rev" "D5")            ; referenced by name, cross-sheet
    (set-note  sales "D5" "sum of all product revenue")
    (merge-cells sales "A5" "C5")                 ; the "Total" label spans A5:C5

    ;; --- Costs: COGS scales with revenue (cross-sheet), plus fixed lines --
    (set-cells costs
      '(("A1" "Category")       ("B1" "Amount")
        ("A2" "COGS (40% rev)") ("B2" (* (cell "Sales!total_rev") 2/5))
        ("A3" "Salaries")       ("B3" 45000)
        ("A4" "Marketing")      ("B4" 12000)
        ("A5" "Rent")           ("B5" 8000)
        ("A6" "Total")          ("B6" (sum (cells "B2" "B5")))))
    (set-name costs "total_cost" "B6")

    ;; --- Summary: a P&L pulling both sheets by name; tax from the env -----
    (merge-cells summary "A1" "B1")               ; title banner
    (set-cells summary
      '(("A1" "Quarterly P&L")
        ("A2" "Revenue")      ("B2" (cell "Sales!total_rev"))
        ("A3" "Total Costs")  ("B3" (cell "Costs!total_cost"))
        ("A4" "Gross Profit") ("B4" (- (cell "B2") (cell "B3")))
        ("A5" "Tax")          ("B5" (* (max 0 (cell "B4")) tax))
        ("A6" "Net Profit")   ("B6" (- (cell "B4") (cell "B5")))
        ("A7" "Margin %")     ("B7" (if (plusp (cell "B2"))
                                        (* 100.0 (/ (cell "B6") (cell "B2")))
                                        0))))
    (set-name summary "net_profit" "B6")
    (set-note summary "B5" "flat 21% corporate rate (the env constant `tax`)")
    wb))

(defun money-formats ()
  "A display registry: dollars in the money columns, a percent for the margin."
  (let ((f (make-formats)))
    (set-column-format f "B" '(:currency "$" 0))
    (set-column-format f "C" '(:currency "$" 0))
    (set-column-format f "D" '(:currency "$" 0))
    (set-format f "Summary!B7" '(:fixed 1)) ; sheet-qualified: the margin %, not $
    ;; make a loss stand out: negative money in parentheses
    (add-conditional f #'minusp (lambda (v) (format nil "($~:D)" (abs (round v)))))
    f))

;;; ---------------------------------------------------------------------------

(let ((wb (build-model)))
  (format t "~2&===== built model =====~%")
  (print-workbook wb :formats (money-formats))

  ;; persist, then reload to prove the round-trip (values recompute on load)
  (save-workbook wb *model-file*)
  (format t "~2&===== saved to ~A =====~%" (file-namestring *model-file*))
  (let ((wb2 (load-workbook *model-file*)))
    (format t "===== reloaded; recomputed values match =====~%")
    (let ((s (find-sheet wb2 "Summary")))
      (format t "  Revenue    : ~:D~%" (get-value s "B2"))
      (format t "  Total Costs: ~:D~%" (get-value s "B3"))
      ;; get-value takes a ref, not a name, so resolve the name first
      (format t "  Net Profit : ~:D  (via named cell: ~:D)~%"
              (get-value s "B6") (get-value s (name-ref s "net_profit")))
      (format t "  Margin %   : ~,2F~%" (get-value s "B7"))
      (format t "  note on B5 : ~S~%" (cell-note s "B5")))))

;;; ---------------------------------------------------------------------------
;;; Bonus: an HTTP-style data source. A cell's value can come from a thunk, so
;;; any HTTP client works. Here a stub stands in for the network call; swap its
;;; body for e.g. (parse-rate (dexador:get "https://api.example.com/fx/USD-EUR")).
;;; For non-blocking fetches use SET-ASYNC + DELIVER-ASYNC on a worker thread.
;;;
;;; Closures don't serialize, so this part is live-only. To persist an external
;;; source, give it a *named* function (a symbol) instead of a lambda — then it
;;; round-trips and reattaches on load, provided that function is defined.

(defun fetch-usd-eur-rate ()
  "Stub for an HTTP API call returning the USD->EUR rate."
  ;; real version: (parse-number (dexador:get ".../fx/USD-EUR"))
  0.92)

(let ((wb (build-model)))
  (let ((summary (find-sheet wb "Summary")))
    (set-external summary "D1" #'fetch-usd-eur-rate)          ; live data source
    (set-cell summary "D2" '(* (cell "net_profit") (cell "D1"))) ; net profit in EUR
    (format t "~2&===== live external (HTTP-style) data source =====~%")
    (format t "  USD->EUR rate (fetched): ~A~%" (get-value summary "D1"))
    (format t "  Net profit in EUR      : ~,2F~%" (get-value summary "D2"))))

#+sbcl (sb-ext:exit :code 0)
#+ecl  (si:quit 0)
