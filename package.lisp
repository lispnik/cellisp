(defpackage #:cellisp
  (:use #:cl)
  (:nicknames #:sheet)
  (:export
   ;; sheet
   #:make-sheet #:sheet #:sheet-p
   ;; reference parsing
   #:ref #:parse-ref #:ref-string
   ;; cell access
   #:cell-formula #:cell-value #:cell-err
   ;; mutation
   #:set-cell #:set-cells #:clear-cell #:get-value #:get-formula
   ;; recalculation
   #:recalc #:recalc-all
   ;; introspection
   #:dependents #:precedents #:map-cells
   ;; conditions
   #:sheet-error #:cyclic-reference #:cell-eval-error #:unbound-cell
   #:cyclic-reference-cells #:cell-eval-error-original
   ;; formula helpers usable inside formulas
   #:cell #:cells #:sum #:average #:cnt))
