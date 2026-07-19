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
   ;; cell classes (note: CELL the class shares its symbol with the CELL
   ;; formula reader, already exported below) and their extension points
   #:external-cell #:async-cell #:observable-mixin #:readonly-mixin
   #:compute-value #:cell-swept #:volatile-p #:cell-writable-p
   ;; cell-kind constructors / drivers
   #:set-external #:set-async #:refresh-async #:deliver-async
   #:set-volatile #:set-readonly #:observe #:unobserve
   ;; concurrency
   #:with-sheet-lock
   ;; introspection
   #:dependents #:precedents #:map-cells #:volatile-refs
   ;; conditions
   #:sheet-error #:cyclic-reference #:cell-eval-error #:unbound-cell
   #:readonly-cell
   #:cyclic-reference-cells #:cell-eval-error-original
   ;; formula helpers usable inside formulas
   #:cell #:cells #:sum #:average #:cnt))
