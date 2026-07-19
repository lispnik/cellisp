(defpackage #:cellisp
  (:use #:cl)
  (:nicknames #:sheet)
  (:export
   ;; sheet
   #:make-sheet #:sheet #:sheet-p
   ;; reference parsing
   #:ref #:parse-ref #:ref-string #:ref-row #:ref-col
   ;; cell access
   #:cell-formula #:cell-value #:cell-err
   ;; mutation
   #:set-cell #:set-cells #:clear-cell #:get-value #:get-formula
   ;; recalculation
   #:recalc #:recalc-all
   ;; undo / redo
   #:undo #:redo
   ;; cell classes (note: CELL the class shares its symbol with the CELL
   ;; formula reader, already exported below) and their extension points
   #:external-cell #:async-cell
   #:observable-mixin #:readonly-mixin #:logged-mixin #:cached-mixin
   #:debounced-mixin #:default-mixin #:transformed-mixin #:validated-mixin
   #:timed-mixin #:retry-mixin #:ttl-cached-mixin #:throttled-mixin
   #:threshold-mixin #:stats-mixin #:persisted-mixin #:append-only-mixin
   #:typed-input-mixin #:versioned-mixin #:audited-mixin
   #:compute-value #:cell-swept #:volatile-p #:cell-writable-p
   #:note-set #:frozen-p #:with-actor #:*actor* #:*audit-clock*
   ;; cell-kind constructors / drivers
   #:set-external #:set-async #:refresh-async #:deliver-async
   #:set-volatile #:set-readonly #:set-logged #:cell-log #:set-cached
   #:observe #:unobserve #:debounce #:*debounce-delay*
   #:set-default #:set-transform #:set-validator #:set-timed #:cell-timing
   #:set-retry #:set-ttl #:throttle #:on-threshold #:set-stats #:cell-stats
   #:set-persist #:set-append-only #:set-typed-input #:set-frozen
   #:set-versioned #:cell-versions #:set-audited #:cell-audit
   ;; concurrency
   #:with-sheet-lock
   ;; serialization
   #:write-sheet #:read-sheet #:save-sheet #:load-sheet
   #:sheet->form #:form->sheet
   ;; structural editing + copy/paste
   #:insert-row #:delete-row #:insert-column #:delete-column
   #:copy-cell #:fill-range #:spill
   ;; named cells and ranges
   #:set-name #:remove-name #:name-ref #:set-range #:range-ref
   ;; introspection
   #:dependents #:precedents #:map-cells #:volatile-refs
   #:used-range #:sheet-dimensions #:set-change-hook
   #:explain #:explain-tree
   ;; conditions
   #:sheet-error #:cyclic-reference #:cell-eval-error #:unbound-cell
   #:readonly-cell #:invalid-value
   #:cyclic-reference-cells #:cell-eval-error-original
   ;; formula helpers usable inside formulas
   #:cell #:cells #:sum #:average #:cnt))
