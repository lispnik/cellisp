(defpackage #:cellisp
  (:use #:cl)
  (:nicknames #:sheet)
  (:export
   ;; sheet
   #:make-sheet #:sheet #:sheet-p #:sheet-name #:sheet-workbook
   #:sheet-environment
   ;; workbook (multi-sheet)
   #:workbook #:workbook-p #:make-workbook #:add-sheet #:find-sheet
   #:remove-sheet #:workbook-sheets #:workbook-names #:recompute-workbook
   ;; reference parsing
   #:ref #:parse-ref #:ref-string #:ref-row #:ref-col
   #:index->col-letters #:col-letters->index
   ;; cell access
   #:cell-formula #:cell-value #:cell-err
   ;; mutation
   #:set-cell #:set-cells #:clear-cell #:get-value #:get-formula
   ;; recalculation
   #:recalc #:recalc-all
   ;; undo / redo, transactions
   #:undo #:redo #:with-transaction
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
   #:cancel-async #:deliver-error-async #:async-pending-p #:async-status
   #:make-async-pool #:shutdown-async-pool #:workbook-async-pool #:close-workbook
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
   #:write-workbook #:read-workbook #:save-workbook #:load-workbook
   #:workbook->form #:form->workbook
   ;; structural editing + copy/paste
   #:insert-row #:delete-row #:insert-column #:delete-column
   #:copy-cell #:fill-range #:spill #:respill
   ;; named cells and ranges
   #:set-name #:remove-name #:name-ref #:set-range #:range-ref #:map-names
   ;; tables (named header'd regions; columns referenced by header text)
   #:set-table #:table-ref #:remove-table #:map-tables #:table-at
   #:table-name #:table-region #:table-headers-p #:table-totals-p
   ;; cell notes / comments
   #:set-note #:cell-note #:remove-note #:map-notes
   ;; merged cells
   #:merge-cells #:unmerge-cells #:merged-range #:merges
   ;; introspection
   #:dependents #:precedents #:map-cells #:volatile-refs
   #:used-range #:sheet-dimensions #:set-change-hook
   #:explain #:explain-tree
   ;; conditions
   #:sheet-error #:cyclic-reference #:cell-eval-error #:unbound-cell
   #:readonly-cell #:invalid-value #:bad-reference #:unknown-name #:numeric-error
   #:cyclic-reference-cells #:cell-eval-error-original
   ;; formula helpers usable inside formulas
   #:cell #:cells #:sum #:average #:cnt
   #:col #:row                                ; whole-column / whole-row reads
   #:table-col                                ; a table column by header text
   ;; formula standard library (stdlib.lisp)
   #:minimum #:maximum #:product #:median
   #:countif #:sumif #:averageif #:safe-cells
   #:grid #:match #:lookup #:vlookup #:hlookup
   #:sortv #:filterv #:uniquev #:generic-lessp
   #:concat #:as-text #:to-number #:text-length #:left #:right #:mid
   #:upper #:lower #:trim #:substitute-text
   #:date #:year #:month #:day #:weekday #:now
   #:blankp #:iferror))
