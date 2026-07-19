# Cellisp

A spreadsheet **engine** whose formula language is Common Lisp itself.

There is no UI and no formula DSL to learn: a cell's formula is either a literal
(a number, string, …) or an arbitrary Lisp form, and formulas read other cells
with the `cell` and `cells` operators. Cellisp's job is the hard part —
**dependency tracking and incremental recalculation** — with a change
short-circuit so an edit only recomputes what actually changed.

```lisp
(let ((s (make-sheet)))
  (set-cell s "A1" 10)
  (set-cell s "A2" 20)
  (set-cell s "A3" '(+ (cell "A1") (cell "A2")))
  (get-value s "A3"))                ; => 30

;; change an input; only the affected cells recompute
(set-cell s "A1" 100)
(get-value s "A3")                   ; => 120
```

> **Formulas are unsandboxed Lisp.** A formula is `eval`'d, so treat any formula
> input as arbitrary code execution.

## Requirements

- A Common Lisp implementation — the test suite runs on **SBCL** and **ECL**
  (both in CI; the code uses no implementation-specific symbols).
- [`bordeaux-threads`](https://common-lisp.net/project/bordeaux-threads/) (per-sheet locking).
- ASDF, and the project visible to it (e.g. symlinked into `~/quicklisp/local-projects/`).

## Load & test

```lisp
(asdf:load-system "cellisp")
(asdf:test-system "cellisp")         ; prints "N checks, M failures."
```

```bash
sbcl --eval '(asdf:test-system "cellisp")' --quit
```

## Guide

### Formulas, references, and the environment

A formula is any Lisp form. Inside it, `(cell "A1")` reads another cell and
`(cells "A1" "B3")` reads a rectangle as a list. Convenience aggregates
`sum`, `average`, and `cnt` ignore non-numbers, spreadsheet-style.

```lisp
(set-cell s "B1" '(sum (cells "A1" "A5")))
```

A sheet may carry an **environment** of named constants exposed to every
formula:

```lisp
(let ((s (make-sheet :environment '((tax . 1/10)))))
  (set-cell s "A1" 200)
  (set-cell s "A2" '(* (cell "A1") tax))
  (get-value s "A2"))                ; => 20
```

References are A1-style strings. `$` marks absoluteness for copy/paste
(`$A$1`, `$A1`, `A$1`); it's ignored when resolving. A cell can also have a
**name**:

```lisp
(set-name s "price" "A1")
(set-cell s "B1" '(* (cell "price") 2))
```

### Errors and recovery

Reads never raise — `get-value` returns `(values value error-or-nil)`. A cell
whose formula errors (division by zero, a cycle, a reference to an empty cell)
stores the condition; its dependents error too. One broken cell never aborts a
recompute, and fixing the cause recovers everything downstream.

```lisp
(set-cell s "A2" '(/ 1 (cell "A1")))   ; A1 = 0 -> A2 errors
(set-cell s "A1" 5)                    ; A2 recovers to 1/5
```

### Cell kinds

Beyond formula cells, a cell's *value source* can be:

- **external** — produced by a Lisp thunk instead of a formula
  (`set-external s "A1" (lambda () (read-sensor)))`;
- **async** — non-blocking; a value is pushed in out-of-band with
  `deliver-async` and the dependents recompute when it arrives.

### Behavior mixins

Cross-cutting behaviors layer onto *any* cell and compose freely (a cell can be
several at once):

| Driver | Effect |
|---|---|
| `observe` / `unobserve` | fire a callback when the value changes |
| `debounce` / `throttle` | coalesce / rate-limit change notifications |
| `on-threshold` | fire when the value crosses a level |
| `set-stats` / `cell-stats` | running count / sum / min / max / mean |
| `set-persist` | call an output sink on each change |
| `set-cached` | memoize; recompute only when an input changed |
| `set-ttl` | time-based memoization |
| `set-default` | fall back to a value on error |
| `set-transform` | post-process the value (clamp, round, …) |
| `set-validator` | signal `invalid-value` on a bad value |
| `set-timed` / `cell-timing` | profile recompute time |
| `set-retry` | retry a transient failure |
| `set-readonly` / `set-append-only` / `set-typed-input` | mutation guards |
| `set-versioned` / `cell-versions` | formula-edit history |
| `set-audited` / `cell-audit` | full provenance (who, when, what) |

```lisp
(observe s "A3" (lambda (v) (format t "A3 is now ~S~%" v)))
(set-validator s "A2" #'plusp)            ; A2 must stay positive
(set-audited s "A1" t)
(with-actor ("alice") (set-cell s "A1" 42))
(cell-audit s "A1")   ; => ((:time … :actor "alice" :formula 42) …)
```

Two orthogonal **attributes** aren't classes at all: `set-volatile` (recompute
every sweep, like `RAND()`/`NOW()`) and `set-frozen` (hold a value, skip
recompute).

### Thread safety

Every public entry point takes the sheet's recursive lock, so readers, writers,
and out-of-band async deliveries from other threads are serialized.

### Editing

- **Undo/redo** of formula edits: `(undo s)` / `(redo s)`.
- **Structural editing**: `insert-row` / `delete-row` / `insert-column` /
  `delete-column` move cells and rewrite references to follow them; a reference
  to a deleted cell becomes `#REF!`.
- **Copy/paste**: `copy-cell` and `fill-range` shift *relative* references by
  the source→dest offset while *absolute* (`$`) references stay fixed.

```lisp
(set-cell s "B1" '(* (cell "A1") 2))
(copy-cell s "B1" "B2")              ; B2 becomes (* (cell "A2") 2)
```

### Persistence

`save-sheet` / `load-sheet` (and `write-sheet` / `read-sheet` for streams)
round-trip a sheet as a readable form: formulas, environment, names,
declarative attributes, and durable history. **Values are recomputed on load**,
not stored — so a saved sheet is a live model you can edit and reload:

```lisp
(save-sheet s #p"/tmp/model.sheet")
;; …edit an input in the file…
(let ((s2 (load-sheet #p"/tmp/model.sheet")))   ; dependents recompute
  (get-value s2 "A3"))
```

Closure-based config (external sources, validators, callbacks) round-trips only
when given as **named functions**; anonymous lambdas can't be serialized.

## Design notes

The engine is one Common Lisp package (`cellisp`, nickname `sheet`). References
are `(row . col)` conses; a cell caches its value/error, its formula, and
bidirectional precedent/dependent links, plus a compiled-thunk cache. Cell kinds
and behaviors are a **CLOS taxonomy** — value-source subclasses, composable
behavior mixins combined on the fly, and the whole thing dispatched through a
few extension generics (`compute-value`, `cell-swept`, `cell-writable-p`,
`note-set`). Correctness of incremental recompute is guarded by a property-based
test asserting it always equals a full recomputation.

See `CLAUDE.md` for the internal architecture in depth.

## License

MIT.
