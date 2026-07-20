# Cellisp

[![CI](https://github.com/lispnik/cellisp/actions/workflows/ci.yml/badge.svg)](https://github.com/lispnik/cellisp/actions/workflows/ci.yml)

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

## Benchmark

`bench.lisp` is a standing performance harness (separate from the test suite). It
prints a timing table and, notably, measures the change-propagation
short-circuit's payoff directly — the same edits recompute a whole dependent cone
or prune it entirely:

```bash
sbcl --script bench.lisp        # or:  ecl --load bench.lisp
sbcl --script bench.lisp 4      # optional scale factor grows every workload
```

## Guide

### Formulas, references, and the environment

A formula is any Lisp form. Inside it, `(cell "A1")` reads another cell and
`(cells "A1" "B3")` reads a rectangle as a list. Convenience aggregates
`sum`, `average`, and `cnt` ignore non-numbers, spreadsheet-style.

```lisp
(set-cell s "B1" '(sum (cells "A1" "A5")))
```

Because a formula is just Lisp, CL's own `if`, `min`, `max`, `and`, … work
directly on explicit arguments. On top of that, a small **standard library** adds
the spreadsheet conveniences CL lacks — aggregates that ignore blanks/text over a
range, predicate-filtered aggregates, 2D range access, lookups, and `iferror`:

A **range** read tolerates gaps: `cells`/`grid` read an empty cell as `nil`
(blank), so `(sum (cells "A1" "A99"))` sums whatever is there. (A *single*
`(cell "Z9")` read stays strict — an empty cell there signals, so errors still
propagate.) `safe-cells` goes further and skips *errored* cells too.

```lisp
(minimum (cells "A1" "A9"))              ; MIN ignoring text/blanks (also maximum, product, median)
(sum (safe-cells "A1" "A99"))            ; also skips errored cells, not just blanks
(sumif (lambda (x) (> x 100)) (cells "A1" "A9"))   ; also countif, averageif
(grid "A1" "B9")                         ; a range as a list of rows (2D), vs. cells' flat list
(vlookup "acme" (grid "A1" "C9") 3)      ; also lookup, hlookup, match
(sortv (cells "A1" "A9"))                ; also filterv, uniquev (compose with spill)
(concat (cell "A1") " " (cell "B1"))     ; text: left, right, mid, upper, lower, trim, substitute-text
(year (date 2026 7 20))                  ; dates as universal-time: date, year, month, day, weekday, now
(iferror (/ (cell "A1") (cell "A2")) 0)  ; a value on error; precedents still tracked
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
(get-value s "price")                     ; => 20
(set-cell s "price" 15)                    ; the whole public API takes a name
```

A name works **anywhere an A1 ref does** — in formulas *and* through the public
API (`get-value`, `set-cell`, `clear-cell`, `set-note`, the mixin drivers, …).

A name can also alias a whole **range**, read with the one-argument form of
`cells` (which also accepts a single cell as a 1×1 range):

```lisp
(set-range s "q1" "A1" "A3")
(set-cell s "B1" '(sum (cells "q1")))     ; sums A1:A3
```

Both kinds of name follow their cells across structural edits and round-trip
through serialization.

### Workbooks (multiple sheets)

A **workbook** groups named sheets, and a formula reaches another sheet with a
`Sheet!A1` qualifier. The dependency graph spans sheets, so editing a cell on one
sheet recomputes everything that reads it on any other — the same incremental,
short-circuited recompute, now cross-sheet:

```lisp
(let* ((wb (make-workbook))
       (data    (add-sheet wb "Data"))
       (summary (add-sheet wb "Summary")))
  (set-cell data "A1" 10)
  (set-cell data "A2" 20)
  (set-cell summary "B1" '(sum (cells "Data!A1" "Data!A2")))
  (get-value summary "B1")            ; => 30
  (set-cell data "A1" 100)            ; edit on Data…
  (get-value summary "B1"))           ; => 120  (Summary recomputed)
```

Sheet names are case-insensitive; `find-sheet` looks one up, `workbook-names`
lists them in order. A sheet from `make-sheet` is standalone and pays none of the
cross-sheet cost. Producers are always recomputed before their consumers;
a genuine cross-sheet reference *cycle* is detected and flagged
(`cyclic-reference`) rather than looping. Whole workbooks persist with
`save-workbook` / `load-workbook` (and `write-workbook` / `read-workbook` for
streams), values recomputed on load just like a single sheet.

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
- **Transactions**: `(with-transaction (s) …)` applies a group of edits in one
  recompute sweep and as a single undo step, rolling the sheet fully back if the
  body signals.
- **Structural editing**: `insert-row` / `delete-row` / `insert-column` /
  `delete-column` move cells and rewrite references to follow them; a reference
  to a deleted cell becomes `#REF!`.
- **Copy/paste**: `copy-cell` and `fill-range` shift *relative* references by
  the source→dest offset while *absolute* (`$`) references stay fixed.
- **Spill**: `spill` fills a rectangle from an array-valued formula
  (`(spill s "B1" '(mapcar #'1+ (cells "A1" "A3")))`), tracking its inputs.

```lisp
(set-cell s "B1" '(* (cell "A1") 2))
(copy-cell s "B1" "B2")              ; B2 becomes (* (cell "A2") 2)
```

### Driving a UI

A front end needs two things the engine now hands it directly. A **change hook**
delivers, after every edit, exactly the cells to repaint — the refs whose value
or error changed — so nothing has to diff the grid:

```lisp
(set-change-hook s (lambda (changed) (dolist (r changed) (repaint r))))
(set-cell s "A1" 100)   ; hook fires with (A1 A3 …): A1 and its dependents
(set-cell s "A1" 100)   ; hook fires with (): the value didn't change
```

The set is row-major sorted, includes cells emptied by `clear-cell`, and is
empty when an edit changes nothing (the propagation short-circuit is visible
here). It fires for *any* edit path — `set-cell(s)`, `clear-cell`, `recalc`,
undo/redo, structural edits. **Grid extent** comes from `used-range` (the tight
bounding box of non-empty cells, as a `(top-left . bottom-right)` cons, or `nil`)
and `sheet-dimensions` (rows and cols as two values):

```lisp
(used-range s)        ; => ((1 . 1) . (4 . 3))   i.e. B2:D5
(sheet-dimensions s)  ; => 5, 4
```

### Explaining a value

`explain` prints a cell's precedent tree — how its value (or error) arises.
Because a precedent's error propagates to the cells that read it, the tree
pinpoints the root cause:

```
B1 = <error: Cell Z9 is empty>   (* (CELL "A3") TAX)
└─ A3 = <error: Cell Z9 is empty>   (/ (CELL "A1") (CELL "Z9"))
   ├─ A1 = 1000   1000
   └─ Z9  <empty>
```

`explain-tree` returns the same information as a nested plist for programmatic
use.

### Display

The value a cell *shows* — a formatted string, or a spreadsheet error token — is
the job of the optional **`cellisp/display`** system (loaded separately; the
engine carries no UI concern):

```lisp
(asdf:load-system "cellisp/display")
(use-package '#:cellisp/display)

(display-value s "A3")                 ; => "120"        (a value)
(display-value s "A2")                 ; => "#DIV/0!"    (a stored error)
```

`display-value` returns the cell's value formatted, `""` for an empty cell, or an
error token — `#DIV/0!`, `#REF!`, `#CYCLE!`, `#VALUE!`, `#NUM!`, `#NAME?`,
`#ERR!` — derived from the stored condition. Formatting is controlled by an
optional, in-memory **format registry** (per cell or per column; cell wins):

```lisp
(let ((f (make-formats)))
  (set-column-format f "B" '(:percent 0))    ; whole column B as percentages
  (set-format f "B1" '(:fixed 2))            ; …but B1 to 2 decimals
  (display-value s "B1" :formats f))
```

Specs are `:general`, `:integer`, `(:fixed n)`, `(:percent n)`,
`(:currency sym n)`, a literal string, or a function of the value. `format-value`
and `error-token` are also exported for direct use.

**Rendering to a console:** `print-sheet` / `print-workbook` dump an aligned text
grid to a stream (default stdout) — column-letter headers, row numbers, cells via
`display-value` (numbers right-aligned), an optional format registry applied:

```lisp
(print-workbook wb)          ; each sheet, headed by its name, to *standard-output*
(print-sheet s :formats f)   ; one sheet, styled
```

```
Data
  | A  | B     | C     |
--+----+-------+-------+
1 | 10 | hello | #REF! |
2 | 20 |       |       |
3 | 30 |       |       |
```

**Conditional formatting** layers value-dependent rules on top: a predicate over
the cell's value picks a spec that overrides the static format (first match wins,
optionally scoped to a column):

```lisp
(add-conditional f #'minusp (lambda (v) (format nil "(~A)" (abs v))))  ; parenthesize negatives
(add-conditional f #'zerop "—" :column "B")                            ; blank zeros in column B
```

Cell/column formats and rules may be **sheet-qualified** (`"Sales!D5"`,
`"Sales!B"`, `:sheet "Summary"`), so one registry can style a whole workbook,
each sheet differently — most specific scope wins.

### Cell metadata

Sheets also carry UI metadata that follows cells across structural edits and
round-trips through serialization: **notes/comments** (`set-note` / `cell-note` /
`remove-note`) and **merged cells** (`merge-cells` / `unmerge-cells` /
`merged-range` / `merges`, anchored at the top-left, overlaps refused). The engine
never reads either — they're for the front end.

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
