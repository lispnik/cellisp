# Changelog

All notable changes to Cellisp are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/), and the project aims to follow
[Semantic Versioning](https://semver.org/).

## [Unreleased]

### Added
- **User-defined behavior mixins.** `register-mixin` promotes any class that
  overrides a cell generic to a first-class mixin: it joins the known-mixin set
  (surviving later morphs, reported by `mixins-at`), takes an optional
  `:precedence-after`, and — given `:serialize-as`/`:dump`/`:restore` — round-trips
  through `sheet->form`. `set-mixin` attaches/detaches any mixin by designator (the
  general form of `set-cached` & friends); `mixins-at` introspects a cell.
- **Whole-column and whole-row references.** `(col "A")` / `(row 5)` read an entire
  column/row as a list — bands too (`(col "A" "C")`, `(row 2 5)`), and the Excel
  colon form through `cells` (`(cells "A:A")`, `(cells "1:1")`). These track the
  column/row *as a whole*: one coarse dependency (a per-sheet column/row watcher
  index), not an edge per cell, so a change anywhere — including a cell filled in
  *later* — re-fires the reader with no per-cell edge explosion. They shift
  Excel-faithfully under structural edits (an endpoint deletion shrinks a band;
  deleting the whole column yields `#REF!`).
- **Computable tables.** `set-table` names a header'd rectangular region whose
  columns are referenced by *header text*: `(table-col "Sales" "Amount")` or the
  Excel string `(cells "Sales[Amount]")` reads the column's data rows and re-fires as
  the data changes or grows. Also `Sales[@Amount]` (`:this-row`, calculated columns),
  a `:totals`-flagged row excluded from data reads, and auto-expand when a cell is
  typed directly below/right. Tables serialize (`:tables`, serialization version 2),
  shift Excel-faithfully under structural edits, and reject overlap. API: `set-table`,
  `table-ref`, `remove-table`, `map-tables`, `table-at`, `table-col`.
- **Cross-sheet whole-column/row and table references.** The sheet-qualified forms
  `(cells "Data!A:A")`, `(cells "Data!1:1")`, and `(cells "Data!Sales[Amount]")` read
  another sheet's column / row / table column, recording a *coarse* cross-sheet
  dependency (a per-target column/row watcher, `foreign-col/row-watchers`) so the
  workbook cascade re-fires the reader when any cell on that line of the target
  changes — including a cell filled in later — with no per-cell foreign edges.
- **copy/fill shift whole-column/row references** by the paste offset, like cell
  refs (off-grid → `#REF!`); table references, being name-based, stay put.

### Changed
- **Table-column dependencies are row-bounded** — within *and across* sheets. A
  `(table-col …)` / `Sales[Amount]` reader (and its cross-sheet form,
  `Data!Sales[Amount]`) now depends on just the table's own rows (header through
  the data, plus the one auto-grow row below when there is no totals row), so an
  edit *elsewhere* in the same physical column no longer re-fires it — the coarse
  over-fire noted when whole-column dependencies shipped. Header re-resolution and
  auto-expand still trigger it. Internally, spans carry an optional orthogonal
  bound, the per-sweep changed-column/row index records *which* lines changed, and
  each cross-sheet column/row watcher entry carries the consumer's bound so the
  workbook cascade gates on it.
- **Whole-column/row reads scan only populated cells.** `read-span` (and its
  cross-sheet twin) now enumerate the sheet's actual content cells rather than
  every position in the used-range rectangle, so reading a sparse column in a tall
  sheet no longer visits the empty rows between its cells. Values and ordering are
  unchanged.
- **Concurrency: unified workbook locking.** A workbook's sheets now share one
  recursive lock instead of each holding its own. A cross-sheet cascade (an edit
  on one sheet recomputing consumers on its peers) previously mutated those peer
  sheets while holding only the originating sheet's lock — a data race with any
  concurrent access to a peer, and a cross-sheet lock-ordering deadlock risk.
  One lock per workbook covers every sheet a cascade touches and removes both.
  Standalone sheets (and independent workbooks) still lock independently.
- **Guarded global lazy-inits.** Creating the shared default async pool (and a
  workbook's pool) and defining a new combined cell class are now each guarded by
  a dedicated lock with double-checked locking, so a race can neither spawn a
  duplicate pool whose worker threads leak nor corrupt the global class table.

### Fixed
- **Error tokens by condition class, and a deleted reference is now `#REF!`.**
  `error-token` maps a cell's stored condition to a spreadsheet token by *type*
  now — the core signals dedicated `bad-reference`/`unknown-name`/`numeric-error`
  conditions at each failure site — instead of parsing the condition's report
  text. This fixes two mis-tokenings: a reference left dangling by a structural
  delete renders `#REF!` (was `#NAME?`, because the `#REF!` sentinel's `!` was
  read as a sheet qualifier), and an aggregate over no numbers renders `#NUM!`
  (was `#ERR!`).
- **`typed-input` and `threshold` survive serialization.** Both mixins were
  silently dropped on save; a `typed-input` write-guard (a named predicate) and a
  `threshold` subscriber (a named function) now round-trip, so a loaded sheet
  keeps its input constraints and threshold reactions. Loading a file whose
  version is newer than the running build now fails loudly instead of silently
  discarding unknown fields.
- **Spill extent survives structural edits.** A row or column inserted or
  deleted *inside* a spill now updates the spill's recorded `(rows . cols)`
  extent (to the new bounding box), not just its anchor. Previously the extent
  was left stale, so a later `respill` cleared the wrong rectangle and orphaned
  a displaced spilled cell.
- **Mixin layer order is explicit, not alphabetical.** Combined cell classes are
  now ordered by an explicit `*mixin-precedence*` (cache ▸ default ▸ validate ▸
  transform ▸ retry ▸ timed) instead of by class name, so the value-pipeline
  semantics are a documented decision and a newly-added mixin can't silently
  reorder the `:around` stack. As part of this, `validated` now wraps
  `transformed` (it validates the *transformed* value, not the raw one); the
  intentional `default`-over-`validated` "soft validation" and `retry`-over-
  `timed` orderings are preserved.
- **Stale error across an existence transition.** Assigning a previously
  unreferenced/placeholder cell a value equal to its placeholder default (e.g.
  `nil`) now refreshes dependents that had errored on the cell's absence, instead
  of leaving them with a stale `unbound-cell` — the value-only change
  short-circuit used to skip them. Affects `set-cell` and `set-cells`.
- **`set-cells` on a named cell.** A batch targeting a registered name (e.g.
  `(set-cells s '(("total" 5)))`) no longer signals from the undo capture; undo
  now restores the correct cell (the capture used raw `parse-ref` instead of the
  name-aware `resolve-ref-in`).
- **Observers on async cells.** A successful `deliver-async` now fires the async
  cell's own `cell-swept` hooks (`observe` and logging/stats mixins), matching
  `set-async`; delivered values previously updated dependents but skipped the
  cell's own subscribers.
- **`sumif` / `averageif` predicate safety.** The predicate is now applied
  defensively (as in `countif`), so a value it errors on — e.g. `evenp` of a
  float in a mixed range — is simply excluded rather than aborting the aggregate.

### Security / robustness
- **Serialization hardening.** `load-sheet` / `load-workbook` bind `*read-eval*`
  to `nil`, so a `#.` reader macro in a `.sheet` file can no longer execute at
  read time. `save-sheet` / `save-workbook` now validate the form is readably
  printable *before* opening the target file, so an unserializable value fails
  loudly with a `sheet-error` instead of silently writing a load-broken file (and
  without truncating an existing good file first).

## [1.0.0] — 2026-07-21

First stable release. A dependency-light spreadsheet **engine** (formulas are
arbitrary Common Lisp) with a separate optional display layer, tested on SBCL and
ECL in CI with a broad automated check suite (core + display), including
property-based tests. (The exact check counts are whatever `run-tests` reports
on a given commit — see the tail of a CI run — rather than a figure hand-copied
here that drifts as tests are added.)

### Engine (`cellisp`)
- **Dependency graph + incremental recalculation** with a change-propagation
  short-circuit; reads never raise (`get-value` returns `(values value error)`),
  per-cell errors are stored as conditions and recover when their cause is fixed.
- **Formulas** are Lisp forms reading cells via `cell` / `cells`; a per-sheet
  **environment** of named constants; **named cells and ranges**; `$`-absolute
  references for copy/paste.
- **Formula standard library** (`stdlib.lisp`): `sum`/`average`/`cnt`,
  `minimum`/`maximum`/`product`/`median`, `countif`/`sumif`/`averageif`,
  `safe-cells`, `grid`, `sortv`/`filterv`/`uniquev`, `match`/`lookup`/`vlookup`/
  `hlookup`, text (`concat`, `left`/`right`/`mid`, …), dates as universal-time
  (`date`/`year`/`month`/`day`), `to-number`, `iferror`, `blankp`.
- **Cell kinds & behavior mixins** (CLOS taxonomy, composed on the fly): value
  sources `external` / `async`; mixins `observe`, `debounce`, `throttle`,
  `on-threshold`, `cached`, `ttl`, `stats`, `persist`, `default`, `transform`,
  `validate`, `timed`, `retry`, `readonly`, `append-only`, `typed-input`,
  `versioned`, `audited`, `logged`; attributes `volatile`, `frozen`.
- **Async cells**: manual or an engine-owned **thread pool** (`:pool`);
  `cancel-async` (epoch-gated), a `:cancelable` cancel-token to abort work,
  `deliver-error-async`, `async-status`/`async-pending-p`; `close-workbook` /
  `shutdown-async-pool` own the thread lifecycle.
- **Multi-sheet workbooks** with cross-sheet references (`Data!A1`), a cascading
  recompute, and cross-sheet cycle detection.
- **Editing**: `undo`/`redo`, `with-transaction` (atomic, single recompute,
  rollback on error), structural edits (`insert-row`/`delete-row`/…, `#REF!`),
  copy/paste with relative/absolute refs, `spill` / self-clearing `respill`.
- **Metadata**: cell notes, merged cells; **change hook**, `used-range` /
  `sheet-dimensions`, `explain` / `explain-tree`.
- **Serialization**: `save-sheet` / `load-sheet` (and workbook variants) round-
  trip formulas, environment, names, notes, merges, declarative attributes, and
  durable history as a readable form; values recomputed on load.
- **Thread-safe** at the API boundary via a per-sheet recursive lock.

### Display layer (`cellisp/display`, optional, separate system)
- `display-value`, `error-token` (Excel-style `#DIV/0!` / `#REF!` / …),
  `format-value`, an in-memory `make-formats` registry (per-cell/per-column,
  sheet-qualified), value-dependent `add-conditional` rules.
- `print-sheet` / `print-workbook` — aligned text grid, `:formulas` view,
  `:max-col-width`, names/environment footer.

### Tooling
- `bench.lisp` performance harness; property-based tests (single-sheet, cross-
  sheet, serialization round-trip, display totality); CI on SBCL + ECL.
- Worked examples under `examples/` (financial model, CSV-over-HTTP spill, layered
  caching, async pool) with persisted `.sheet` artifacts.

[1.0.0]: https://github.com/lispnik/cellisp/releases/tag/v1.0.0
