# Changelog

All notable changes to Cellisp are documented here. The format follows
[Keep a Changelog](https://keepachangelog.com/), and the project aims to follow
[Semantic Versioning](https://semver.org/).

## [1.0.0] — 2026-07-21

First stable release. A dependency-light spreadsheet **engine** (formulas are
arbitrary Common Lisp) with a separate optional display layer, tested on SBCL and
ECL in CI (543 core + 74 display checks, including property-based tests).

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
