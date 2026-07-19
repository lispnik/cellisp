# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A spreadsheet *backend* whose formula language is Common Lisp itself. There is no UI and no parser for a formula DSL: a cell's formula is either a literal (number/string/etc.) or an arbitrary Lisp form, and cells are read from within formulas via the `cell`/`cells` operators. The system's job is dependency tracking and incremental recalculation.

## Commands

Load and test via ASDF (adjust the implementation as needed; examples use SBCL):

```lisp
;; load the system
(asdf:load-system "cellisp")

;; run the test suite (defsystem test-op perform hook)
(asdf:test-system "cellisp")
```

From the shell:

```bash
sbcl --eval '(asdf:test-system "cellisp")' --quit
```

The project must be visible to ASDF (e.g. symlinked into `~/quicklisp/local-projects/` or `~/common-lisp/`, or registered on `asdf:*central-registry*`).

Tests are a hand-rolled harness in `test.lisp` — no external test framework. `run-tests` prints `N checks, M failures.` and signals an `error` if any check fails. There is no per-test runner; add a `check`/`check-signals` form to `run-tests` to test one thing.

## Architecture

Files load in the `:serial t` order declared in `cellisp.asd`: `package` → `cell` → `sheet` → `eval` → `api`. Everything is in the single `#:cellisp` package (nickname `#:sheet`).

**References** (`cell.lisp`) — A ref is a `(row . col)` cons of zero-based integers. The external form is an A1 string (`"AA10"`). `parse-ref` coerces a ref cons, string, or symbol into a ref (validating either form and signaling `sheet-error` on anything malformed — bad column letters, a zero/negative row, trailing junk, or a cons that isn't two non-negative integers); `ref-string` is the inverse. Refs are compared with `EQUAL`, which is why the sheet's cell table and every dependency set uses `:test 'equal` / `:test #'equal` throughout — keep that invariant when touching those structures.

**Cell & sheet structs** (`cell.lisp`, `sheet.lisp`) — A `cell` caches its `value` (or an `err` condition when evaluation failed) alongside its `formula`, plus two adjacency lists: `precedents` (refs it reads) and `dependents` (refs that read it). It also memoizes a `compiled` thunk and the `compiled-from` formula it was built from (see `eval-formula` below). A `sheet` is a ref→cell hash table plus an `environment` alist of `(symbol . value)` constants exposed to every formula.

**Evaluation & the dependency graph** (`eval.lisp`) — the core. Four dynamic vars drive recalculation: `*sheet*` (sheet being evaluated), `*eval-stack*` (refs currently on the DFS stack, for cycle detection), `*collected-precedents*` (a hash table that, when bound, records which cells the running formula touches), and `*fresh*` (a hash table, bound for one recompute sweep, of refs already computed this sweep).

- Precedents are **rediscovered on every recompute** by evaluating the formula with `*collected-precedents*` active — `cell`/`cells` call `note-precedent`. This is deliberately dynamic so conditional references (`(if ... (cell "A1") (cell "B1"))`) track only the branch actually taken.
- `evaluate-ref` pulls a cell's value on demand, recursing into precedents depth-first. It short-circuits a ref already in `*fresh*` (reusing its value, or re-signaling its stored `err`) — so within one sweep each cell computes at most once, and a diamond/DAG doesn't recompute shared precedents per reader. The `*fresh*` check sits *before* the cycle check; a cell mid-computation isn't fresh yet, so a re-entrant read still hits `*eval-stack*` and signals `cyclic-reference`.
- `compute-cell` evaluates the formula, commits `value`/`err`, then — in an `unwind-protect` cleanup that runs **even when the formula errors** — calls `update-dependency-links` to reconcile the bidirectional back-links and marks the ref in `*fresh*`. Committing links on error is essential: otherwise an errored cell drops out of the graph and never recomputes when its inputs later recover.
- `eval-formula` runs the formula: a bare symbol resolves against the environment alist (else self-evaluates); a cons is real Lisp — `eval`'d directly when the sheet has no environment, otherwise run through a `compile`d lambda with `let` bindings. That lambda is **compiled once and cached** on the cell (`compiled`/`compiled-from` via `cell-thunk`), keyed by formula identity (`eq`) — recompiled only when the formula changes, since the environment is assumed fixed per sheet (mutating `sheet-environment` after a formula has run leaves cached thunks stale). Environment values are spliced in **quoted**, so a list- or symbol-valued constant is treated as data, not evaluated as a form. **Formulas are unsandboxed Lisp** — treat any formula input as arbitrary code execution.

**Public API** (`api.lisp`) — `set-cell`/`clear-cell` mutate, then call `recompute-closure`. `affected-closure` collects a seed plus its transitive dependents via DFS over the `dependents` graph; `recompute-closure` binds a fresh `*fresh*` set for the sweep and forces each affected cell, skipping any already computed as a precedent of an earlier cell (ordering resolves itself because `evaluate-ref` pulls precedents on demand). Per-cell errors are stored on the cell and swallowed during a sweep so one broken cell doesn't abort the whole recompute; `set-cell` re-signals only its *own* cell's error. `set-cells` takes a list of `(designator formula)` pairs, installs *all* formulas before recomputing, then runs a single `recompute-closure` over the combined seeds — so batch cells may reference each other in any order (forward refs resolve with no transient error) and shared dependents recompute once. It does *not* re-signal (errors stay on the cells); it returns the resulting values in input order. Reads never raise: `get-value` returns `(values value error-or-nil)` (empty cell → `nil, nil`), so callers inspect the second value rather than handling a condition; `get-formula` returns the stored form. `recalc`/`recalc-all` force recomputation on demand, and `dependents`/`precedents`/`map-cells` expose the graph for introspection. The full exported surface is the `:export` list in `package.lisp` — including the in-formula helpers `cell`, `cells`, `sum`, `average`, `cnt`.

## Conventions

- Conditions form a hierarchy rooted at `sheet-error` (defined in `sheet.lisp`, above the sheet struct): `cyclic-reference`, `unbound-cell`, `cell-eval-error` (wraps a non-sheet error raised inside a formula, preserving the `original`). Signal these rather than raw `error` for anything a formula author could trigger. `compute-cell` catches `sheet-error` and re-signals it as-is, but wraps any other `error` in a `cell-eval-error` — so formula bugs always surface as a `sheet-error` subtype.
- The in-formula aggregates (`sum`/`cnt`/`average`, all over `flatten-numbers`) **ignore non-numeric values** spreadsheet-style: `sum` of nothing is `0`, `cnt` counts only numbers, and `average` of no numbers **signals `sheet-error`** rather than dividing by zero.
- A `sheet` is **not thread-safe** — recalc mutates the cell table and adjacency lists in place through dynamic vars with no locking. Confine each sheet to one thread, or lock the mutators (`set-cell`/`set-cells`/`clear-cell`) and reads yourself.
- The exported symbol list in `package.lisp` is the public surface — update it when adding user-facing functions.