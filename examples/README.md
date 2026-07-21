# Examples

### `quarterly-model.lisp` → `quarterly-model.sheet`

A small three-sheet financial model (Sales · Costs · Summary) that exercises much
of the engine at once:

- **cross-sheet references**, including a cross-sheet *named* cell
  (`(cell "Sales!total_rev")`);
- an **environment constant** (`tax`) used in a formula;
- **named cells**, **notes**, and a **merged** title/label;
- stdlib helpers (`sum`, `max`, `if`).

Run it to build the model, persist it, reload it (values recompute on load), and
render it to the console:

```bash
sbcl --script examples/quarterly-model.lisp     # or: ecl --load examples/quarterly-model.lisp
```

`quarterly-model.sheet` is the persisted workbook — a plain, readable Lisp form
(formulas, environment, names, notes, merges). Load it yourself with:

```lisp
(cellisp:load-workbook #p"examples/quarterly-model.sheet")
```

**Display formatting is not stored** (currency, percent, the negative-in-parens
rule) — that lives in a display-owned `make-formats` registry, applied at render
time; see `money-formats` in the script.

### Live data sources (HTTP / APIs)

The last section of the script shows a cell whose value comes from a **thunk**
instead of a formula (`set-external`), with a stub standing in for a real API
call — swap its body for e.g. `(dexador:get "…")`. For non-blocking fetches use
`set-async` + `deliver-async` from a worker thread, and pair with `set-volatile`
to poll. Closures don't serialize, so a persisted external source must use a
**named function** (a symbol), which round-trips and reattaches on load.

### `csv-spill.lisp` — fetch CSV over HTTP and spill the rows

A working prototype of "a cell that requests an HTTP endpoint returning CSV and
spills for however many rows the response has." Self-contained: the script starts
a **tiny local HTTP server** (usocket) serving CSV, then an **async** cell fetches
it on a worker thread and `spill`s one cell per field — sized dynamically to the
response. Switching the requested dataset re-fetches and re-spills, growing or
shrinking the block (old rows are cleared).

```bash
sbcl --script examples/csv-spill.lisp     # or: ecl --load examples/csv-spill.lisp
```

Needs `usocket` (server + client, keeping core dependency-free). Notes: the async
fetch avoids blocking the recompute under the sheet lock; `spill` doesn't clear a
shrunk block itself, so the driver clears the prior rectangle before each
re-spill; `parse-field` is minimal (no quoted-comma handling) — use `cl-csv` for
real data.

### `csv-spill-live.lisp` — the same, against a real HTTPS endpoint

Fetches a **live** public feed — Microsoft's Office 365 IP/URL list
(`endpoints.office.com`, `format=CSV`) — over HTTPS with `dexador`, and spills it.
Two real-world upgrades over `csv-spill.lisp`: dexador (HTTPS, pulls `cl+ssl`) and
a **quote-aware RFC-4180 CSV parser** (the IP-list fields are double-quoted and
comma-packed). The dump truncates wide cells so it stays readable. Switching the
`ServiceAreas` (Exchange → SharePoint) re-fetches a different-sized feed and
re-spills — e.g. 51 vs 52 rows × 11 cols.

```bash
sbcl --script examples/csv-spill-live.lisp     # needs internet + dexador
```

Both csv-spill scripts share **`csv-util.lisp`** (the RFC-4180 `parse-csv`, which
coerces fields with core `to-number`) and use the core `respill` (self-clearing
dynamic spill) and `print-sheet :max-col-width` — the pieces this exploration
promoted into the library.

### `cache-layers.lisp` → `cache-layers.sheet`

Stacks every caching/reliability mixin on **one** synchronous "fetch" cell —
`external` (source) + `retry` (survive transient failures) + `ttl` (time-window
cache) + `timed` (profile) + `logged` (history) — with a virtual clock, injected
failures, and a real-call counter, so each layer's effect is visible (retry
survives 2 failures; three recalcs inside the TTL window do 0 fetches; past the
window it re-fetches). Then it shows the two other caching tools — `set-cached`
(input-based) and `set-frozen` (pin) — and persists the layered cell to
`cache-layers.sheet`, reloading it to show what round-trips: the external source
(a **named** function), the `retry`/`ttl` config, and the logged history (but not
the transient timer or the live clock closure).

```bash
sbcl --script examples/cache-layers.lisp
```

They compose because `cached`/`ttl-cached`/`retry`/`timed` each hook
`compute-value` with an `:around` method, and `combined-class` stacks them.

### `async-pool.lisp` — engine-owned async pool + cancellation

Shows the opt-in async thread pool and cooperative cancellation: `(set-async …
:pool p)` runs a plain blocking fetcher on an engine-owned bounded pool (which
delivers its result or error and owns the thread lifecycle), `cancel-async` drops
an in-flight fetch's result, `async-status`/`async-pending-p` report state, and
`shutdown-async-pool` joins the workers. Self-contained (a stub thunk, no
network).

```bash
sbcl --script examples/async-pool.lisp
```
