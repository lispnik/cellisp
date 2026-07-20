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
