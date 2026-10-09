# Plan: make textDocument/formatting work for notebook cells

## Where this came from

Four languages reach their language server through a notebook proxy: Rust
(rust-notebook-language-server), Haskell (haskell-notebook-language-server), C++
(cpp-notebook-language-server) and Go (go-notebook-language-server). Each one projects the
notebook's cells into a document the server will accept, registers it under a different URI,
and rewrites requests and responses between the two coordinate spaces.

None of them handled `textDocument/formatting`, so the request went to the server naming a
document it had never opened and failed. rust-notebook-language-server#2 fixed it for Rust.
The other three still can't format a cell, and the reason isn't the missing handler — it's that
their projections can't be undone once a formatter has rewritten the document.

## Why formatting is different from every other request

Hover, completion, go-to-definition and inlay hints all point *at a place*. Mapping them back is
a position-at-a-time job, which the transformers already do via `untransformPosition`.

Formatting rewrites the whole document. The edits only mean anything applied together, so the
response can't be mapped edit-by-edit — you have to apply them, get the formatted document, and
carry *that* back to the original's coordinates. rust-notebook-language-server#2 added
`unproject :: Params a -> a -> Doc -> Maybe Doc` to the `Transformer` class for exactly this: the
dual of `project`, returning `Nothing` when a transformer threw away something it would need.

It works for Rust because the whole projection is "wrap the cell in `fn main() { … }`": drop two
lines, undo one level of indentation, done.

It doesn't work for the others because their projections move lines around:

| Proxy | Projection |
| --- | --- |
| cpp, go | `DeclarationSifter` — a line *permutation* (it stores `forward`/`inverse` index vectors), plus a package header, plus a wrapper function, plus a 2-column indent on the wrapper body |
| haskell | Seven transformers: directives to pragmas, directives stripped, expressions and statements rewritten to declarations, imports sifted to the top, an injected import, pragmas sifted above the imports |

All of those depend on knowing which output line came from which input line. A formatter destroys
that: it splits long lines, joins short ones, inserts and deletes blanks.

## The idea: track lines through the format with marker comments

Tag each line the projection moves with a comment carrying its original index, so the mapping can
be read back out of the formatted document:

```go
import "fmt"  //@3
func _notebookExec() {
  x := 1          //@0
  fmt.Println(x)  //@1
}
```

### What was measured

| Formatter | Marker survives? | Notes |
| --- | --- | --- |
| gofmt | yes | stays attached to its line, gets aligned; stripping it leaves alignment padding to trim. No column limit, so no distortion |
| clang-format | yes | stays attached; when a line is split the marker lands on the **last** fragment |
| ormolu | yes, but | `--@0` is a **parse error** — Haskell lexes `--@` as an operator, not a comment. It has to be `-- @0`. ormolu also inserts blank lines of its own, which is fine |

One real catch, demonstrated with clang-format: the marker consumes column budget. A 78-character
line that clang-format leaves alone gets split once a 6-character marker is appended. So markers
change the formatted result, not just the bookkeeping.

Mitigation: fixed-width markers plus a `ColumnLimit` raised by exactly that width. The proxy owns
the shadow directory, so it can write a `.clang-format` there. ormolu has no column setting, and
gofmt has no limit at all.

### What an implementation needs

1. **Attribution.** Every output line maps to an original index or to "synthetic". A marked line
   maps to its index; a run of unmarked lines attaches to the following marker (matching where
   clang-format leaves it after a split). Header and wrapper lines are identified positionally or
   carry their own markers.
2. **Reassembly.** Group output lines by original index, sort, concatenate, strip the markers and
   the whitespace they attracted.
3. **Knowing when to stop.** If an index has vanished because the formatter deleted its line, or
   the indices come back in an order that can't be reconciled, return `Nothing`. The `Maybe` in
   `unproject` is where that lives.

## The better shape: format against a scratch document

Markers in the real shadow document would also be seen by diagnostics, completion and hover. You
only want them while formatting, which points somewhere else:

On a formatting request, the proxy opens a *second* document (`<shadow>.fmt.<ext>`) holding the
sub-document projected **for formatting only**, asks the server to format that, reads the result,
and closes it.

That buys three things:

- Markers never touch the document everything else uses.
- The formatting projection can be far simpler than the analysis one. cpp and go only need "make
  it parse" — a package header and a function wrapper, no sifting, no permutation. At that point
  `unproject` is the easy Rust case and **no markers are needed at all**.
- The proxy owns the scratch file's directory, so clang-format's column budget is fixable there.

Costs: two extra round trips per format, and the server will publish diagnostics against the
scratch URI which the proxy has to swallow.

## Effort

- **cpp, go** — port `unproject` to their `Transformer` class, add the scratch-document path with
  a minimal wrapper. No markers. Roughly a day each with tests. This is the one worth doing.
- **haskell** — the hard one. Even a minimal projection still needs `ExpressionToDeclaration` and
  `StatementToDeclaration` to make a cell parse, and those rewrite lines in place. They're
  invertible per line if the rewrite is recorded, but the attribution still has to survive ormolu,
  so Haskell genuinely needs the marker scheme. Several days, and the `-- @N` spacing quirk is a
  reminder that each language wants its own marker syntax.

## Check this first

Everything above assumes a formatting request covers a whole per-language sub-document. codedown
routes requests to a child server by the line the request starts on, and asks for inlay hints once
per cell. If it also formats per cell, the scratch document should hold just that cell, and all of
this gets easier. Worth confirming on the codedown side before building anything.

## Current state

- Rust: works. rust-notebook-language-server#2, merged.
- C++, Go, Haskell: formatting a cell fails with a server error. Inlay hints are fixed separately
  (cpp-notebook-language-server#1, go-notebook-language-server#1,
  haskell-notebook-language-server#2). Plain `.rs` / `.go` / `.cpp` / `.hs` files were never
  affected — the proxies leave a non-notebook URI alone, so formatting already worked there, and
  the tests in `tests/app/Spec/Tests/` cover that path.
