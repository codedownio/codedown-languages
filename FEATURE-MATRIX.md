# Feature matrix

Which languages support which notebook and editor features. The graphic version is in the
[README](./README.md); this is the same data as tables, plus how it's produced.

<!-- BEGIN FEATURE MATRIX -->

✅ supported &nbsp;&nbsp; – not available &nbsp;&nbsp; ? not measured

### Notebook

| Language | Jupyter kernel | REPL | Variable inspector | Debugger | Syntax highlighting |
| --- | --- | --- | --- | --- | --- |
| Bash | ✅ | ✅ | ✅ | – | ✅ |
| C++ 23 | ✅ | ✅ | – | – | ✅ |
| Clojure | ✅ | ✅ | ✅ | – | ✅ |
| Coq | ✅ | ✅ | – | – | ✅ |
| Go | ✅ | ✅ | – | – | ✅ |
| Haskell | ✅ | ✅ | – | – | ✅ |
| JavaScript | ✅ | ✅ | – | – | ✅ |
| Julia | ✅ | ✅ | ✅ | – | ✅ |
| Octave | ✅ | ✅ | ✅ | – | ✅ |
| PostgreSQL | ✅ | ✅ | – | – | ✅ |
| PyPy | ✅ | ✅ | ✅ | ✅ | ✅ |
| Python | ✅ | ✅ | ✅ | ✅ | ✅ |
| R | ✅ | ✅ | ✅ | – | ✅ |
| R (Ark) | ✅ | ✅ | ✅ | ✅ | ✅ |
| Ruby | ✅ | ✅ | ✅ | – | ✅ |
| Rust | ✅ | ✅ | ✅ | – | ✅ |
| TypeScript | ✅ | ✅ | – | – | ✅ |

### Code intelligence

| Language | Language server | Code completion | Hover docs | Signature help | Diagnostics | Semantic highlighting | Inlay hints |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Bash | ✅ | ✅ | ✅ | – | ✅ | – | – |
| C++ 23 | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Clojure | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | – |
| Coq | – | – | – | – | – | – | – |
| Go | ✅ | ✅ | ✅ | ✅ | ✅ | – | ✅ |
| Haskell | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| JavaScript | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Julia | ✅ | ✅ | ✅ | ✅ | ✅ | – | ✅ |
| Octave | – | – | – | – | – | – | – |
| PostgreSQL | – | – | – | – | – | – | – |
| PyPy | ✅ | ✅ | ✅ | ✅ | ✅ | – | – |
| Python | ✅ | ✅ | ✅ | ✅ | ✅ | – | – |
| R | ✅ | ✅ | ✅ | ✅ | ✅ | – | – |
| R (Ark) | – | – | – | – | – | – | – |
| Ruby | ✅ | ✅ | ✅ | ✅ | ✅ | – | – |
| Rust | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| TypeScript | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

### Navigation

| Language | Jump to definition | Jump to type definition | Find references | Document outline | Workspace symbol search | Highlight occurrences |
| --- | --- | --- | --- | --- | --- | --- |
| Bash | ✅ | – | ✅ | ✅ | ✅ | ✅ |
| C++ 23 | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Clojure | ✅ | – | ✅ | ✅ | ✅ | ✅ |
| Coq | – | – | – | – | – | – |
| Go | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Haskell | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| JavaScript | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Julia | ✅ | – | ✅ | ✅ | ✅ | ✅ |
| Octave | – | – | – | – | – | – |
| PostgreSQL | – | – | – | – | – | – |
| PyPy | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Python | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| R | ✅ | – | ✅ | ✅ | ✅ | ✅ |
| R (Ark) | – | – | – | – | – | – |
| Ruby | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Rust | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| TypeScript | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

### Editing

| Language | Formatting | Rename symbol | Code actions |
| --- | --- | --- | --- |
| Bash | ✅ shfmt | ✅ | ✅ |
| C++ 23 | ✅ clang-format | ✅ | ✅ |
| Clojure | ✅ cljfmt | ✅ | ✅ |
| Coq | – | – | – |
| Go | ✅ gofmt | ✅ | ✅ |
| Haskell | ✅ ormolu | ✅ | ✅ |
| JavaScript | ✅ tsserver | ✅ | ✅ |
| Julia | ✅ JuliaFormatter | ✅ | ✅ |
| Octave | – | – | – |
| PostgreSQL | – | – | – |
| PyPy | – | ✅ | ✅ |
| Python | – | ✅ | ✅ |
| R | ✅ styler | ✅ | ✅ |
| R (Ark) | – | – | – |
| Ruby | ✅ rubocop | ✅ | – |
| Rust | ✅ rustfmt | ✅ | ✅ |
| TypeScript | ✅ tsserver | ✅ | ✅ |

### Packages

| Language | Subpackage management |
| --- | --- |
| Bash | – |
| C++ 23 | – |
| Clojure | – |
| Coq | ✅ |
| Go | – |
| Haskell | ✅ |
| JavaScript | ✅ |
| Julia | ✅ |
| Octave | ✅ |
| PostgreSQL | – |
| PyPy | ✅ |
| Python | ✅ |
| R | ✅ |
| R (Ark) | ✅ |
| Ruby | ✅ |
| Rust | ✅ |
| TypeScript | ✅ |

<!-- END FEATURE MATRIX -->

## How it's generated

The matrix is derived from the repo, not maintained by hand:

- `nix/feature-matrix.nix` evaluates the module system with every kernel enabled and reads
  the facts off the built kernels — whether there's a variable inspector option, whether the
  kernel speaks the Jupyter debug protocol, which language servers it enables, whether it has
  a package set, and so on.
- Language-server-backed columns (completion, hover, jump to definition, …) can't be answered
  by evaluating Nix, because they're whatever the server says at runtime. `scripts/probe-lsp-capabilities`
  builds a single-kernel environment per language, starts each language server, performs the
  LSP `initialize` handshake, and records the advertised capabilities into
  `nix/lsp-capabilities.json`.
- The Formatting column needs both halves — see [Formatting](#formatting) below.
- `scripts/render-feature-matrix.py` turns the resulting JSON into the SVG in the README and
  the Markdown tables above.

To regenerate:

```bash
# Only needed after a nixpkgs bump or a language server change; builds every kernel.
scripts/probe-lsp-capabilities

# Fast: re-evaluates and rewrites docs/, the README and this file.
scripts/update-feature-matrix
```

## The JSON

`docs/feature-matrix.json` is the machine-readable form, and is meant to be consumed directly
by anything that wants to render this data elsewhere. Its shape:

```jsonc
{
  "schemaVersion": 1,
  "groups":   [{ "id": "core", "name": "Notebook" }, ...],
  "features": [{ "id": "hover", "name": "Hover docs", "group": "intelligence",
                 "description": "...", "source": "lsp" }, ...],
  "languages": [{
    "id": "python3",
    "displayName": "Python",
    "version": "3.13.12",
    "extensions": ["py"],
    "languageServers": { "available": [...], "enabledByDefault": [...], "probed": [...] },
    "support": { "hover": { "level": "full", "detail": "jedi" }, ... }
  }, ...]
}
```

`level` is `full`, `none`, or `unknown` (nothing probed that kernel yet), and `detail` says
which language server or REPL is behind it. A feature can also carry `"render": "label"`,
which asks the renderers to print `detail` in the cell rather than a check mark; `formatting`
is the one that does, so its cell names the formatter.

## Formatting

Formatting is the one column that can't be read off the `initialize` response alone. A server
will happily advertise `documentFormattingProvider` and then fail every request because the
tool behind it isn't installed — bash-language-server did exactly that until shfmt was added
to its wrapper. So the column is the conjunction of two things: the server advertises
formatting, *and* the kernel names the formatter behind it in `passthru.formatters` (declared
next to each language server's config and aggregated in the kernel's `default.nix`). That name
is what the cell prints.

| Language | Formatter | Comes from | Setting |
| --- | --- | --- | --- |
| Bash | shfmt | separate binary on bash-language-server's PATH | — |
| C++ | clang-format | built into clangd | — |
| Clojure | cljfmt | built into clojure-lsp | — |
| Go | gofmt, gofumpt | both vendored into gopls | `kernels.go.lsp.gopls.formatter` |
| Haskell | ormolu, fourmolu, stylish-haskell | all compiled into haskell-language-server | `kernels.haskell.lsp.haskell-language-server.formattingProvider` |
| JavaScript / TypeScript | tsserver | built into typescript-language-server | — |
| Julia | JuliaFormatter | dependency of LanguageServer.jl | a `.JuliaFormatter.toml` in the workspace |
| Python | autopep8, yapf, black, ruff | none ship with python-lsp-server | `kernels.python3.lsp.python-lsp-server.formatter` |
| R | styler | already an R dependency of languageserver | — |
| Ruby | rubocop | already a gem dependency of solargraph | `kernels.ruby.lsp.solargraph.formatting` |
| Rust | rustfmt | separate binary on rust-analyzer's PATH | — |
| Typst | typstyle, typstfmt | both vendored into tinymist | `exporters.typst.lsp.tinymist.formatter` |

Where one formatter is listed and there's no setting, it costs nothing to ship (already in the
server's closure, or a small binary) and is on unconditionally. Where several are listed, the
setting picks between them; they're all built into the server, so the choice doesn't change
what gets built — except for Python, where python-lsp-server ships with no formatter at all and
the setting decides which one is added to the environment.

Each of these has a test that formats a deliberately misformatted document over LSP and checks
what comes back (`itFormatsAs` in `tests/src/TestLib/LSP.hs`), because advertising the
capability proves nothing.

Caveats the table above can't show:

- Python's default language server is Jedi, which has no formatting support of any kind. The
  `formatter` setting only applies when `python-lsp-server` is also enabled, which it isn't by
  default — so Python's Formatting cell is empty.
- Typst is an exporter rather than a kernel, so tinymist isn't probed and Typst has no row in
  the matrix. tinymist already formatted with typstyle by default; the setting exists to pick
  typstfmt or turn formatting off.
- Rust formats correctly when rust-analyzer is driven directly, but
  rust-notebook-language-server forwarded `textDocument/formatting` with the notebook's URI
  instead of the shadow file's, so rust-analyzer answered "file not found".
  [rust-notebook-language-server#2](https://github.com/codedownio/rust-notebook-language-server/pull/2)
  fixes it; the Rust formatting test stays pending until that's released and the pinned
  version here moves up.
- The R formatting test is pending too. styler produces the right answer — you can watch it
  come back over the wire in the session log — but the R language server then never answers
  lsp-test's `shutdown` and the session hangs rather than finishing.

## Known gaps

- PyPy's language server columns read `unknown` because the PyPy environment doesn't
  currently build (`mypy-1.17.1 not supported for interpreter pypy3.11`), so there was
  nothing to probe. Its servers are the same ones the CPython kernel uses.
- Ark's language server is spoken over a Jupyter comm rather than published as a language
  server config, so R (Ark) shows no LSP features even though the kernel embeds one.

## REPLs

A kernel's REPLs are declared as `repls` in its `default.nix` and reach the runtime through
`metadata.codedown.repls` in `kernel.json`, where each entry is an `attr`, a `display_name`,
and a `proc` to run in a PTY. `modules/kernels/common.nix` converts between the two.

Most languages get their own interpreter (`ghci`, `irb`, `evcxr`, `ipython`, …). Go, Coq and
PostgreSQL have no interpreter worth attaching to, so they run the kernel itself under
`jupyter-console` via nixpkgs' `jupyter-console.withSingleKernel`, which builds a standalone
kernelspec — the console works without the surrounding codedown environment being installed.
