# codedown-languages

Nix modules that build reproducible language environments: one declarative config gives you
a Jupyter kernel, a language server, package management, and the notebook features that hang
off them, for every language listed below.

See the available options in [OPTIONS.md](./OPTIONS.md).

## Feature matrix

<!-- BEGIN FEATURE MATRIX -->

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="docs/feature-matrix-dark.svg">
  <img alt="Feature support by language" src="docs/feature-matrix.svg" width="100%">
</picture>

<details>
<summary>The same data as tables</summary>

✅ supported &nbsp;&nbsp; – not available &nbsp;&nbsp; ? not measured

#### Notebook

| Language | Jupyter kernel | REPL | Variable inspector | Debugger | Syntax highlighting |
| --- | --- | --- | --- | --- | --- |
| Bash | ✅ | – | ✅ | – | ✅ |
| C++ 23 | ✅ | ✅ | – | – | ✅ |
| Clojure | ✅ | ✅ | ✅ | – | ✅ |
| Coq | ✅ | – | – | – | ✅ |
| Go | ✅ | – | – | – | ✅ |
| Haskell | ✅ | ✅ | – | – | ✅ |
| Julia | ✅ | – | ✅ | – | ✅ |
| Octave | ✅ | ✅ | ✅ | – | ✅ |
| PostgreSQL | ✅ | – | – | – | ✅ |
| PyPy | ✅ | ✅ | ✅ | ✅ | ✅ |
| Python | ✅ | ✅ | ✅ | ✅ | ✅ |
| R | ✅ | ✅ | ✅ | – | ✅ |
| R (Ark) | ✅ | ✅ | ✅ | ✅ | ✅ |
| Ruby | ✅ | – | ✅ | – | ✅ |
| Rust | ✅ | – | ✅ | – | ✅ |

#### Code intelligence

| Language | Language server | Code completion | Hover docs | Signature help | Diagnostics | Semantic highlighting | Inlay hints |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Bash | ✅ | ✅ | ✅ | – | ✅ | – | – |
| C++ 23 | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Clojure | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | – |
| Coq | – | – | – | – | – | – | – |
| Go | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Haskell | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Julia | ✅ | ✅ | ✅ | ✅ | ✅ | – | ✅ |
| Octave | – | – | – | – | – | – | – |
| PostgreSQL | – | – | – | – | – | – | – |
| PyPy | ✅ | ? | ? | ? | ? | ? | ? |
| Python | ✅ | ✅ | ✅ | ✅ | ✅ | – | – |
| R | ✅ | ✅ | ✅ | ✅ | ✅ | – | – |
| R (Ark) | – | – | – | – | – | – | – |
| Ruby | ✅ | ✅ | ✅ | ✅ | ✅ | – | – |
| Rust | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

#### Navigation

| Language | Jump to definition | Jump to type definition | Find references | Document outline | Workspace symbol search | Highlight occurrences |
| --- | --- | --- | --- | --- | --- | --- |
| Bash | ✅ | – | ✅ | ✅ | ✅ | ✅ |
| C++ 23 | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Clojure | ✅ | – | ✅ | ✅ | ✅ | ✅ |
| Coq | – | – | – | – | – | – |
| Go | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Haskell | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Julia | ✅ | – | ✅ | ✅ | ✅ | ✅ |
| Octave | – | – | – | – | – | – |
| PostgreSQL | – | – | – | – | – | – |
| PyPy | ? | ? | ? | ? | ? | ? |
| Python | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| R | ✅ | – | ✅ | ✅ | ✅ | ✅ |
| R (Ark) | – | – | – | – | – | – |
| Ruby | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Rust | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

#### Editing

| Language | Formatting | Rename symbol | Code actions |
| --- | --- | --- | --- |
| Bash | ✅ | ✅ | ✅ |
| C++ 23 | ✅ | ✅ | ✅ |
| Clojure | ✅ | ✅ | ✅ |
| Coq | – | – | – |
| Go | ✅ | ✅ | ✅ |
| Haskell | ✅ | ✅ | ✅ |
| Julia | ✅ | ✅ | ✅ |
| Octave | – | – | – |
| PostgreSQL | – | – | – |
| PyPy | ? | ? | ? |
| Python | – | ✅ | ✅ |
| R | ✅ | ✅ | ✅ |
| R (Ark) | – | – | – |
| Ruby | – | ✅ | – |
| Rust | ✅ | ✅ | ✅ |

#### Packages

| Language | Package management | Package search |
| --- | --- | --- |
| Bash | – | – |
| C++ 23 | – | – |
| Clojure | – | – |
| Coq | ✅ | ✅ |
| Go | – | – |
| Haskell | ✅ | ✅ |
| Julia | ✅ | ✅ |
| Octave | ✅ | ✅ |
| PostgreSQL | – | – |
| PyPy | ✅ | ✅ |
| Python | ✅ | ✅ |
| R | ✅ | ✅ |
| R (Ark) | ✅ | ✅ |
| Ruby | ✅ | ✅ |
| Rust | ✅ | ✅ |

</details>

_15 languages, 23 features. Regenerate with `scripts/update-feature-matrix`._

<!-- END FEATURE MATRIX -->

### How it's generated

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
- `scripts/render-feature-matrix.py` turns the resulting JSON into the SVG above and the
  Markdown tables.

To regenerate:

```bash
# Only needed after a nixpkgs bump or a language server change; builds every kernel.
scripts/probe-lsp-capabilities

# Fast: re-evaluates and rewrites docs/ and this README.
scripts/update-feature-matrix
```

`docs/feature-matrix.json` is the machine-readable form, and is meant to be consumed
directly by anything that wants to render this data elsewhere. Its shape:

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
which language server or REPL is behind it.

### Known gaps

- PyPy's language server columns read `unknown` because the PyPy environment doesn't
  currently build (`mypy-1.17.1 not supported for interpreter pypy3.11`), so there was
  nothing to probe. Its servers are the same ones the CPython kernel uses.
- The Julia probe environment pins Julia 1.10 (see `nix/single-kernel-env.nix`); Julia 1.12,
  the default, can't resolve a package closure through julia-modules.
- Ark's language server is spoken over a Jupyter comm rather than published as a language
  server config, so R (Ark) shows no LSP features even though the kernel embeds one.
