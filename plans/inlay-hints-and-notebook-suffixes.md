# Inlay hints: what the channel needs to change

Context from implementing LSP inlay hints in codedown (codedownio/codedown#1388,
part of codedownio/codedown#1366). The codedown side works: the request is
allowed, routed to the right server, positions are translated back to notebook
coordinates, and the hints are drawn in the editor. No language server the
channel ships can currently produce a hint in a notebook, for two separate
reasons.

## How codedown asks

A notebook is projected to each language server as a sub-document: the lines
belonging to other languages are blanked, and the file is named
`<notebook><notebook_suffix>`, e.g. `main.ipynb.py` for jedi. The server is
told the language through `language_id` in the `didOpen`.

Requests are routed to a child server **by the line the request starts on**. So
codedown asks for inlay hints once per code cell, over that cell's own lines,
rather than once over the whole document.

## Issue 1: rust-analyzer has type hints turned off

rust-analyzer advertises `inlayHintProvider` and answers the request, but with
no hints. Its reported configuration is:

```json
"inlayHints": {
  "bindingModeHints": false,
  "chainingHints": false,
  "closingBraceHints": {"enable": true, "minLines": 25},
  "closureReturnTypeHints": false,
  "lifetimeElisionHints": {"enable": "never", "useParameterNames": false},
  "parameterHints": false,
  "reborrowHints": "never",
  "renderColons": true,
  "typeHints": {"enable": false, "hideClosureInitialization": false, "hideNamedConstructor": false}
}
```

`typeHints.enable` is false, so `let x = 5;` produces nothing. These are
rust-analyzer's own server-side defaults; the editor extensions that people
associate with rust-analyzer turn them on from the client side, which we don't.

Fix: set them in the rust-analyzer language server's `initialization_options`,
the same mechanism `pylsp_initialization_options.nix` already uses. Something
like:

```nix
initialization_options = {
  inlayHints = {
    typeHints.enable = true;
    parameterHints = true;
    chainingHints = true;
  };
};
```

Worth deciding which categories are wanted by default rather than enabling
everything; parameter hints in particular are divisive.

## Issue 2: rust-analyzer and clangd have no notebook_suffix

Neither language server sets `notebook_suffix`, so a notebook's sub-document is
named exactly `main.ipynb`, with no extension telling the server what it is.

- rust-analyzer tolerates this. It analyses the file from the `language_id` and
  answers hovers, code actions and completions correctly.
- clangd does not. It refuses to build an AST for a file it doesn't recognise
  and answers `-32602: trying to get AST for non-added document` to every
  request that needs one, including inlay hints. clangd in a notebook is
  effectively non-functional today.

Fix: give both a `notebook_suffix`, as jedi (`.py`), python-lsp-server (`.py`),
bash-language-server (`.bash`) and tinymist (`.typ`) already do:

- rust-analyzer: `notebook_suffix = ".rs"`
- clangd: `notebook_suffix = ".cpp"`

Other servers advertising `inlayHintProvider` in `nix/lsp-capabilities.json`
are worth the same check: gopls, haskell-language-server,
typescript-language-server and julia's LanguageServer.

## Related: formatters ship without their tools

Same shape of problem, found while adding the format command
(codedownio/codedown#1381). Servers advertise a formatting capability and then
answer with no edits because the tool they format with isn't installed.

- bash-language-server: fixed, `shfmt` added to its wrapper PATH.
- python-lsp-server: still affected. It is built from the bare package, so
  neither `autopep8` nor `yapf` is importable and both `textDocument/formatting`
  and `textDocument/rangeFormatting` return nothing. Adding `ps.autopep8` to the
  environment in `modules/kernels/python/language_servers/language_server_pythonlsp/config.nix`
  would make Python formatting work.

`nix/lsp-capabilities.json` records what each server *advertises*, which is not
the same as what it can do. Six servers advertise range formatting; at least two
of them could not actually format until the tool was added. It may be worth a
note in that file, or a probe that asks for a formatting result rather than only
reading the capability.

## No Python server here gives inlay hints, and basedpyright would

Checked against the shipped packages rather than `nix/lsp-capabilities.json`,
which only records servers that are enabled by default (for python3 that is
jedi alone, so the file says nothing about the others):

| server | implements inlay hints |
|---|---|
| jedi | no |
| pyright 1.1.411 | no |
| python-lsp-server 1.14.0 | no |
| basedpyright 1.39.3 | yes |

pyright looks like a false positive if you grep for it: `inlayHintProvider`
appears twice in its tree, but both are in the bundled
`vscode-languageserver-protocol`, which every server using that library
contains. Its own `pyright-internal.js` and `pyright-langserver.js` have zero
occurrences. Open-source pyright never shipped hints; they were a Pylance
feature. python-lsp-server has no occurrence of "inlay" anywhere.

basedpyright is a fork of pyright that re-adds the features Microsoft keeps in
the closed-source Pylance: inlay hints and semantic highlighting, with the
language server packaged so it works outside VS Code. It merges upstream
pyright, so the type-checking engine is the same code. Its
`basedpyright-langserver` sets `inlayHintProvider: true` and registers four
`onInlayHint` handlers.

It is already in the nixpkgs this channel pins
(`e7215ec9581d62b85a3b1b870cd40cc353a06816`), at 1.39.3, as `pkgs.basedpyright`,
so adding it needs no pin change. Besides hints it would also give Python
semantic tokens, which is the other feature codedown wants
(P1.6 in codedown's plan).

Two things to weigh:

- It adds stricter rules of its own on top of pyright, plus a baseline
  mechanism for adopting it on existing code. Those are configurable, but the
  defaults should be looked at before shipping it to users.
- Our pyright is configured with `pyright.disableLanguageServices = true`, so
  today it acts as a type checker beside jedi rather than as the editor server.
  basedpyright would only provide hints if it is allowed to provide language
  services, which is a decision about how the Python servers divide the work
  rather than a packaging detail.

## How to tell it worked

In codedown, `codedown-tests/lsp/app/Spec/InlayHints.hs` currently asserts only
that the request goes out over the cell's lines, because nothing can answer it.
Once rust-analyzer's type hints are on, that test can assert the rendered hint
text instead: a cell containing `let x = 5;` should draw `: i32`. The same spec
has the assertion written and commented in its history.

If basedpyright lands instead, the test is better off in Python: the standard
LSP test environment already has a Python kernel, so it would need no new nix
environment, and a cell containing `x = 5` should draw `: int`. The Rust
environment exists only because it was the cheapest server that advertised
hints; a C++ one was tried and rejected for adding 5.6 GiB to the test binary
cache, against 0.18 GiB for Rust, which shares a toolchain with an environment
already cached.
