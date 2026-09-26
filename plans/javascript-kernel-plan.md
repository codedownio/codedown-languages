# Plan: bring back the JavaScript / Node.js kernel, with D3 as a subpackage

## Goal

A `javascript` kernel in `modules/kernels/`, with the same shape as the other kernels
(kernelspec + LSP + REPL + searchable subpackages), where selecting the `d3` subpackage
gives a notebook that can draw D3 visualizations.

The old `old_languages/javascript` is not a useful starting point: it used ijavascript via
`node2nix`, and `nodePackages` / `node2nix` are both gone from nixpkgs 26.05.

## What was verified up front

Run against the pinned nixpkgs (`release-26.05`, nodejs 24.19.0, zeromq 4.3.5):

- **The native ZeroMQ binding builds offline.** zeromq.js 6.8.0's addon compiles from a
  single `g++ -std=c++20 -shared -fPIC src/*.cc` against `pkgs.zeromq` + node-addon-api
  headers — no cmake-ts, no vcpkg, no `FetchContent`. A req/rep round-trip passes. (Upstream's
  CMake path wants network at build time; we skip it entirely.)
- **tslab runs as a real kernel on Node 24.** With that addon dropped in and
  `lib/load-addon.js` patched to `require("../build/addon.node")`, `jupyter_client` starts
  `tslab kernel --config-path {connection_file} --js`, and `1+1` / `console.log` work.
- **D3 works in the kernel.** `require('d3')` works (Node 24 can require ESM), `await import('d3')`
  works, and `require('tslab').display.html(...)` emits a `text/html` `display_data` —
  including one containing a `<script>` tag. A jsdom + d3 cell produced a real SVG.
- **Deno's kernel can consume a Nix-built `node_modules`, offline.** With a prebuilt
  `node_modules` + `package.json` in the kernel's cwd, an empty `DENO_DIR` and HTTP(S) proxies
  pointed at a dead port, `import * as d3 from 'd3'` and `createRequire(...)('jsdom')` both
  work. A pre-populated `DENO_DIR` also resolves `npm:d3` offline from *any* cwd. So package
  management is genuinely orthogonal to the kernel choice.
- **The deno in our pin is broken for Jupyter; upstream deno is not.** Using Deno's own
  installed kernelspec and driving it with `nbclient` (not a hand-rolled client), nixpkgs'
  deno 2.8.3 swallows everything: `throw new Error('boom')` produces no output on any channel
  and reports `status: ok`. Official `deno` binaries 2.9.7 and 2.4.0, same notebook, behave
  correctly. So this is a nixpkgs packaging problem, not a Deno problem.

  | cell | deno 2.8.3 (pin) | deno 2.9.7 (official) | tslab |
  | --- | --- | --- | --- |
  | `1 + 1` | nothing | `execute_result` `2` | prints `42`-style value to stdout |
  | `throw new Error('boom')` | **nothing, `status: ok`** | `error`, full traceback | stack on stderr, `status: error` |
  | `undefinedFn()` | **nothing, `status: ok`** | `error: ReferenceError` | `Cannot find name 'undefinedFn'` (static) |
  | `const = ;` | `status: error`, empty traceback | `error` | `1:7 - Variable declaration expected.` |
  | ``Deno.jupyter.html`<b>hi</b>` `` last expr | nothing | `execute_result` with `text/html` | n/a (explicit `display.html`) |

  Deno's rich-display story is actually *better* than tslab's: a last expression can carry a
  MIME bundle, where tslab needs an explicit `display.html(...)` and otherwise prints to stdout.

## Kernel choice: tslab

| | tslab | ijavascript | deno |
| --- | --- | --- | --- |
| npm downloads/month | 522k | 1.6k | n/a (in deno) |
| GitHub stars / last push | 844 / 2024-06 | 2.3k / 2024-07 | first-party, active |
| Runtime | Node | Node | Deno |
| ZeroMQ | zeromq 6 (N-API) | jmp → zeromq 5 (nan) | built into binary |
| Rich display | `tslab.display.html/png/...` | `$$.html()` | `Deno.jupyter.display()` |
| TypeScript | second kernel, same package | no | native |

This is a closer call than it first looked, and it is not decided by display quality — deno
2.9.7's is better. It is decided by **how a selected package reaches the kernel**:

- **Deno ignores `NODE_PATH`** (verified). Bare specifiers resolve only from a `package.json` +
  `node_modules` in the kernel's cwd or an ancestor — i.e. the *user's* notebook directory,
  which we don't control. `deno jupyter` has no `--config` / `--node-modules-dir` flag to
  override that.
- The cwd-independent alternative is a Nix-built `DENO_DIR` npm cache plus `npm:` specifiers in
  every import. That cache is an undocumented, **version-coupled** format: a cache written by
  deno 2.8.3 silently fails to resolve under 2.9.7 — `import * as d3 from "npm:d3"` produced no
  output *and no error*. Building it ourselves means reverse-engineering
  `npm/registry.npmjs.org/<pkg>/<version>/` plus a per-package `registry.json`, and a silent
  break on some future deno release.
- **tslab takes `NODE_PATH` from the kernelspec `env`** and works from any cwd. That is exactly
  how every other kernel here injects its package set.

Secondary reasons, in tslab's favor:

- **It is Node**, which was the ask: `require`, `__dirname`, real Node globals, and curated
  packages tested against Node rather than a compat layer.
- **Static errors come for free** — cells go through TypeScript's checker even in JS mode, so
  `undefinedFn()` is reported before execution.
- **TypeScript is nearly free** — the same binary without `--js` is the TS kernel.
- It answers `complete_request` / `inspect_request` from TypeScript's services, so there is
  decent completion even before the LSP is wired up.
- zeromq 6 (N-API) is the binding we can actually build; ijavascript's zeromq 5 is nan-based
  and much riskier on Node 24. ijavascript is also unmaintained since 2021 and its users have
  been leaving over exactly that.

Cost of picking it: tslab's last release is June 2024 and we own the vendored zeromq build.
Both are contained — the addon build is ~15 lines of Nix, and tslab is small enough to fork if
upstream goes fully dark.

### Where that leaves Deno

Deno remains a reasonable choice if we'd rather be TypeScript-first, and it's the
better-maintained runtime. Picking it would mean:

- Fixing nixpkgs' deno (or fetching the official binary, as this repo already does for
  `rust-notebook-language-server`). The 2.8.3 breakage needs a root cause either way, since a
  silently-succeeding failed cell is the worst possible failure mode.
- Deciding the package story: either write `package.json` + a `node_modules` symlink into the
  user's workspace root, or build and own the `DENO_DIR` cache layout above.
- Accepting `--allow-all` — the Jupyter kernel documents that it ignores permissions, so Deno's
  sandbox buys nothing here.

Either way it slots in as a *second* kernel next to the Node one, sharing the same curated-lock
machinery; nothing in the package design below is Node-specific.

## Subpackages: a curated lockfile, not an index

The question was whether we need a Rust-style checked-in index. We don't, and we couldn't
have one anyway:

- The rust kernel's `all_package_names.nix` is 2.3 MB / 155k crate names.
- npm package *names alone* are 116 MB (~3.6M packages, per `all-the-package-names`). Adding
  versions, tarball URLs and integrity hashes puts a full index in the GB range. Out.
- A `package-lock.json` costs ~436 bytes per resolved entry (measured on a
  tslab + d3 + jsdom lock: 120 entries, 52 KB). A curated set of ~500 top-level libraries
  resolves to something like 5k–15k entries — **2–7 MB**, i.e. the same ballpark as the
  existing rust index, and it's a plain npm artifact we regenerate with one command.

And nixpkgs already has the machinery: **`pkgs.importNpmLock`** builds `node_modules` from a
checked-in `package-lock.json` by `fetchurl`-ing each tarball with `hash = module.integrity`
straight from the lock. No `npmDepsHash`, no IFD, no network at eval, and it has
`packageSourceOverrides` — which is exactly where our Nix-built zeromq goes. (`buildNpmPackage`
+ `fetchNpmDeps` would also work but hashes the whole dep set as one blob, which is worse for
subset selection. `node2nix`, `npmlock2nix` and friends are dead or not in the pin.)

So (this is built, in `modules/kernels/javascript/npm/`):

```
package.json        # the curated dependency list -- tslab plus what users can select
package-lock.json   # generated; the pin (URL + integrity for every package in the closure)
descriptions.json   # generated; name -> {description, homepage, license} for the searcher
update.sh           # regenerates both; refuses packages that need an install script
default.nix         # parses the lock; exposes packageOptions and mkNodeModules
zeromq.nix          # the native addon, built from the same lock's zeromq entry
```

`mkNodeModules { packages }` walks the lock from the selected names, takes the transitive
closure, and hands `importNpmLock.buildNodeModules` a lock filtered to it. `packageOptions` is
one entry per curated top-level name, fed to `common.searcher'` with
`packageMustBeDerivation = false` the way the rust kernel indexes crate names.

### Does the curated list scale, and can its packages conflict?

**Conflicts: no, and this is structural.** npm's `node_modules` is a tree, not a flat
single-version namespace like Python's site-packages or R's library. When two packages need
incompatible versions of the same dependency, npm nests one of them, and the closure walk
reproduces those paths exactly. Our current lock already does this in 13 places — a built
environment with `topojson-client` and `apache-arrow` selected contains:

```
node_modules/topojson-client/node_modules/commander  2.20.3
node_modules/tslab/node_modules/commander            10.0.1
```

So adding a package to the curated list cannot break another one. Re-resolving may *move* a
hoisted version, but that shows up as a version bump in the lock diff, not as a conflict. The
one case that can genuinely fail is a peer-dependency conflict, and npm fails that at
`./update.sh` time — where we see it — rather than for a user.

**Scaling: measured, and the lock is not the constraint.** Two candidate lists were built and
resolved for real.

*npm's most-downloaded packages* (`npm-high-impact`'s `npmTopDownloads`, 15,916 names), after
filtering out frameworks, bundlers, linters, test runners, CLI tooling, `@types/*` and native
addons:

| top-level | lock entries | lock size | resolve |
| --- | --- | --- | --- |
| 100 | 183 | 0.10 MB | 1 s |
| 500 | 807 | 0.39 MB | 5 s |
| 1000 | 1,664 | 0.80 MB | 8 s |

That list is nearly useless for notebooks, though — after filtering, the top 100 by downloads is
still `yallist`, `hasown`, `undici-types`, `is-fullwidth-code-point`. Download rank measures how
often something appears in a `node_modules`, not whether a person would ever import it.

*Notebook-relevant packages*, from the registry's popularity-ranked search across ~30 data and
visualization keywords (1,117 hits, 1,049 after the same filtering) — this list starts with
`d3`, `d3-shape`, `d3-scale`, `echarts`, `highcharts`, `vega-lite`, `vega`, `plotly.js`,
`cytoscape`, `deck.gl`:

| top-level | lock entries | lock size | resolve | needing install scripts |
| --- | --- | --- | --- | --- |
| 16 (today) | 239 | 0.10 MB | 1 s | 0 |
| 100 | 1,270 | 0.56 MB | 25 s | 6 |
| 250 | 3,181 | 1.48 MB | 57 s | 14 |
| 1049 | 11,236 | 6.03 MB | 324 s | 96 |

Real libraries pull ~11–13 lock entries each, against ~1.7 for the plumbing packages, so this is
the honest scaling curve. Even the whole 1,049-package list is 6 MB — larger than the rust
kernel's 2.3 MB crate-name index, but the same order, and it's a file npm regenerates in five
minutes.

A 101-package curated lock (the relevant top 100 plus tslab) was built and run through the
actual kernel machinery: selecting `echarts` alone produces a 74-package tree, selecting
`d3 + vega-lite + echarts + cytoscape + plotly.js-dist-min` produces 170, and an `echarts`
environment renders a chart to SVG in a cell. Nix eval time is flat — 1.3–1.9 s whether the
lock holds 239 or 1,665 entries, and that's mostly nixpkgs instantiation, not our parsing.

So the limits are: **~96 of 1,049 relevant packages need an install script** and have to be
excluded (or packaged individually in Nix, like `zeromq.nix`); npm resolution gets slow enough
at 1,000 that `update.sh` becomes a coffee break; and each added package costs closure size in
any environment that selects it (`echarts` alone is 65 MB on top of the 55 MB base). What
doesn't scale is a human deciding what belongs on the list. That's why the list is not
load-bearing:

### The curated list

56 packages, chosen for what a notebook reaches for rather than by download rank:

- **Charting** — `d3` (+ `d3-sankey`, `d3-cloud`), `@observablehq/plot`, `vega`, `vega-lite`,
  `echarts`, `highcharts`, `chart.js`, `plotly.js-dist-min`, `cytoscape`
- **Data** — `arquero` (dataframes), `apache-arrow`, `papaparse`, `exceljs`, `js-yaml`,
  `fast-xml-parser`, `sql.js` (SQLite compiled to WASM), `lodash`, `axios`, `cheerio`
- **Math / stats / ML** — `mathjs`, `simple-statistics`, `jstat`, `vega-statistics`,
  `probability-distributions`, `nerdamer` (symbolic), `ml-matrix`, `ml-pca`, `ml-kmeans`,
  `ml-regression`, `ml-random-forest`, `ml-distance`, `@tensorflow/tfjs`, `seedrandom`
- **Geo / graph** — `@turf/turf`, `topojson-client`, `graphology` (+ `-metrics`,
  `-layout-forceatlas2`)
- **Text** — `markdown-it`, `compromise` (NLP), `franc` (language detection)
- **Output** — `@napi-rs/canvas`, `pdf-lib`, `chroma-js`, `jsdom`
- **Misc** — `date-fns`, `dayjs`, `uuid`, plus `@types/*` for d3, jsdom, lodash, papaparse and
  topojson-client

Deliberately left out: `xlsx` (npm's copy is a stale 0.18.5 with open advisories; SheetJS
publishes elsewhere now), and anything needing a compiler that has a working alternative.

Each was exercised in a cell: arquero group-by, simple-statistics, chart.js → PNG,
vega-lite → SVG, turf, ml-matrix, tfjs (CPU backend), sql.js, pdf-lib, compromise, graphology,
markdown-it, franc, cheerio, mathjs, jstat, Observable Plot → SVG.

**One usability note worth documenting for users:** cells should use `import`, not `require`,
for packages that ship ESM-style default exports. `require('graphology')` fails tslab's type
check with "has no construct signatures" even though it works at runtime, because the cell is
compiled as CommonJS with `esModuleInterop`. `import Graph from 'graphology'` works, as does
top-level `await`. CJS-only packages are fine either way.

### Long-term: arbitrary package sets

Nothing in `npm/default.nix` knows the list is curated — it reads *a* lock. Pure Nix eval needs
a URL and an integrity hash for every package in the closure, which means resolution (semver
range → concrete version) has to happen somewhere with network access. Three places it can go:

1. **At environment-definition time (the intended path).** When a user changes their package
   selection, the thing that owns the environment definition runs
   `npm install --package-lock-only` — resolution only, no installs, no builds, a second or
   two — and stores the resulting lock alongside the environment. Nix then builds from that
   lock exactly as it does today; `mkNodeModules` takes the lock as a parameter. Add
   `--before=<date>` and the resolution is reproducible. This is the same shape as
   `cargoHash`/`vendorHash`, except the artifact is a lockfile a person can read.
2. **A checked-in registry snapshot plus a semver resolver written in Nix** — the analogue of
   the rust kernel's crate index. Possible for a top-N subset, but it means owning a resolver
   and a large generated artifact, and it buys nothing over (1) except skipping the resolve
   step.
3. **Resolving inside a derivation** — needs network, so a fixed-output derivation, so the
   output hash must be known in advance. That's a two-step "build it, copy the hash" dance,
   which is fine for a package definition and bad for a UI-driven package picker.

The concrete next step for (1) is a `kernels.javascript.packagesLockFile` option that points at
a generated lock, with the curated lock as the default. The searcher stays on the curated set
(that's what makes a *browsable* package list), while the lock option covers "I need this
specific thing from npm."

### Install scripts, and what actually blocks a package

An install script is not by itself a reason to exclude anything. Checking what the flagged
packages in the candidate set actually run:

| package | install script | verdict |
| --- | --- | --- |
| `core-js` | `node -e "try{require('./postinstall')}catch(e){}"` | funding banner |
| `es5-ext` | same shape | funding banner |
| `inferno` | `opencollective-postinstall` | funding banner |
| `@vaadin/vaadin-usage-statistics` | `node check.cjs` | telemetry |
| `@parcel/watcher` | `node scripts/build-from-source.js` | real native build |

So the build sets `npmRebuildFlags = ["--ignore-scripts"]` — the npm hook installs with
`--ignore-scripts` and then runs `npm rebuild`, which would have run them anyway. Skipping them
costs nothing and keeps the build offline. `update.sh` now *classifies* install scripts instead
of rejecting them: it fetches each one from the registry and fails only when the script actually
compiles something (`node-gyp`, `prebuild`, `cmake`, `build-from-source`, `make`).

That leaves two real cases:

- **Prebuilt-binary packages just work.** `@napi-rs/canvas` and friends publish one package per
  platform and select with the lock's `os`/`cpu` fields — no install script, no compiler. The
  closure walk filters those entries to the host platform (otherwise every environment would
  carry the Windows and macOS binaries, and npm refuses to install a mismatched one). Verified:
  `chart.js` renders a PNG through `@napi-rs/canvas` inside a cell.
- **Packages that really need a compiler need a Nix derivation**, the way `zeromq.nix` does. Two
  came up while choosing the list, and both had a better route: `chartjs-node-canvas` pulls
  node-canvas (cairo/pango/node-gyp) and was replaced by `@napi-rs/canvas`; `danfojs-node` pulls
  `@tensorflow/tfjs-node` (downloads libtensorflow at install) and was replaced by `arquero` for
  dataframes plus plain `@tensorflow/tfjs`, which runs on the CPU backend with no native code.

## D3 rendering: client-side, with a static fallback

Chosen: interactive, browser-side D3. The kernel emits `text/html` containing a container
`<div>`, the d3 bundle, and the user's drawing code as a `<script>`. This gives real zoom,
hover and transitions, which is the point of D3.

The frontend runs such scripts in a sandbox; the exact policy gets settled as we go. Two
assumptions that fall out of any strict sandbox, and cost nothing to hold to anyway: the d3
bundle and the data are **inlined** (no CDN fetch, which would be subject to the frame's CSP),
and the drawing code only ever touches its own container element — no `window.parent`, no
same-origin storage. If the sandbox turns out to be too restrictive, `renderStatic` below is
the fallback and is already verified working.

Shipped as a small in-repo package, `codedown-d3`, added to the kernel's `node_modules`
whenever `d3` is selected:

```js
const { render } = require('codedown-d3');

render((d3, el, data) => {
  const svg = d3.select(el).append('svg').attr('width', 600).attr('height', 400);
  // ...ordinary browser-side d3...
}, myData);
```

`render` serializes the callback's source plus a JSON-encoded `data`, wraps them in an IIFE
against a unique container id, and calls `tslab.display.html`. The d3 bundle is inlined
(~280 KB), guarded on `window.d3` so several outputs sharing a frame only pay once.

`renderStatic(fn, data)` is the same API against jsdom, emitting the SVG as `text/html`. It
works regardless of frontend script handling and survives export, so it's what tests should
assert on and what the docs should recommend for exported notebooks.

Both are verified: `render`'s output, loaded into jsdom with `runScripts: "dangerously"`, draws
the expected `<circle>` elements, and `renderStatic` produces the SVG directly in the kernel.

## The rest of the kernel

- **`kernel.nix`** — `common.makeJupyterKernel`, argv
  `${tslab}/bin/tslab kernel --config-path {connection_file} --js`, `env.NODE_PATH` pointing at
  the built `node_modules`, `attrs = ["javascript" "js"]`, `extensions = ["js" "mjs" "cjs"]`,
  `code_mirror_mode = "javascript"`, logos from `old_languages/javascript` plus a new
  monochrome SVG.
- **REPL** — `node` with `NODE_PATH` set, via a wrapper (every kernel has one since 59f03b4).
- **LSP** — `typescript-language-server` 5.3.0 from the pin, wrapped with `NODE_PATH` and a
  generated `jsconfig.json` (`allowJs`, `checkJs: false`, `typeRoots` and `paths` pointing at
  the environment's `node_modules`). `notebook_suffix = ".js"`; no notebook-LSP wrapper needed
  to start, since JS cells concatenate cleanly (unlike Go/Rust/C++).
- **Variable inspector** — deferred. tslab keeps cell state inside its executor with no listing
  command, so `variable_inspector` is null for now.
- **`module.nix`** — `enable`, `packages`, `nodejsPackage` (enum `nodejs` / `nodejs_22` /
  `nodejs_24`), `interface.attrs` / `interface.extensions`,
  `lsp.typescript-language-server.{enable,debug}`.

### The cwd problem, and why the kernel patches tslab

Everything awkward in this kernel comes from one fact: the kernel process's working directory
is the user's notebook directory, which the environment doesn't control. `NODE_PATH` in the
kernelspec `env` handles Node's own `require` at runtime. But tslab type-checks every cell
first, and **TypeScript ignores `NODE_PATH`** — it resolves modules and `@types` from the
file's directory upwards. Without something extra, `require('d3')` fails the type check with
"Cannot find module 'd3'" before it ever runs.

tslab already has half the answer: when the working directory has no `@types/node`, it falls
back to `@types` under its own `__dirname`. Since tslab lives inside the environment's
`node_modules`, `__dirname` is a path into the tree we built — so the derivation patches
`dist/converter.js` to derive `baseUrl`/`paths` the same way, and symlinks the tree's hoisted
`@types` into tslab's own `node_modules/@types`. No environment variable, and no derivation
referring to its own output path.

## TypeScript kernel

Cheap with tslab: the same binary without `--js`, a second kernelspec (`attrs = ["typescript" "ts"]`,
`extensions = ["ts"]`), its own icons, feature-matrix rows and tests. Plan it as a follow-on
commit in the same PR, behind `kernels.typescript.enable`, once the JS kernel's tests are green.

## Files

Added:

```
modules/kernels/javascript/
  module.nix, default.nix, kernel.nix
  npm/{package.json, package-lock.json, descriptions.json, update.sh, default.nix, zeromq.nix}
  codedown-d3/{index.js, index.d.ts, package.json}
  language_server_typescript/config.nix
  javascript-logo-{32x32,64x64}.png    # from old_languages/javascript
  javascript-monochrome.svg (+ .license)
sample_environments/javascript.nix
```

Edited: `nix/evaluate-config.nix` (register the module), `sample_environments.nix`.

Still to do: `tests/app/Spec/Tests/Javascript.hs` (+ `Tests.hs`), regenerate
`docs/feature-matrix*`, `FEATURE-MATRIX.md`, `nix/lsp-capabilities.json` and `OPTIONS.md`, and
remove `old_languages/javascript`.

## Tests

`tests/app/Spec/Tests/Javascript.hs` plus `Javascript/{Common,Packages,Display,Completion,Hovers}.hs`,
picked up automatically by sandwich-discover. The environment under test selects
`d3`, `jsdom`, `@types/d3` and `simple-statistics`:

- **Kernel** — `console.log`, a bare last expression printing its value, and top-level `await`
- **Packages** — `d3` and `simple-statistics` resolve from an empty working directory (they
  arrive through `NODE_PATH`), and `lodash` — curated but *not* selected here — does not
- **D3 display** — `renderStatic` emits `text/html` containing `<svg>`/`<circle>`; `render`
  emits the container id, a `<script>`, and the inlined d3 bundle
- **LSP** — completions for local variables, completions for `d3.` (`scaleLinear`, `scaleBand`),
  a hover on a local function, and a hover on `d3.scaleLinear` that mentions `ScaleLinear`

All 14 pass (`nix build .#tests` in `tests/`, then `tests --javascript`), in ~33 s.
The LSP ones are what caught the bug described below.

### What the language server needed

The server starts and answers completion/hover/definition/documentSymbol out of the box, but
it was doing nothing useful: hovering `d3.scaleLinear` gave `any`, and completion after `d3.`
returned 4 items — the words already in the file — instead of d3's API.

It's the cwd problem again. tsserver resolves modules and `@types` from the edited file's
directory upwards, and cells are written to the user's notebook directory. A `jsconfig.json`
beside the file with `paths`/`typeRoots` pointing at the environment's `node_modules` fixes it
completely: hover becomes `function scaleLinear<number, number, never>(...)` and completion
returns 577 items.

So `language_server_typescript/seed-workspace.js` is a small stdio proxy in front of
typescript-language-server: it watches for `initialize`, writes the environment's
`jsconfig.json` into each workspace root that has no TypeScript configuration of its own, and
passes every message through unchanged. It never overwrites an existing `jsconfig.json` or
`tsconfig.json`.

This is the lightweight version of what `go-notebook-language-server` and
`rust-notebook-language-server` do for their languages. If writing into the user's workspace
turns out to be unwelcome, the heavier alternative is the one those use: proxy the document
URIs into a shadow directory we prepare. JavaScript doesn't otherwise need that, since cells
concatenate cleanly.

## Platform notes

- The addon build is a normal C++ compile, so aarch64-linux under QEMU is slow but fine. Kernel
  tests there are the usual timing risk (see issue #96 and the python/julia exclusions) — if
  the JS tests turn out flaky under emulation, exclude the sample environment there rather than
  weakening the assertions.
- Darwin needs `-undefined dynamic_lookup` on the link line (upstream's CMake does the same).

## Rough steps

1. `modules/kernels/javascript/tslab/zeromq.nix` + `default.nix`: build the addon, build tslab
   with `importNpmLock`, override `node_modules/zeromq`. Smoke test: start the kernelspec by
   hand under `jupyter_client`.
2. `kernel.nix` / `module.nix` / `default.nix` with no packages and no LSP; register in
   `evaluate-config.nix`; add the sample environment. Get `testKernelStdout` green.
3. `npm/`: curated list, `update.sh`, lock, searcher wiring, `mkNodeModules`; `NODE_PATH` into
   the kernelspec and the REPL. Test with `packages = ["d3"]`.
4. `codedown-d3` + `misc.d3Include`; static path first (verifiable in tests), then the
   interactive path once the frontend's script handling is confirmed.
5. `typescript-language-server` config + generated `jsconfig.json`; completion/hover tests.
6. Regenerate feature matrix / options / lsp-capabilities; drop `old_languages/javascript`.
7. Follow-on: TypeScript kernel; then the variable inspector.

## Open questions

1. tslab prints a bare last expression's value to stdout rather than sending `execute_result`.
   Live with it, or patch tslab's executor to send a real `execute_result` (better rendering,
   and `Out[n]` history)?
2. How big should the curated package list be at launch — the ~15 above, or go straight to a
   few hundred pulled from a downloads ranking?
3. Worth reporting the nixpkgs deno 2.8.3 Jupyter breakage upstream regardless of which kernel
   we ship? It's a silent-failure bug and the root cause is unknown.
