## Describe the bug

In restricted or pure evaluation mode, evaluation fails with `error: path '...' is not valid` when a
fixed-output derivation resolves to the same store path as an eval-time fetch whose contents were
read earlier in the same evaluation, and that path was not already in the store.

The failure is deterministic, not a race — a single process reproduces it. The path is actually
created during the failing evaluation and is present in the store afterwards, so re-running the
identical command immediately succeeds. That makes it look intermittent in CI: the first evaluation
after a dependency bump fails and every one after it passes.

## Steps To Reproduce

`builtins.fetchTree` and `pkgs.fetchFromGitHub` on the same rev are content-addressed to the same
store path, so this is one download reached two ways, not two fetches. The script deletes the path
first so the result doesn't depend on what you already have.

```bash
#!/usr/bin/env bash
REV=217acc55c4d1b7fe068c85dd62fbce9339a5afc4
HASH='sha256-k7ummovy8O1PeL3FpWYZdq1ii8J0XtBh0n4DHqfyiyU='

EXPR="
  let
    pkgs = import (builtins.fetchTree {
      type = \"github\";
      owner = \"NixOS\";
      repo = \"nixpkgs\";
      rev = \"$REV\";
    }) {};

    src = pkgs.fetchFromGitHub {
      owner = \"NixOS\";
      repo = \"nixpkgs\";
      rev = \"$REV\";
      sha256 = \"$HASH\";
    };
  in
    builtins.readFile \"\${src}/.version\"
"

repro() {
  nix eval --impure --raw \
    --option restrict-eval true \
    --option allowed-uris "github:NixOS/nixpkgs/$REV" \
    --expr "$EXPR"
}

SRC=$(nix eval --raw --impure \
  --expr "(builtins.fetchTree { type=\"github\"; owner=\"NixOS\"; repo=\"nixpkgs\"; rev=\"$REV\"; }).outPath")
nix store delete "$SRC" >/dev/null 2>&1

echo "== first run"
repro
echo

echo "== second run"
repro
echo
```

## Expected behavior

Both runs print `26.05`.

## Actual behaviour

```
== first run
error:
       … while calling the 'readFile' builtin
         at «string»:17:5:
           16|   in
           17|     builtins.readFile "${src}/.version"
             |     ^

       … while realising the context of path '/nix/store/idarbbj73yhf2a8j3jppm52v1pjmxmrj-source/.version'

       error: path '/nix/store/idarbbj73yhf2a8j3jppm52v1pjmxmrj-source' is not valid

== second run
26.05
```

Running the first invocation with `-v` shows the derivation being built and the store optimiser
hard-linking files *into* `/nix/store/idarbbj73...-source/` immediately before the path is reported
invalid, and the path is valid once the process exits.

## Additional context

Each row below is a single change from the script above:

| variant | result |
|---|---|
| as above | **fails** |
| `--pure-eval` instead of `--option restrict-eval true` | **fails** |
| neither, plain `--impure` | succeeds |
| `builtins.fetchTarball` instead of `builtins.fetchTree` | **fails** |
| `fetchFromGitHub` taken from `<nixpkgs>` rather than from the fetched tree | **fails** |
| fetched path's contents never read, only `seq` on its `outPath` | succeeds |
| fetched path read with `readFile` instead of `import` | **fails** |
| `fetchTree` on a different rev, so the two paths differ | succeeds |
| no `fetchTree`/`fetchTarball` at all, just the FOD | succeeds |
| same expression against a fresh `--store "$(mktemp -d)"` | succeeds |

So it needs, together:

1. restricted or pure eval mode,
2. an eval-time fetch whose **contents are accessed** — `import` and `readFile` both do it, forcing
   `outPath` alone does not,
3. a fixed-output derivation resolving to that **same** store path,
4. the path not already in the store.

Which fetcher provides `fetchFromGitHub` is irrelevant; `<nixpkgs>` works as well as the fetched
tree. The `--store` row is worth noting on its own: a brand new store, where every path is cold,
does not reproduce it, and it leaves the default store untouched, so it isn't picking the path up
from there. Path coldness alone isn't sufficient.

Since `--pure-eval` reproduces it too, this may share a root cause with #11683 and #11712, which
cover `EvalState::realiseContext` in pure eval mode. #12045 (merged) touched the same function.

## Metadata

```
nix (Nix) 2.35.2
system = x86_64-linux
store = auto
sandbox = true
experimental-features = ca-derivations fetch-tree flakes impure-derivations nix-command
```
