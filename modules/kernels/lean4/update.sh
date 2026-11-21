#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix jq gnused git coreutils

# Pin Lean's nixpkgs to REV (default: the nixos-unstable head), then regenerate meta.json and
# repl-version.nix for the Lean it has.

set -eo pipefail

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPTDIR"

rev="${1:-$(git ls-remote https://github.com/NixOS/nixpkgs refs/heads/nixos-unstable | cut -f1)}"
echo "nixpkgs rev: $rev"

src=$(nix eval --json --impure --expr "let s = builtins.fetchTree { type = \"github\"; owner = \"NixOS\"; repo = \"nixpkgs\"; rev = \"$rev\"; }; in { inherit (s) narHash lastModified; path = s.outPath; }")
hash=$(jq -r .narHash <<< "$src")
date=$(date -u -d "@$(jq -r .lastModified <<< "$src")" +%Y-%m-%d)
path=$(jq -r .path <<< "$src")

nix eval --json --impure --expr "let pkgs = import $path { system = builtins.currentSystem; }; in (import ./meta.nix { inherit (pkgs) lib; }).fromPackage pkgs.leanPackages.lean4" \
  | jq . > meta.json.new
version=$(jq -r .version meta.json.new)
echo "Lean: $version"

replHash=$(nix store prefetch-file --json --unpack "https://github.com/leanprover-community/repl/archive/refs/tags/v$version.tar.gz" | jq -r .hash)
echo "repl v$version: $replHash"

mv meta.json.new meta.json
sed -i "s|rev = \".*\"; # nixos-unstable, .*|rev = \"$rev\"; # nixos-unstable, $date|" nixpkgs.nix
sed -i "s|hash = \".*\";|hash = \"$hash\";|" nixpkgs.nix
sed -i "s|tag = \".*\";|tag = \"v$version\";|; s|sha256 = \".*\";|sha256 = \"$replHash\";|" repl-version.nix
