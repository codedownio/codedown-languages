#!/usr/bin/env sh

BUILT=$(nix build --impure --expr 'with import <nixpkgs> {}; (callPackage ./evcxr/withPackages.nix {}).allPackageNames' --json | jq -r '.[0].outputs.out')

cp "$BUILT" ./all_package_names.nix
