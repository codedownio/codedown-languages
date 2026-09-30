#!/usr/bin/env bash

set -eu -o pipefail

SCRIPTDIR=$(cd "${BASH_SOURCE[0]%/*}" && pwd)
cd "$SCRIPTDIR"

OUTPUT="$(nix build \
           --impure --no-link \
           --expr 'with (import ../../../../. {}).pkgsStable; callPackage ./generate-package-names.nix {}' \
           --json | jq -r '.[0].outputs.out')"

rm -f ./package-names.nix
cp "$OUTPUT" ./package-names.nix

chmod u+w ./package-names.nix
echo "" >> ./package-names.nix
