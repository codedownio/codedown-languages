#!/usr/bin/env bash

set -eo pipefail

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SCRIPTDIR

cp $(nix build --no-link .#haskellCompilers --json | jq -r '.[0].outputs.out') ./languages/haskell/compilers.nix
chmod 664 ./languages/haskell/compilers.nix
