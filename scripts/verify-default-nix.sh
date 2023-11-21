#!/usr/bin/env sh

set -e

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPTDIR"

FLAKE_NIXPKGS_REV=$(nix flake info --json | jq -r .locks.nodes.nixpkgs.locked.rev)
FLAKE_NIXPKGS_UNSTABLE_REV=$(nix flake info --json | jq -r '.locks.nodes["nixpkgs-unstable"].locked.rev')

echo "FLAKE_NIXPKGS_REV: $FLAKE_NIXPKGS_REV"
echo "FLAKE_NIXPKGS_UNSTABLE_REV: $FLAKE_NIXPKGS_UNSTABLE_REV"

DEFAULT_NIXPKGS_REV=$(cat default.nix | grep nixpkgs-rev | cut -d'"' -f2)
DEFAULT_NIXPKGS_UNSTABLE_REV=$(cat default.nix | grep nixpkgs-unstable-rev | cut -d'"' -f2)

echo "DEFAULT_NIXPKGS_REV: $DEFAULT_NIXPKGS_REV"
echo "DEFAULT_NIXPKGS_UNSTABLE_REV: $DEFAULT_NIXPKGS_UNSTABLE_REV"


if [ "$FLAKE_NIXPKGS_REV" != "$DEFAULT_NIXPKGS_REV" ]; then
    echo "nixpkgs rev differed between flake and default.nix"
    exit 1
fi

if [ "$FLAKE_NIXPKGS_UNSTABLE_REV" != "$DEFAULT_NIXPKGS_UNSTABLE_REV" ]; then
    echo "nixpkgs-unstable rev differed between flake and default.nix"
    exit 1
fi
