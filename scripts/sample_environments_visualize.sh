#!/usr/bin/env sh

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPTDIR/.."

set -e

nix build '.#sample_environments_farm'

nix-tree '.#sample_environments_farm'
