#!/usr/bin/env bash

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SCRIPTDIR

mkdir -p gcroots

nix build .#sample_environments_farm $* -o gcroots/sample_environments_farm
nix build .#sample_environments_farm.inputDerivation $* -o gcroots/sample_environments_farm.input.drv
