#!/usr/bin/env sh

set -e

nix build '.#sample_environments_farm'

nix-tree '.#sample_environments_farm'
