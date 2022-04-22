#!/usr/bin/env bash

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $SCRIPTDIR

YAML_FILE=$(nix build .#ci --no-link --json | jq -r '.[0].outputs.out')

echo "Got yaml file: $YAML_FILE"

cp "$YAML_FILE" ./.github/workflows/ci.yml
