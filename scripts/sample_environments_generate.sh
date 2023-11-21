#!/usr/bin/env bash

SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPTDIR/.."

echo "args: {" > sample_environments.nix

for nixFile in $(ls sample_environments/*.nix); do
    if [[ "$nixFile" == *"disabled"* ]]; then continue; fi
    echo "  $(basename $nixFile .nix) = import ./$nixFile args;" >> sample_environments.nix
done

echo "}" >> sample_environments.nix
