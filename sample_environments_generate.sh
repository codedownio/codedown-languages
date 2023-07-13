#!/usr/bin/env bash


echo "args: {" > sample_environments.nix

for nixFile in $(ls sample_environments/*.nix); do
    echo "  $(basename $nixFile .nix) = import ./$nixFile args;" >> sample_environments.nix
done

echo "}" >> sample_environments.nix