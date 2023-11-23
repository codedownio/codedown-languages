#!/usr/bin/env bash


SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$SCRIPTDIR/.."

output=$(nix flake metadata --json)

for input in nixpkgs nixpkgs-unstable nixpkgs-master; do
    echo "Processing input: $input"
    rev=$(echo "$output" | jq -r ".locks.nodes[\"${input}\"].locked.rev")
    hash=$(echo "$output" | jq -r ".locks.nodes[\"${input}\"].locked.narHash")
    echo "Got ${input} rev: $rev"
    echo "Got ${input} hash: $hash"
    sed -i "s/rev = \".*\"; # ${input}-rev/rev = \"$rev\"; # ${input}-rev/g" ./default.nix
    sed -i "s/sha256 = \".*\"; # ${input}-sha256/sha256 = \"$hash\"; # ${input}-sha256/g" ./default.nix
done
