#! /usr/bin/env nix-shell
#! nix-shell -i bash -p node2nix

node2nix -14 -i node-packages.json --include-peer-dependencies
