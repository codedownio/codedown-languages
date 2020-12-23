#!/bin/bash

nix-shell -p bundler --run 'bundle lock --update'
nix-shell -p bundix --run 'bundix'
