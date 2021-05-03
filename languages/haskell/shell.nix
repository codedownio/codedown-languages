{ pkgs ? import <nixpkgs> {} }:

let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  nixpkgs = import haskellNix.sources.nixpkgs haskellNix.nixpkgsArgs;
  haskell = nixpkgs.haskell-nix;

in
    
with pkgs;

# haskell.haskellPackages.ghcWithPackages (ps: with ps; [ lens conduit conduit-extra ])

mkShell {
  buildInputs = [
    (haskell.haskellPackages.ghcWithPackages (ps: with ps;
      [ lens conduit conduit-extra aeson ]
    ))

    # haskell.haskellPackages.ihaskell.components.exes.ihaskell
    haskell.snapshots."lts-17.10".ihaskell.components.exes.ihaskell
  ];
}
