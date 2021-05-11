{ pkgs ? import <nixpkgs> {} }:

let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/ec898c02d17899282663544e451ef330cf7d1d1f.tar.gz) {};
  nixpkgs = import haskellNix.sources.nixpkgs haskellNix.nixpkgsArgs;
  haskell = nixpkgs.haskell-nix;

  # Needed on 17.10 due to https://github.com/haskell/stm/issues/45
  baseSnapshot = haskell.snapshots."lts-17.10";
  snapshot = baseSnapshot // {
    stm = baseSnapshot.stm.components.library.override { patches = [./stm.patch]; };
  };

in

nixpkgs.mkShell {
  buildInputs = [
    (snapshot.ghcWithPackages (ps: with ps; [stm lens conduit]))
  ];
}

# haskell.haskellPackages.ghcWithPackages (ps: with ps; [ lens conduit conduit-extra ])

# mkShell {
#   buildInputs = [
#     (haskell.snapshots."lts-17.9".ghcWithPackages (ps: with ps; [ lens conduit conduit-extra aeson ]
#     ))

#     # haskell.haskellPackages.ihaskell.components.exes.ihaskell
#     # haskell.snapshots."lts-17.9".ihaskell.components.exes.ihaskell
#   ];
# }
