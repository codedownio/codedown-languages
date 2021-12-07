{ }:

let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/d231ee71dc511806ff75a6d83c7481fa25bbf8fe.tar.gz) {};
  nixpkgs = import haskellNix.sources.nixpkgs haskellNix.nixpkgsArgs;
  haskell = nixpkgs.haskell-nix;

  # Needed on 17.10 due to https://github.com/haskell/stm/issues/45
  snapshot = haskell.snapshots."lts-18.18";
  # snapshot = baseSnapshot // {
  #   stm = baseSnapshot.stm.components.library.override { patches = [./stm.patch]; };
  # };

in

haskell

# nixpkgs.mkShell {
#   buildInputs = [
#     (snapshot.ghcWithPackages (ps: with ps; [stm lens conduit]))
#   ];
# }

# haskell.haskellPackages.ghcWithPackages (ps: with ps; [ lens conduit conduit-extra ])

# mkShell {
#   buildInputs = [
#     (haskell.snapshots."lts-17.9".ghcWithPackages (ps: with ps; [ lens conduit conduit-extra aeson ]
#     ))

#     # haskell.haskellPackages.ihaskell.components.exes.ihaskell
#     # haskell.snapshots."lts-17.9".ihaskell.components.exes.ihaskell
#   ];
# }
