{pkgs, callPackage, writeText, stdenv}:

rec {
  name = "haskell";

  binaries = [(import ./stack.nix) haskell.packages.ghc883.ghc];

  # ihaskellWithPackages = ihaskell.override {
  #   ghcWithPackages = haskell.haskellPackages.ghcWithPackages (ps: with ps;
  #     [ lens conduit conduit-extra aeson ]
  #   );
  # };

  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  nixpkgs = import pkgs haskellNix.nixpkgsArgs;
  haskell = nixpkgs.haskell-nix;
  # ihaskellWithPackages = ihaskell;

  hls = callPackage ./hls.nix {};

  kernel = callPackage ./kernel.nix {};
  languageServer = writeText "language_servers.yaml" (pkgs.lib.generators.toYAML {} [hls]);
  modeInfo = writeTextDir "lib/codedown/haskell-mode-config.yaml" (pkgs.lib.generators.toYAML {} [{
    attrName = "haskell";
    codeMirrorMode = "haskell";
    extensionsToHighlight = ["hs"];
    extensionsToRun = ["hs"];
  }]);
  extraGitIgnoreLines = [".stack"];
}
