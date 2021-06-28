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
  languageServer = writeTextDir "lib/codedown/haskell-language-servers.yaml" (pkgs.lib.generators.toYAML {} [hls]);
  modeInfo = writeTextDir "lib/codedown/haskell-modes.yaml" (pkgs.lib.generators.toYAML {} [{
    attr_name = "haskell";
    code_mirror_mode = "haskell";
    extensions_to_highlight = ["hs"];
    extensions_to_run = ["hs"];
  }]);
  extraGitIgnoreLines = [".stack"];
}
