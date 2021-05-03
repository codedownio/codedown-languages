{pkgs}:

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
  ihaskellWithPackages = ihaskell;
  haskell = nixpkgs.haskell-nix;

  kernel = jupyter-kernel.create {
    definitions = {
      haskell = {
        displayName = "Haskell";
        argv = [
          "${haskell.haskellPackages.ihaskell.components.exes.ihaskell}/bin/ihaskell"
          "kernel"
          "{connection_file}"
          "--stack"
          "+RTS" "-M3g" "-N2" "-RTS"
        ];
        language = "haskell";
        logo32 = null;
        logo64 = ./IHaskell/html/logo-64x64.svg;
        metadata = {
          codedown = {
            priority = 1;
          };
        };
      };
    };
  };

  languageServer = writeText "language_servers.yaml" (lib.generators.toYAML {} [{
    name = "haskell";
    extensions = ["hs"];
    attrs = ["haskell"];
    type = "stream";
    args = [
      "${haskell.haskellPackages.haskell-language-server}/bin/haskell-language-server-wrapper"
      "--lsp"
      "-l"
      "/tmp/hls.log"
    ];
    notebook_suffix = ".hs";
  }]);

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [{
    attrName = "haskell";
    codeMirrorMode = "haskell";
    extensionsToHighlight = ["hs"];
    extensionsToRun = ["hs"];
  }]);

  homeFolderPaths = (import ../../util.nix).folderBuilder ./home_folder;

  extraGitIgnoreLines = [
    ".stack"
  ];
}
