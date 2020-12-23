let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;

rec {
  name = "haskell";

  binaries = [(import ./stack.nix) haskell.packages.ghc883.ghc];

  kernel = jupyter-kernel.create {
    definitions = {
      haskell = {
        displayName = "Haskell";
        argv = [
          "${import ./ihaskell.nix}/bin/ihaskell"
          "kernel"
          "{connection_file}"
          "--stack"
          "+RTS" "-M3g" "-N2" "-RTS"
        ];
        language = "haskell";
        logo32 = null;
        logo64 = ./IHaskell/html/logo-64x64.svg;
      };
    };
  };

  languageServer = writeText "language_servers.yaml" (lib.generators.toYAML {} [{
    name = "haskell";
    extensions = ["hs"];
    attrs = ["haskell"];
    type = "stream";
    args = [
      "${haskellPackages.haskell-language-server}/bin/haskell-language-server-wrapper"
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
