{pkgs, lib, callPackage, writeTextDir, symlinkJoin}:

rec {
  metadata = callPackage ./metadata.nix {};

  modeInfo = writeTextDir "lib/codedown/bash-modes.yaml" (lib.generators.toYAML {} [{
    attrName = "bash";
    codeMirrorMode = "shell";
    extensionsToHighlight = ["sh" "bash"];
    extensionsToRun = ["sh" "bash"];
  }]);

  build = {
    baseName
    , packages ? (_: [])
    , languageServers ? (_: [])
    , codeDownAttr ? "bash"
    , otherLanguageKeys ? []
  }:
    let
      base = pkgs.lib.findSingle (x: x.name == baseName) null "multiple" metadata.baseOptions;
      bash = base.bash;
    in symlinkJoin {
      name = "bash";
      paths = [
        (callPackage ./kernel.nix {})
        (callPackage ./man-with-pages.nix {})
        modeInfo
      ];
    };
}


  # pkgs.writeTextDir "language-servers.yaml" (lib/codedown/bash-lib.generators.toYAML {} [
  #   # Primary language server
  #   (callPackage ./language_server_bash/config.nix {}).config

  #   # Secondary language servers (for diagnostics, formatting, etc.)
  #   (callPackage ./language_server_shellcheck/config.nix {}).config
  # ])
