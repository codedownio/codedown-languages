{pkgs, lib, callPackage, writeText}:

rec {
  metadata = callPackage ./metadata.nix {};

  build = {
    baseName,
    packages ? (_: []),
    languageServers ? (_: []),
    codeDownAttr ? "bash",
    otherLanguageKeys ? []
  }:
    let
      base = pkgs.lib.findSingle (x: x.name == baseName) null "multiple" metadata.baseOptions;
      bash = base.bash;
    in {
      name = "bash";
      binaries = [(callPackage ./man-with-pages.nix {})];
      kernel = callPackage ./kernel.nix {};
      languageServer = null;
      modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [{
        attrName = "bash";
        codeMirrorMode = "shell";
        extensionsToHighlight = ["sh" "bash"];
        extensionsToRun = ["sh" "bash"];
      }]);
    };
}


  # pkgs.writeText "language_servers.yaml" (lib.generators.toYAML {} [
  #   # Primary language server
  #   (callPackage ./language_server_bash/config.nix {}).config

  #   # Secondary language servers (for diagnostics, formatting, etc.)
  #   (callPackage ./language_server_shellcheck/config.nix {}).config
  # ])
