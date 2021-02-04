with import <nixpkgs> {};
with stdenv.lib;

rec {
  name = "bash";
  binaries = [(import ./shared.nix).manWithPages];

  kernel = jupyter-kernel.create {
    definitions = {
      bash = {
        displayName = "Bash";
        argv = [
          "${import ./kernel.nix}/bin/bash_kernel"
          "-f"
          "{connection_file}"
        ];
        language = "bash";
        logo32 = ./bash.png;
        logo64 = ./bash.png;
        metadata = {
          codedown = {
            priority = 10;
          };
        };
      };
    };
  };

  languageServer = pkgs.writeText "language_servers.yaml" (generators.toYAML {} [
    # Primary language server
    (callPackage ./language_server_bash/config.nix {}).config

    # Secondary language servers (for diagnostics, formatting, etc.)
    (callPackage ./language_server_shellcheck/config.nix {}).config
  ]);

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [{
    attrName = "bash";
    codeMirrorMode = "shell";
    extensionsToHighlight = ["sh" "bash"];
    extensionsToRun = ["sh" "bash"];
  }]);
}
