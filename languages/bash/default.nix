{ pkgs
, lib
, callPackage
, writeTextDir
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    "bashInteractive"
    "bashInteractive_5"
  ];

  modeInfo = writeTextDir "lib/codedown/bash-modes.yaml" (lib.generators.toYAML {} [{
    attr_name = "bash";
    code_mirror_mode = "shell";
    extensions_to_highlight = ["sh" "bash"];
    extensions_to_run = ["sh" "bash"];
  }]);

in

lib.listToAttrs (map (x:
  let
    bash = lib.getAttr x pkgs;
    meta = bash.meta // {
      baseName = x;
      displayName = "Bash " + bash.version;
      icon = ./bash.png;
    };
  in
    {
      name = x;
      value = rec {
        packageOptions = {};
        packageSearch = common.searcher packageOptions;

        languageServerOptions = {};
        languageServerSearch = common.searcher languageServerOptions;

        build = args@{
          packages ? []
          , languageServers ? []
          , codeDownAttr ? "bash"
          , otherLanguageKeys ? []
        }:
          symlinkJoin {
            name = "bash";

            paths = [
              (callPackage ./kernel.nix {})
              (callPackage ./man-with-pages.nix {})
              modeInfo
            ];

            passthru = {
              args = args // { baseName = x; };
              inherit meta packageOptions languageServerOptions;
            };
          };
      };

      inherit meta;
    }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))


  # pkgs.writeTextDir "language-servers.yaml" (lib/codedown/bash-lib.generators.toYAML {} [
  #   # Primary language server
  #   (callPackage ./language_server_bash/config.nix {}).config

  #   # Secondary language servers (for diagnostics, formatting, etc.)
  #   (callPackage ./language_server_shellcheck/config.nix {}).config
  # ])
