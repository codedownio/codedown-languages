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
          , attrs ? ["bash"]
          , extensions ? ["sh" "bash"]
          , metaOnly ? false
        }:
          symlinkJoin {
            name = "bash";

            paths = [
              (callPackage ./kernel.nix { inherit attrs extensions metaOnly; })
              (callPackage ./mode_info.nix { inherit attrs extensions; })
            ] ++ (if metaOnly then [] else [
              (callPackage ./man-with-pages.nix {})
            ]);

            passthru = {
              args = args // { baseName = x; };
              inherit meta packageOptions languageServerOptions;
            };
          };

        inherit meta;
      };
    }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))


  # pkgs.writeTextDir "language-servers.yaml" (lib/codedown/bash-lib.generators.toYAML {} [
  #   # Primary language server
  #   (callPackage ./language_server_bash/config.nix {}).config

  #   # Secondary language servers (for diagnostics, formatting, etc.)
  #   (callPackage ./language_server_shellcheck/config.nix {}).config
  # ])
