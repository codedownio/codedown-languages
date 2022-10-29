{ pkgs
, lib
, callPackage
, writeTextDir
, symlinkJoin
, metaOnly ? false
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    "bashInteractive"
    "bashInteractive_5"
  ];

  packageOptions = {};
  packageSearch = common.searcher packageOptions;

  languageServerOptions = {
    bash-language-server = callPackage ./language_server_bash {};
    shellcheck = callPackage ./language_server_shellcheck {};
  };
  languageServerSearch = common.searcher languageServerOptions;

in

lib.listToAttrs (map (x:
  let
    bash = lib.getAttr x pkgs;
    meta = bash.meta // {
      baseName = x;
      displayName = "Bash " + bash.version;
      version = bash.version;
      icon = ./bash.png;
    };
  in
    {
      name = x;
      value = rec {
        inherit packageOptions packageSearch;
        inherit languageServerOptions languageServerSearch;

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
            ]
            ++ (if metaOnly then [] else [(callPackage ./man-with-pages.nix {})])
            ++ (if metaOnly then [] else (map (y: builtins.getAttr y languageServerOptions) languageServers))
            ;

            passthru = {
              args = args // { baseName = x; };
              inherit meta packageOptions languageServerOptions;
            };
          };

        inherit meta;
      };
    }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))
