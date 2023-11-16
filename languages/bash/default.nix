{ callPackage
, lib
, pkgs
, nodePackages
, symlinkJoin
, writeTextDir

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

  chooseLanguageServers = settings: kernelName:
    []
    ++ lib.optionals (common.isTrue settings "lsp.bash-language-server.enable") [(callPackage ./language_server_bash { inherit kernelName; })]
    ;

  settingsSchema = [
    {
      target = "lsp.bash-language-server.enable";
      title = "Enable Bash language server";
      type = "boolean";
      defaultValue = true;
    }
  ];

in

lib.listToAttrs (map (x:
  let
    bash = lib.getAttr x pkgs;
    meta = bash.meta // {
      baseName = x;
      displayName = "Bash " + bash.version;
      version = bash.version;
      icon = ./bash.png;
      inherit settingsSchema;
    };
  in
    {
      name = x;
      value = rec {
        inherit packageOptions packageSearch;
        languageServerOptions = [
          nodePackages.bash-language-server
        ];

        build = args@{
          packages ? []
          , attrs ? ["bash"]
          , extensions ? ["sh" "bash"]
          , metaOnly ? false
          , settings ? {}
        }:
          let
            settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
          in symlinkJoin {
            name = "bash";

            paths = [
              (callPackage ./kernel.nix { inherit attrs extensions metaOnly; })
              (callPackage ./mode_info.nix { inherit attrs extensions; })
            ]
            ++ (if metaOnly then [] else [(callPackage ./man-with-pages.nix {})])
            ++ (if metaOnly then [] else chooseLanguageServers settingsToUse x)
            ;

            passthru = {
              args = args // { baseName = x; };
              inherit meta packageOptions;
            };
          };

        inherit meta;
      };
    }
) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))
