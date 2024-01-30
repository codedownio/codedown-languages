{ callPackage
, gopls
, lib
, pkgs
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    "go"
    "go_1_17"
    "go_1_18"
    "go_1_19"
  ];

  settingsSchema = [
    {
      target = "lsp.gopls.enable";
      title = "Enable gopls language server";
      type = "boolean";
      defaultValue = true;
    }
  ];

  chooseLanguageServers = settings: go: attrs: kernelName:
  []
  ++ lib.optionals (common.isTrue settings "lsp.gopls.enable") [(callPackage ./language-server-gopls.nix { inherit go attrs; inherit kernelName; })]
  ;

  repls = go: {};

in

with lib;

listToAttrs (map (x:
  let
    go = getAttr x pkgs;

    meta = go.meta // {
      baseName = x;
      displayName = "Go";
      version = go.version;
      icon = ./go-logo-64x64.png;
      inherit settingsSchema;
    };

  in {
    name = x;
    value = rec {
      packageOptions = {};
      packageSearch = common.searcher packageOptions;
      versions = {
        go = go.version;
        gopls = gopls.version;
      };

      build = args@{
        packages ? []
        , settings ? {}
        , attrs ? ["go"]
        , extensions ? ["go"]
      }:
        let
          settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
        in symlinkJoin {
          name = "go";
          paths = [
            (callPackage ./kernel.nix {
              inherit attrs extensions;
              version = go.version;
            })
            go
          ]
          ++ (chooseLanguageServers settingsToUse go attrs x)
          ;

          passthru = {
            inherit meta packageOptions;
            args = args // { baseName = x; };
            repls = repls go;
            modes = {
              inherit attrs extensions;
              code_mirror_mode = "go";
            };
          };
        };

      inherit meta;
    };
  }

) (lib.filter (x: lib.hasAttr x pkgs) baseCandidates))
