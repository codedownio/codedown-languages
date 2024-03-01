{ pkgs
, lib
, callPackage
, writeTextDir
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  packageOptions = {};
  packageSearch = common.searcher packageOptions;

  settingsSchema = [];

  chooseLanguageServers = settings:
    []
    ;

  meta = {
    name = "postgres";
    baseName = "postgres";
    displayName = "PostgreSQL";
    description = "A simple Jupyter kernel for PostgreSQL";
    icon = ./postgres-logo-64x64.png;
    version = "0.1";
    inherit settingsSchema;
  };

in

{
  postgres = {
    inherit packageOptions packageSearch;
    versions = {
      postgres-kernel = meta.version;
    };

    build = args@{
      packages ? []
      , settings ? []
      , attrs ? ["postgres"]
      , extensions ? ["sql"]
    }:
      let
        settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
      in symlinkJoin {
        name = "postgres";

        paths = [
          (callPackage ./kernel.nix { inherit attrs extensions; })
        ]
        ++ (chooseLanguageServers settingsToUse)
        ;

        passthru = {
          args = args // { baseName = "postgres"; };
          inherit meta packageOptions;
          inherit settingsSchema settings;
          modes = {
            inherit attrs extensions;
            code_mirror_mode = "sql";
          };
        };
      };

    inherit meta;
  };
}
