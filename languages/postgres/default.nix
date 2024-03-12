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

  versions = {
    postgres-kernel = meta.version;
  };

in

{
  postgres = lib.makeOverridable ({
    packages ? []
    , settings ? []
    , attrs ? ["postgres"]
    , extensions ? ["sql"]
  }@args:
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
        inherit meta packageOptions packageSearch versions;
        inherit settingsSchema settings;
        modes = {
          inherit attrs extensions;
          code_mirror_mode = "sql";
        };
      };
    }
  ) {};
}
