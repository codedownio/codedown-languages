{ pkgs
, lib
, callPackage
, writeTextDir
, symlinkJoin

, packages
, attrs
, extensions
, settings
, settingsSchema
}:

let
  common = callPackage ../common.nix {};

  packageOptions = {};
  packageSearch = common.searcher packageOptions;

  languageServers = [];

  kernel = callPackage ./kernel.nix { inherit attrs extensions; };

  version = "0.1";

in

symlinkJoin {
  name = "postgres";

  paths = [
    kernel
  ]
  ++ languageServers
  ;

  passthru = {
    args = {
      inherit attrs extensions settings packages;
    };
    meta = {
      name = "postgres";
      baseName = "postgres";
      displayName = "PostgreSQL";
      description = "A simple Jupyter kernel for PostgreSQL";
      icon = ./postgres-logo-64x64.png;
      inherit version;
      inherit settingsSchema;
    };
    inherit packageOptions packageSearch;
    versions = {
      postgres-kernel = version;
    };
    inherit settingsSchema settings;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "sql";
    };
  };
}