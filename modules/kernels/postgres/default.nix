{ callPackage
, symlinkJoin

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

let
  common = callPackage ../common.nix {};

  packageOptions = {};
  packageSearch = common.searcher packageOptions;

  languageServers = [];

  displayName = "PostgreSQL";

  kernel = callPackage ./kernel.nix { inherit displayName attrs extensions; };

  version = "0.2.2";

in

symlinkJoin {
  name = "postgres";

  paths = [
    kernel
  ]
  ++ languageServers
  ;

  passthru = {
    meta = {
      name = "postgres";
      baseName = "postgres";
      inherit displayName;
      description = "A simple Jupyter kernel for PostgreSQL";
      icon = ./postgres-logo-64x64.png;
      iconMonochrome = ./postgresql-monochrome.svg;
      inherit version;
      inherit settingsSchema;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch;
    versions = {
      postgres-kernel = version;
    };
    inherit settingsSchema settings;
    inherit (kernel) repls;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "sql";
    };
  };
}
