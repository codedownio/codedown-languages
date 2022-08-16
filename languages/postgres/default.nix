{ pkgs
, lib
, callPackage
, writeTextDir
, symlinkJoin
, metaOnly ? false
}:

let
  common = callPackage ../common.nix {};

  packageOptions = {};
  packageSearch = common.searcher packageOptions;

  languageServerOptions = {};
  languageServerSearch = common.searcher languageServerOptions;

  meta = {
    name = "postgres";
    baseName = "postgres";
    displayName = "PostgreSQL";
    description = "A simple Jupyter kernel for PostgreSQL";
    icon = ./logo-64x64.png;
    version = "0.1";
  };

in

{
  postgres = {
    inherit packageOptions packageSearch;
    inherit languageServerOptions languageServerSearch;

    build = args@{
      packages ? []
      , languageServers ? []
      , attrs ? ["postgres"]
      , extensions ? ["sql"]
      , metaOnly ? false
    }:
      symlinkJoin {
        name = "postgres";

        paths = [
          (callPackage ./kernel.nix { inherit attrs extensions metaOnly; })
          (callPackage ./mode_info.nix { inherit attrs extensions; })
        ]
        ++ (if metaOnly then [] else (map (y: builtins.getAttr y languageServerOptions) languageServers))
        ;

        passthru = {
          args = args // { baseName = "postgres"; };
          inherit meta packageOptions languageServerOptions;
        };
      };

    inherit meta;
  };
}
