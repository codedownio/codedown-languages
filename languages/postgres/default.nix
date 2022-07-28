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
    baseName = "postgres";
    displayName = "PostgreSQL";
    version = "1.0.0";
  };

in

{
  postgres = {
    inherit packageOptions packageSearch;
    inherit languageServerOptions languageServerSearch;

    build = args@{
      packages ? []
      , languageServers ? []
      , attrs ? []
      , extensions ? []
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
