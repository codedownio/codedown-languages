{ lib
, callPackage
, writeTextDir
, symlinkJoin
, cling ? null
, clang
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    "cpp11"
    "cpp14"
    "cpp17"
    "cpp2a"
  ];

  icons = {
    cpp11 = ./cpp11.png;
    cpp14 = ./cpp14.png;
    cpp17 = ./cpp17.png;
    cpp2a = ./cpp2a.png;
  };

  displayNames = {
    cpp11 = "C++ 11";
    cpp14 = "C++ 14";
    cpp17 = "C++ 17";
    cpp2a = "C++ 2a";
  };

  stds = {
    cpp11 = "c++11";
    cpp14 = "c++14";
    cpp17 = "c++17";
    cpp2a = "c++2a";
  };

  repls = icon: {
    cling = {
      display_name = "Cling " + cling.unwrapped.version;
      args = ["${cling}/bin/cling"];
      icon = icon;
    };
  };

in

with lib;

if cling == null then {} else
  listToAttrs (map (x:
    let
      meta = clang.meta // {
        baseName = x;
        displayName = getAttr x displayNames;
        version = clang.version;
        icon = getAttr x icons;
      };

    in {
      name = x;
      value = rec {
        packageOptions = {};
        packageSearch = common.searcher packageOptions;

        languageServerOptions = {};
        languageServerSearch = common.searcher languageServerOptions;

        build = args@{
          packages ? []
          , languageServers ? []
          , attrs ? [baseName "cpp"]
          , extensions ? ["cpp" "hpp" "cxx" "hxx" "c" "h"]
          , metaOnly ? false
        }:
          symlinkJoin {
            name = x;
            paths = [
              cling
              ((callPackage ./kernel.nix { inherit attrs extensions; }) (getAttr x displayNames) (getAttr x stds) x (getAttr x icons)) # TODO: pass the other args normally
              (callPackage ./mode_info.nix { inherit attrs extensions; })
            ];
            passthru = {
              inherit meta packageOptions languageServerOptions;
              args = args // { baseName = x; };
              repls = repls (getAttr x icons);
            };
          };

        inherit meta;
      };
    }
  ) baseCandidates)
