{ lib
, callPackage
, writeTextDir
, symlinkJoin
, cling
, clang
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    # "cpp98"
    "cpp11"
    "cpp14"
    "cpp17"
    "cpp20"
    "cpp23"
  ];

  icons = {
    # cpp98 = ./cpp11.png; # TODO
    cpp11 = ./cpp11.png;
    cpp14 = ./cpp14.png;
    cpp17 = ./cpp17.png;
    cpp20 = ./cpp2a.png; # TODO
    cpp23 = ./cpp2a.png; # TODO
  };

  stds = {
    # cpp98 = "c++98";
    cpp11 = "c++11";
    cpp14 = "c++14";
    cpp17 = "c++17";
    cpp20 = "c++20";
    cpp23 = "c++23";
  };

  settingsSchema = [];

  repls = icon: {
    cling = {
      display_name = "Cling " + cling.unwrapped.version;
      attr = "cling";
      args = ["${cling}/bin/cling"];
      icon = icon;
    };
  };

in

with lib;

if cling == null then {} else
  listToAttrs (map (x:
    let
      std = getAttr x stds;

      displayName = "C++ " + (lib.removePrefix "c++" std);

      meta = clang.meta // {
        baseName = x;
        inherit displayName;
        version = clang.version;
        icon = getAttr x icons;
        inherit settingsSchema;
      };

    in {
      name = x;
      value = rec {
        packageOptions = {};
        packageSearch = common.searcher packageOptions;

        build = args@{
          packages ? []
          , settings ? {}
          , attrs ? [x "cpp"]
          , extensions ? ["cpp" "hpp" "cxx" "hxx" "c" "h"]
          , metaOnly ? false
        }:
          let
            settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
          in symlinkJoin {
            name = x;
            paths = [
              ((callPackage ./kernel_xeus.nix {
                inherit attrs displayName extensions metaOnly std;
                attrName = x;
              }))
              (callPackage ./mode_info.nix { inherit attrs extensions; })
            ]
            ++ (if metaOnly then [] else [cling])
            ;

            passthru = {
              inherit meta packageOptions;
              args = args // { baseName = x; };
              repls = repls (getAttr x icons);
            };
          };

        inherit meta;
      };
    }
  ) baseCandidates)
