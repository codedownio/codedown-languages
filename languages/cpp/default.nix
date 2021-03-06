{ lib
, callPackage
, writeTextDir
, symlinkJoin
, cling ? null
, clang
}:

let
  common = callPackage ../common.nix {};

  modeInfoBase = {
    attr_name = "cpp";
    code_mirror_mode = "clike";
    codeMirrorMimeType = "text/x-c++src";
    extensions_to_highlight = ["cpp" "hpp" "cxx" "hxx" "c" "h"];
    extensions_to_run = ["cpp" "cxx" "c"];
  };

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

in

with lib;

if cling == null then {} else
  listToAttrs (map (x:
    let
      meta = clang.meta // {
        baseName = x;
        displayName = getAttr x displayNames;
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
          , codeDownAttr ? "cpp"
          , otherLanguageKeys ? []
        }:
          let
            modeInfo = writeTextDir "lib/codedown/cpp-modes.yaml" (generators.toYAML {} [
              modeInfoBase
              (modeInfoBase // { attr_name = x; })
            ]);

          in symlinkJoin {
            name = x;
            paths = [
              cling
              ((callPackage ./kernel.nix {}) (getAttr x displayNames) (getAttr x stds) x (getAttr x icons))
              modeInfo
            ];
            passthru = {
              args = args // { baseName = x; };
              inherit meta packageOptions languageServerOptions;
            };
          };

        inherit meta;
      };
    }
  ) baseCandidates)
