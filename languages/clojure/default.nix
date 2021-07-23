{ lib
, pkgs
, callPackage
, stdenv
, writeTextDir
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = [
    "clojure"
  ];

  packagesLookup = {
    clojure = {};
  };

  modeInfo = writeTextDir "lib/codedown/clojure-modes.yaml" (lib.generators.toYAML {} [{
    attr_name = "clojure";
    code_mirror_mode = "clojure";
    extensions_to_highlight = ["clj"];
    extensions_to_run = ["clj"];
  }]);

in

with lib;

listToAttrs (map (x:
  let
    clojure = getAttr x pkgs;

    meta = clojure.meta // {
      baseName = x;
      displayName = "Clojure";
      icon = ./logo-64x64.png;
    };

  in {
    name = x;
    value = rec {
      packageOptions = getAttr x packagesLookup;
      packageSearch = common.searcher packageOptions;

      languageServerOptions = {};
      languageServerSearch = common.searcher languageServerOptions;

      build = args@{
        packages ? []
        , languageServers ? []
        , codeDownAttr ? "clojure"
        , otherLanguageKeys ? []
      }:
        symlinkJoin {
          name = "clojure";
          paths = [
            clojure
            (callPackage ./kernel.nix {})
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

) (filter (x: (common.hasAttrSafe x pkgs) && !(attrByPath [x "meta" "broken"] false pkgs)) baseCandidates))


  # languageServer = writeTextDir "lib/codedown/clojure-language-servers.yaml" (lib.generators.toYAML {} [{
  #   name = "clojure";
  #   extensions = ["clj"];
  #   attrs = ["clojure"];
  #   type = "stream";
  #   args = ["${clojure-lsp}/bin/clojure-lsp"];
  #   initialization_options = {
  #     "src-paths" = ["/home/user" "/home/user/src" "/home/user/test"];
  #   };
  #   notebook_suffix = ".clj";
  # }]);
