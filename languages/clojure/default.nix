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

  repls = clojure: {
    clojure = {
      display_name = "Clojure " + clojure.version;
      args = ["${clojure}/bin/clojure"];
      icon = ./logo-64x64.png;
    };
  };

in

with lib;

listToAttrs (map (x:
  let
    clojure = getAttr x pkgs;

    meta = clojure.meta // {
      baseName = x;
      displayName = "Clojure";
      version = clojure.version;
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
        , attrs ? ["clojure"]
        , extensions ? ["clj"]
        , metaOnly ? false
      }:
        symlinkJoin {
          name = "clojure";
          paths = [
            clojure
            (callPackage ./kernel.nix { inherit attrs extensions; })
            (callPackage ./mode_info.nix { inherit attrs extensions; })
          ];
          passthru = {
            inherit meta packageOptions languageServerOptions;
            args = args // { baseName = x; };
            repls = repls clojure;
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
