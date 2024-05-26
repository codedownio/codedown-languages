{ pkgsStable
, pkgsUnstable
, pkgsMaster
, requiredPackages ? []
, system ? "x86_64-linux"
}:

let
  common = pkgsStable.callPackage ./languages/common.nix {};

  callPackage = pkgsStable.callPackage;

  # Languages
  # First argument controls whether attributes get filtered to the valid ones.
  # This can be expensive to evaluate for languages like Haskell where there are tons of
  # Stackage snapshots and one nix file for each. So, we don't bother with that when evaluating
  # the languages attrset normally--only when building the languagesSearcher.
  languagesFn = filterToValid: pkgsStable.lib.zipAttrsWith (n: v: pkgsStable.lib.head v) [
    (callPackage ./languages/bash {})
    (callPackage ./languages/clojure {})
    (callPackage ./languages/coq {})
    (pkgsMaster.callPackage ./languages/cpp {})
    (callPackage ./languages/go {})
    (pkgsMaster.callPackage ./languages/haskell {})
    (callPackage ./languages/julia {})
    (callPackage ./languages/octave {})
    (callPackage ./languages/postgres {})
    (callPackage ./languages/python {
      poetry2nix = import (pkgsStable.fetchFromGitHub {
        owner = "nix-community";
        repo = "poetry2nix";
        rev = "78fc8882411c29c8eb5f162b09fcafe08b8b03a3";
        sha256 = "1dfgm286c48ac6yrk16xz41d0rsg6bv08122ngy420b0z88la9nj";
      }) {
        pkgs = pkgsStable;
      };
    })
    (callPackage ./languages/r {})
    (callPackage ./languages/ruby {})
    (pkgsMaster.callPackage ./languages/rust {})
  ];

  lib = pkgsStable.lib;

in

rec {
  nixpkgsStableSearcher = common.searcher pkgsStable;

  spellchecker = pkgsUnstable.callPackage ./language_servers/markdown-spellcheck-lsp {};

  languages = languagesFn false;

  shells = {
    zsh = callPackage ./shells/zsh {};
    fish = callPackage ./shells/fish {};
    bash = callPackage ./shells/bash {};
  };

  exporters = {
    nbconvert-small = pkgsMaster.callPackage ./exporters/nbconvert.nix { size = "small"; };
    nbconvert-large = pkgsMaster.callPackage ./exporters/nbconvert.nix { size = "large"; };
  };

  # Exported so clients can build searchers for other package sets, like "codedown.searcher nixpkgs"
  searcher = common.searcher;

  codedownSearcher = common.searcher' {
    packages = languagesFn true
      // (lib.mapAttrs' (n: v: lib.nameValuePair ("shells." + n) v) shells)
      // (lib.mapAttrs' (n: v: lib.nameValuePair ("exporters." + n) v) exporters)
      // { inherit spellchecker; };
  };

  languagesIcons = common.searcherIcons' {
    packages = languagesFn true;
    packageMustBeDerivation = false;
  };

  settingsSchemas = lib.mapAttrs (attr: value:
    common.safeEval (lib.attrByPath ["meta" "settingsSchema"] [] value)
  ) languages;

  mkCodeDownEnvironment = callPackage ./codedown/mkCodeDownEnvironment.nix {
    inherit requiredPackages languages;
  };

  validateCodeDownEnvironment = callPackage ./codedown/validateCodeDownEnvironment.nix {};

  # Exposed so it's easier to compute build dependencies in the presence of IFD
  inherit pkgsStable pkgsUnstable requiredPackages;
}
