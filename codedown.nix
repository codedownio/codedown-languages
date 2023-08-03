{ pkgsStable
, pkgsUnstable
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
    (pkgsUnstable.callPackage ./languages/cpp {})
    (callPackage ./languages/dot {})
    (callPackage ./languages/go {})
    (callPackage ./languages/haskell {})
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
    (pkgsUnstable.callPackage ./languages/rust {})
  ];

in

rec {
  searcher = common.searcher;

  nixpkgsStableSearcher = common.searcher pkgsStable;

  spellchecker = callPackage ./language_servers/markdown-spellcheck-lsp {};

  shells = {
    zsh = callPackage ./shells/zsh {};
    fish = callPackage ./shells/fish {};
    bash = callPackage ./shells/bash {};
  };
  availableShells = shells;
  shellsSearcher = common.searcher' {
    attrPrefix = "shells.";
    packages = shells;
  };

  exporters = {
    nbconvert-small = callPackage ./exporters/nbconvert.nix { size = "small"; };
    nbconvert-large = callPackage ./exporters/nbconvert.nix { size = "large"; };
  };
  availableExporters = exporters;
  exportersSearcher = common.searcher' {
    attrPrefix = "exporters.";
    packages = exporters;
  };

  languages = languagesFn false;
  languagesSearcher = common.searcher' {
    packages = languagesFn true;
    packageMustBeDerivation = false;
  };

  settingsSchemas = pkgsStable.lib.mapAttrs (attr: value:
    common.safeEval (pkgsStable.lib.attrByPath ["meta" "settingsSchema"] [] value)
  ) languages;

  mkCodeDownEnvironment = callPackage ./codedown/mkCodeDownEnvironment.nix {
    inherit requiredPackages languages;
  };

  validateCodeDownEnvironment = callPackage ./codedown/validateCodeDownEnvironment.nix {};

  # Exposed so it's easier to compute build dependencies in the presence of IFD
  inherit pkgsStable pkgsUnstable requiredPackages;
}
