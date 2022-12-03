{ lib
, callPackage
, symlinkJoin
, writeTextDir
, pkgs
, pkgsUnstable
, fetchFromGitHub
, requiredPackages ? [pkgs.nix-prefetch-git]
, system ? "x86_64-linux"
}:

with lib;

let
  common = callPackage ./languages/common.nix {};

  # Languages
  # First argument controls whether attributes get filtered to the valid ones.
  # This can be expensive to evaluate for languages like Haskell where there are tons of
  # Stackage snapshots and one nix file for each. So, we don't bother with that when evaluating
  # the languages attrset normally--only when building the languagesSearcher.
  languagesFn = filterToValid: zipAttrsWith (n: v: head v) [
    (callPackage ./languages/bash {})
    (callPackage ./languages/clojure {})
    (callPackage ./languages/coq {})
    (callPackage ./languages/cpp {})
    (callPackage ./languages/dot {})
    (callPackage ./languages/go {})
    (pkgsUnstable.callPackage ./languages/haskell {})
    (callPackage ./languages/julia {})
    (callPackage ./languages/octave {})
    (callPackage ./languages/postgres {})
    (pkgsUnstable.callPackage ./languages/python {})
    (callPackage ./languages/r {})
    (callPackage ./languages/ruby {})
    (pkgsUnstable.callPackage ./languages/rust {})
  ];

in

rec {
  searcher = common.searcher;

  nixpkgsSearcher = common.searcher pkgs;

  spellchecker = callPackage ./language_servers/markdown-spellcheck-lsp.nix {};

  shells = {
    zsh = callPackage ./shells/zsh {};
    fish = callPackage ./shells/fish {};
    bash = callPackage ./shells/bash {};
  };
  availableShells = shells;
  shellsSearcher = common.searcher' "shells." shells;

  exporters = {
    nbconvert-small = callPackage ./exporters/nbconvert.nix { size = "small"; };
    nbconvert-large = callPackage ./exporters/nbconvert.nix { size = "large"; };
  };
  availableExporters = exporters;
  exportersSearcher = common.searcher' "exporters." exporters;

  languages = languagesFn false;
  languagesSearcher = common.searcher (languagesFn true);

  mkCodeDownEnvironment = callPackage ./codedown/mkCodeDownEnvironment.nix {
    inherit requiredPackages languages;
  };
}
