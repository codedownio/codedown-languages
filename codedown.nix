{ lib
, callPackage
, symlinkJoin
, writeTextDir
, pkgs
, fetchFromGitHub
, requiredPackages ? [pkgs.nix-prefetch-git]
, system ? "x86_64-linux"
}:

with lib;

let
  common = callPackage ./languages/common.nix {};
  shellsCommon = callPackage ./shells/common.nix {};

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

  # Languages
  # First argument controls whether attributes get filtered to the valid ones.
  # This can be expensive to evaluate for languages like Haskell where there are tons of
  # Stackage snapshots and one nix file for each. So, we don't bother with that when evaluating
  # the languages attrset normally--only when building the languagesSearcher.
  languagesFn = filterToValid: zipAttrsWith (n: v: head v) [
    (callPackage ./languages/bash {})
    (callPackage ./languages/clojure {})
    (callPackage ./languages/cpp {})
    (callPackage ./languages/dot {})
    (callPackage ./languages/haskell (
      let
        # This must be chosen to match haskellNix.sources.nixpkgs!
        # We do it ourselves because we want to use fetchFromGitHub instead of fetchTarball.
        nixpkgsSrc = fetchFromGitHub {
          owner = "NixOS";
          repo = "nixpkgs";
          rev = "110a2c9ebbf5d4a94486854f18a37a938cfacbbb";
          sha256 = "0v12ylqxy1kl06dgln6h5k8vhlfzp8xvdymljj7bl0avr0nrgrcm";
        };

        haskellNix = import (fetchFromGitHub {
          owner = "input-output-hk";
          repo = "haskell.nix";
          rev = "5c49987f99a8b12961979e32fcefec163ce0ef52";
          sha256 = "sha256-Mb99mzuCM6Tme96s2Gqajy/hT2a1N1J4gDyVr4phXe0=";
        }) { pkgs = import nixpkgsSrc { inherit system; }; };

      in
        {
          inherit filterToValid;
          haskell-nix = (import nixpkgsSrc (haskellNix.nixpkgsArgs // { inherit system; })).haskell-nix;
          ghc-boot-packages = (import nixpkgsSrc (haskellNix.nixpkgsArgs // { inherit system;})).ghc-boot-packages;
        }))
    (callPackage ./languages/julia {})
    (callPackage ./languages/octave {})
    (callPackage ./languages/python {})
    (callPackage ./languages/r {})
    (callPackage ./languages/ruby {})
    (callPackage ./languages/rust {})
  ];
  languages = languagesFn false;

  languagesSearcher = common.searcher (languagesFn true);

  # Build tools
  mkCodeDownEnvironment = args@{
    channels
    , kernels ? []
    , otherPackages ? []
    , metaOnly ? false
    , ...
  }: let
    builtKernels = map (x: let kernel = (getAttr x.language languages).build (x.args // { inherit metaOnly; }); in
                           kernel.overrideAttrs (old: {
                             passthru = old.passthru // {
                               language = x.language;
                               channel = x.channel;
                             };
                           })) kernels;

    shellToReplInfo = shell: {
      name = shell.contents.name;
      display_name = shell.contents.displayName;
      args = ["${shell.contents}/lib/codedown/shell"];
      icon = shell.contents.icon;
    };

    shells = filter (x: lib.hasPrefix "shells." x.attr) otherPackages;

    exporters = filter (x: lib.hasPrefix "exporters." x.attr) otherPackages;
    exporterInfos = concatMap (exporter: exporter.contents.meta.exporterInfos) exporters;

    repls =
      map shellToReplInfo shells
      ++ concatMap (kernel: lib.mapAttrsToList (name: value: value // { inherit name; }) (if kernel.passthru ? "repls" then kernel.passthru.repls else {})) builtKernels
    ;

  in
    symlinkJoin {
      name = "codedown-environment";
      paths = builtKernels
              ++ [((callPackage ./spec_yaml.nix {}) (args //  { inherit shells exporters; kernels = builtKernels; }))]
              ++ (if metaOnly then [] else [(shellsCommon.wrapShells shells)])
              ++ (if metaOnly then [] else (map (x: x.contents) otherPackages))
              ++ (if metaOnly then [] else requiredPackages)
              ++ [(writeTextDir "lib/codedown/repls.yaml" (lib.generators.toYAML {} repls))]
              ++ [(writeTextDir "lib/codedown/exporters.yaml" (lib.generators.toYAML {} exporterInfos))]
      ;
    };
}
