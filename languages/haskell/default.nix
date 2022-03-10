{ lib
, callPackage
, writeText
, writeTextDir
, stdenv
, symlinkJoin
, fetchFromGitHub
, filterToValid ? false
, ltsOnly ? true
}:

let
  common = callPackage ../common.nix {};

  allLanguageServerOptions = snapshot: kernelName: {

  };

  # This must be chosen to match haskellNix.sources.nixpkgs!
  # We do it ourselves because we want to use fetchFromGitHub instead of fetchTarball.
  nixpkgsSrc = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "110a2c9ebbf5d4a94486854f18a37a938cfacbbb";
    sha256 = "0v12ylqxy1kl06dgln6h5k8vhlfzp8xvdymljj7bl0avr0nrgrcm";
  };

  haskellNix = import (fetchFromGitHub {
    owner = "codedownio";
    repo = "haskell.nix";
    rev = "d9f17eca28b3cb336aa33080773f8196f01f458a";
    sha256 = "sha256-pZZO5tYsGDZTWIF20PeNgP1XeWZoLUY+TyIbeOEo4mY=";
  }) { inherit nixpkgsSrc; };

  nixpkgs = import nixpkgsSrc haskellNix.nixpkgsArgs;

  haskell = nixpkgs.haskell-nix;

  # Filter to LTS only to speed up evaluation time
  baseSnapshots = (lib.filterAttrs (n: v: lib.hasInfix "lts" n) haskell.snapshots);

  snapshotToCompiler = lib.mapAttrs (name: value: ((lib.getAttr name haskell.stackage) haskell.hackage).compiler.nix-name) baseSnapshots;

  validSnapshots = if filterToValid then (lib.filterAttrs (n: v: lib.hasAttr (lib.getAttr n snapshotToCompiler) nixpkgs.haskell.packages) baseSnapshots) else baseSnapshots;

  repls = ghc: {
    ghci = {
      display_name = "GHCi " + ghc.version;
      args = ["${ghc}/bin/ghci"];
      icon = ./haskell-logo-64x64.png;
    };
  };

in

lib.listToAttrs (lib.mapAttrsToList (name: snapshot:
  let displayName = "Haskell (Stackage " + name + ")";
      meta = {
        baseName = "haskell-stackage-" + name;
        name = "haskell-stackage-" + name;
        description = "An advanced, purely functional programming language";
        inherit displayName;
        icon = ./haskell-logo-64x64.png;
      };

  in {
    name = meta.baseName;
    value = rec {
      packageOptions = snapshot;

      # Grab the meta from the library component
      # Could also search over other components?
      packageSearch = common.searcher (lib.mapAttrs (name: value:
        let meta = (lib.attrByPath ["components" "library" "meta"] null value); in
        if meta == null then value else value // { inherit meta; }) packageOptions);

      languageServerOptions = allLanguageServerOptions snapshot "haskell";
      languageServerSearch = common.searcher languageServerOptions;

      settingsSchema = [];
      defaultSettings = {};

      build = args@{
        packages ? []
        , languageServers ? []
        , attrs ? [meta.baseName "haskell"]
        , extensions ? ["hs"]
        , settings ? defaultSettings
        , metaOnly ? false
      }:
        let
          settingsToUse = defaultSettings // settings;
          ghc = snapshot.ghcWithPackages (ps: (map (x: builtins.getAttr x ps) packages));

        in symlinkJoin {
          name = meta.baseName;

          paths = [
            (callPackage ./kernel.nix {
              inherit displayName attrs extensions metaOnly snapshot;
              compiler = lib.getAttr (lib.getAttr name snapshotToCompiler) nixpkgs.haskell.packages;
              ghc = snapshot.ghcWithPackages (ps: (map (x: builtins.getAttr x ps) packages));
              packages = packages;
              # enableVariableInspector = settingsToUse.enableVariableInspector;
            })

            ghc

            (callPackage ./mode_info.nix { inherit attrs extensions; })
          ];

          passthru = {
            args = args // { baseName = meta.baseName; };
            settings = settingsToUse;
            inherit meta languageServerOptions packageOptions settingsSchema;
            repls = repls ghc;
          };
        };

      inherit meta;
    };
  }
) validSnapshots)


  # languageServer = writeTextDir "lib/codedown/python-language-servers.yaml" (pkgs.lib.generators.toYAML {} (map (x: x.config) (languageServers availableLanguageServers)));
  # extraGitIgnoreLines = [".ipython"];


  # rec {
  #   # ihaskellWithPackages = ihaskell.override {
  #   #   ghcWithPackages = haskell.haskellPackages.ghcWithPackages (ps: with ps;
  #   #     [ lens conduit conduit-extra aeson ]
  #   #   );
  #   # };

  #   hls = callPackage ./hls.nix {};

  #   languageServer = writeTextDir "lib/codedown/haskell-language-servers.yaml" (pkgs.lib.generators.toYAML {} [hls]);

  #   extraGitIgnoreLines = [".stack"];
  # }
