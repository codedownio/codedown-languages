{ lib
, callPackage
, writeText
, writeTextDir
, stdenv
, symlinkJoin
, fetchFromGitHub

, haskell
, haskell-nix
, filterToValid ? false
, ltsOnly ? true
}:

with callPackage ./inputs.nix {};

with lib;

let
  common = callPackage ../common.nix {};

  allLanguageServerOptions = snapshot: kernelName: {
    haskell-language-server = callPackage ./language-server-hls/config.nix { inherit snapshot kernelName; };
  };

  snapshotToCompiler = mapAttrs (name: value: ((getAttr name haskell-nix.stackage) haskell-nix.hackage).compiler.nix-name) baseSnapshots;

  applyVersionToSnapshot = name: snapshot: let
    version = if hasPrefix "lts-" name then "b." + (removePrefix "lts-" name)
              else if hasPrefix "nightly-" name then "a." + (removePrefix "nightly-" name)
              else name; in snapshot // { inherit version; };

  baseSnapshots = haskell-nix.snapshots;

  validSnapshots = mapAttrs applyVersionToSnapshot
    (if filterToValid then (filterAttrs (n: v: hasAttr (getAttr n snapshotToCompiler) haskell.packages) baseSnapshots) else baseSnapshots);

  repls = ghc: {
    ghci = {
      display_name = "GHCi " + ghc.version;
      args = ["${ghc}/bin/ghci"];
      icon = ./haskell-logo-64x64.png;
    };
  };

in

listToAttrs (mapAttrsToList (name: snapshot:
  let displayName = "Haskell (Stackage " + name + ")";
      meta = {
        baseName = "haskell-stackage-" + name;
        name = "haskell-stackage-" + name;
        description = "An advanced, purely functional programming language";
        version = snapshot.version;
        inherit displayName;
        icon = ./haskell-logo-64x64.png;
      };

  in {
    name = meta.baseName;
    value = rec {
      packageOptions = snapshot;

      # Grab the meta from the library component
      # Could also search over other components?
      packageSearch = common.searcher (mapAttrs (name: value:
        let meta = (attrByPath ["components" "library" "meta"] null value); in
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
              ihaskell = callPackage ./ihaskell.nix {
                inherit packages;
                compiler = getAttr (getAttr name snapshotToCompiler) haskell.packages;
              };
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


  # languageServer = writeTextDir "lib/codedown/python-language-servers.yaml" (generators.toYAML {} (map (x: x.config) (languageServers availableLanguageServers)));
  # extraGitIgnoreLines = [".ipython"];


  # rec {
  #   # ihaskellWithPackages = ihaskell.override {
  #   #   ghcWithPackages = haskell.haskellPackages.ghcWithPackages (ps: with ps;
  #   #     [ lens conduit conduit-extra aeson ]
  #   #   );
  #   # };

  #   hls = callPackage ./hls.nix {};

  #   languageServer = writeTextDir "lib/codedown/haskell-language-servers.yaml" (generators.toYAML {} [hls]);

  #   extraGitIgnoreLines = [".stack"];
  # }
