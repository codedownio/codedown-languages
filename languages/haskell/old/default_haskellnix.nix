{ lib
, callPackage
, stdenv
, symlinkJoin
, makeWrapper
, pkgs

, haskell
, haskell-language-server
, haskell-nix
, ghc-boot-packages
, filterToValid ? false
, ltsOnly ? true
}:

with lib;

let
  common = callPackage ../common.nix {};
  util = callPackage ./util.nix {};

  allLanguageServerOptions = ghc: kernelName: {
    haskell-language-server = callPackage ./language-server-hls/config.nix {
      inherit kernelName;
      haskell-language-server = stdenv.mkDerivation {
        pname = "haskell-language-server-wrapped";
        version = haskell-language-server.version;

        buildInputs = [makeWrapper];

        dontUnpack = true;
        dontConfigure = true;
        buildPhase = ''
          mkdir -p $out/bin
          makeWrapper ${haskell-language-server}/bin/haskell-language-server $out/bin/haskell-language-server \
                      --suffix PATH ':' ${ghc}/bin
        '';
        dontInstall = true;

        inherit (haskell-language-server) meta;
      };
    };
  };

  compilers = filterAttrs (n: v: hasAttr n ghc-boot-packages) (import ./compilers.nix);

  repls = ghc: {
    ghci = {
      display_name = "GHCi " + ghc.version;
      attr = "ghci";
      args = ["${ghc}/bin/ghci"];
      icon = ./haskell-logo-64x64.png;
    };
  };

in

listToAttrs (mapAttrsToList (compilerName: snapshotName:
  let
    snapshot = util.applyVersionToSnapshot snapshotName (getAttr snapshotName haskell-nix.snapshots);

    displayName = "Haskell";

    meta = {
      baseName = "haskell-" + compilerName;
      name = "haskell-" + compilerName;
      description = "An advanced, purely functional programming language (Stackage ${snapshotName})";
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

      settingsSchema = [];
      defaultSettings = {};

      build = args@{
        packages ? []
        , attrs ? [meta.baseName "haskell"]
        , extensions ? ["hs"]
        , settings ? {}
      }:
        let
          settingsToUse = defaultSettings // settings;
          ghc = snapshot.ghcWithPackages (ps: [ps.directory] ++ (map (x: builtins.getAttr x ps) packages));

        in symlinkJoin {
          name = meta.baseName;

          paths = [
            (callPackage ./kernel.nix {
              inherit displayName attrs extensions snapshot;

              ihaskell = snapshot.ihaskell.components.exes.ihaskell;
              inherit ghc;

              # enableVariableInspector = settingsToUse.enableVariableInspector;
            })
            ghc
          ]
          ++ (map (y: builtins.getAttr y (allLanguageServerOptions ghc meta.baseName)) languageServers)
          ;

          passthru = {
            args = args // { baseName = meta.baseName; };
            settings = settingsToUse;
            inherit meta languageServerOptions packageOptions settingsSchema;
            repls = repls ghc;
            modes = {
              # TODO
            };
          };
        };

      inherit meta;
    };
  }
) compilers)
