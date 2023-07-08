{ lib
, callPackage
, runCommand
, fetchFromGitHub
, stdenv
, symlinkJoin
, makeWrapper
, pkgs

, haskell
, ltsOnly ? true
}:

with lib;

let
  common = callPackage ../common.nix {};
  util = callPackage ./util.nix {};

  settingsSchema = [
    {
      target = "lsp.haskell-language-server.enable";
      title = "Enable haskell-language-server";
      type = "boolean";
      defaultValue = true;
    }
    {
      target = "lsp.haskell-language-server.debug";
      title = "Haskell-language-server: enable debug output";
      description = "Print verbose debug output.";
      type = "boolean";
      defaultValue = false;
    }

    {
      target = "enableHlintOutput";
      title = "Enable hlint warnings in code output.";
      description = "Show hlint warnings as part of Jupyter kernel output. Normally you don't want this because it is provided by haskell-language-server.";
      type = "boolean";
      defaultValue = false;
    }
  ];

  ihaskell-source = fetchFromGitHub {
    owner = "IHaskell";
    repo = "IHaskell";
    rev = "c547ee2fdc0a09cf4129b19292147fec38527a55";
    sha256 = "lpUSdhZ2HtUMcUygG5ORbib9Dc9SE2e81fQHO0UWNCo=";
  };

  chooseLanguageServers = settings: snapshot: ghc: kernelName:
    []
    ++ lib.optionals (common.isTrue settings "lsp.haskell-language-server.enable") [((callPackage ./hls.nix {}) snapshot ghc kernelName (common.focusSettings "lsp.haskell-language-server." settings))]
    ;

  compilers = {
    ghc810 = haskell.packages.ghc810;
    ghc90 = haskell.packages.ghc90;
    ghc92 = haskell.packages.ghc92;

    ghc94 = haskell.packages.ghc94.override {
      overrides = self: super: {
        ghc-parser = let
          ghc-parser-source = runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out";
        in
          self.callCabal2nix "ghc-parser" ghc-parser-source {};

        ihaskell = super.ihaskell.overrideAttrs (oldAttrs: {
          src = ihaskell-source;
        });

        zeromq4-haskell = super.zeromq4-haskell.overrideAttrs (oldAttrs: {
          buildInputs = oldAttrs.buildInputs ++ [pkgs.libsodium];
        });
      };
    };

    ghc96 = haskell.packages.ghc96.override {
      overrides = self: super: {
        ghc-parser = let
          ghc-parser-source = runCommand "ghc-parser-source" {} "cp -r ${ihaskell-source}/ghc-parser $out";
        in
          self.callCabal2nix "ghc-parser" ghc-parser-source {};

        here = super.here.overrideAttrs (oldAttrs: {
          src = fetchFromGitHub {
            owner = "tmhedberg";
            repo = "here";
            rev = "2530d70b44b23dc6f3dfbc762a8199e70b952e1c";
            sha256 = "q6oneTExLJw6P7iwwkHJCAN/MS69B0uw4r97fA49Jcw=";
          };
          doCheck = false;
          buildInputs = oldAttrs.buildInputs ++ [super.hspec];
        });

        hlint = super.shelly.overrideAttrs (oldAttrs: {
          src = fetchFromGitHub {
            owner = "ndmitchell";
            repo = "hlint";
            rev = "ed1259a7da88420e8d05d6241d6bdd4493a9997f";
            sha256 = "kLqO2Hbm2ekNzNgT54oZfF8EleqvvxoFlWjJTiteBzI=";
          };
        });

        ihaskell = super.ihaskell.overrideAttrs (oldAttrs: {
          src = ihaskell-source;
        });

        shelly = super.shelly.overrideAttrs (oldAttrs: {
          src = fetchFromGitHub {
            owner = "gregwebs";
            repo = "Shelly.hs";
            rev = "db62da933cba5da2a6aed34f049685fc72cb8440";
            sha256 = "VOYIH9hzAL98x1nmFHWsKUSapq/UEj1ZhjSqk0JECPg=";
          };
        });

        zeromq4-haskell = super.zeromq4-haskell.overrideAttrs (oldAttrs: {
          buildInputs = oldAttrs.buildInputs ++ [pkgs.libsodium];
        });
      };
    };
  };

  repls = ghc: {
    ghci = {
      display_name = "GHCi " + ghc.version;
      args = ["${ghc}/bin/ghci"];
      icon = ./haskell-logo-64x64.png;
    };
  };

in

listToAttrs (mapAttrsToList (compilerName: snapshot:
  let
    version = snapshot.ghc.version;
    displayName = "Haskell (GHC " + version + ")";

    meta = {
      baseName = "haskell-" + compilerName;
      name = "haskell-" + compilerName;
      description = "An advanced, purely functional programming language (GHC ${version})";
      inherit version displayName settingsSchema;
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

      build = args@{
        packages ? []
        , attrs ? [meta.baseName "haskell"]
        , extensions ? ["hs"]
        , settings ? {}
        , metaOnly ? false
      }:
        let
          settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
          ghc = snapshot.ghcWithPackages (ps:
            [ps.directory]
            ++ (map (x: builtins.getAttr x ps) packages)
            ++ (if (common.isTrue settingsToUse "lsp.haskell-language-server.enable") then [ps.haskell-language-server] else [])
          );

        in symlinkJoin {
          name = meta.baseName;

          paths = [
            (callPackage ./kernel.nix {
              inherit displayName attrs extensions metaOnly snapshot;

              language = "haskell";

              ihaskell = if settingsToUse.enableHlintOutput then snapshot.ihaskell else snapshot.ihaskell.overrideAttrs (oldAttrs: {
                configureFlags = ["-f" "-use-hlint"];
              });
              inherit ghc;

              # enableVariableInspector = settingsToUse.enableVariableInspector;
            })

            (callPackage ./mode_info.nix { inherit attrs extensions; })
          ]
          ++ (if metaOnly then [] else [ghc])
          ++ (if metaOnly then [] else chooseLanguageServers settingsToUse snapshot ghc meta.baseName)
          ;

          passthru = {
            args = args // { baseName = meta.baseName; };
            settings = settingsToUse;
            inherit meta packageOptions settingsSchema;
            repls = repls ghc;
          };
        };

      inherit meta;
    };
  }
) compilers)
