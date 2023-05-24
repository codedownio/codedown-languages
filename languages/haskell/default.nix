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

  chooseLanguageServers = settings: snapshot: ghc: kernelName:
    []
    ++ lib.optionals (common.isTrue settings "lsp.haskell-language-server.enable") [((callPackage ./hls.nix {}) snapshot ghc kernelName (common.focusSettings "lsp.haskell-language-server." settings))]
    ;

  compilers = {
    # ghc865 = haskell.packages.ghc865Binary; # Fails with error: attribute 'exceptions_0_10_4' missing
    # ghc884 = haskell.packages.ghc884; # hlint HLS plugin not working

    ghc8107 = haskell.packages.ghc8107;
    ghc902 = haskell.packages.ghc902;

    # Don't update to ghc925 due to https://gitlab.haskell.org/ghc/ghc/-/issues/22425
    # Wait for GHC 9.2.6 which has the fix: https://www.haskell.org/ghc/blog/20230210-ghc-9.2.6-released.html
    ghc924 = haskell.packages.ghc924.override {
      overrides = self: super: {
        ghc-parser = self.callCabal2nix "ghc-parser" (builtins.fetchTarball {
          url = "https://hackage.haskell.org/package/ghc-parser-0.2.4.0/ghc-parser-0.2.4.0.tar.gz";
          sha256 = "13ih9b417nyglzs46z1n68apw9w3pvmp1sfcxpmr24s2nshl1r74";
        }) {};
      };
    };

    ghc943 = haskell.packages.ghc943.override {
      overrides = self: super: {
        ghc-parser = let
          ihaskell-source = (fetchFromGitHub {
            owner = "IHaskell";
            repo = "IHaskell";
            rev = "8afa4e22c5724da89fec85a599ee129ab5b4cb9a";
            sha256 = "0rkvqrpnsyp33x8mzh1v48vm96bpmza14nl6ah1sgjfbp86ihi8p";
          });

          ghc-parser-source = runCommand "ghc-parser-source" {} ''
            cp -r ${ihaskell-source}/ghc-parser $out
          '';

          in

          self.callCabal2nix "ghc-parser" ghc-parser-source {};
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
