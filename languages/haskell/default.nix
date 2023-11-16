{ lib
, callPackage
, runCommand
, fetchFromGitHub
, stdenv
, symlinkJoin
, makeWrapper

, haskell
, ltsOnly ? true
}:

with lib;

let
  common = callPackage ../common.nix {};

  hasHlsSupport = version: true;

  chooseLanguageServers = settings: snapshot: ghc: kernelName:
    []
    ++ lib.optionals (common.isTrue settings "lsp.haskell-language-server.enable" && hasHlsSupport ghc.version)
                     [((callPackage ./hls.nix {}) snapshot ghc kernelName (common.focusSettings "lsp.haskell-language-server." settings))]
  ;

  compilers = callPackage ./compilers.nix {
    ihaskell-source = fetchFromGitHub {
      owner = "codedownio";
      repo = "IHaskell";
      rev = "9db3044d7cfcac6acfb92633c0bea9e27fa31b42";
      sha256 = "12zp765aqf3ks0h84i3y2jx0gyamkya7wm9s8x1sa482729sv8mp";
    };
  };

  repls = ghc: {
    ghci = {
      display_name = "GHCi " + ghc.version;
      attr = "ghci";
      args = ["${ghc}/bin/ghci"];
      icon = ./haskell-logo-64x64.png;
    };
  };

in

listToAttrs (mapAttrsToList (compilerName: snapshot:
  let
    version = snapshot.ghc.version;
    displayName = "Haskell (GHC " + version + ")";

    settingsSchema = callPackage ./settings_schema.nix { inherit version; };

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
      languageServerOptions = lib.optionals (hasHlsSupport version) [
        snapshot.haskell-language-server
      ];

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
) (lib.filterAttrs (k: _: !(hasPrefix "override") k) compilers))
