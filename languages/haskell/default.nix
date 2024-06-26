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

  hasHlsSupport = version: builtins.compareVersions version "9.0" >= 0;

  chooseLanguageServers = settings: snapshot: ghc: kernelName:
    []
    ++ lib.optionals (common.isTrue settings "lsp.haskell-language-server.enable" && hasHlsSupport ghc.version)
      [((callPackage ./language-server-hls {}) snapshot ghc kernelName (common.focusSettings "lsp.haskell-language-server." settings))]
  ;

  compilers = callPackage ./compilers.nix {
    ihaskell-source = fetchFromGitHub {
      owner = "codedownio";
      repo = "IHaskell";
      rev = "72e663bcc1af12fc136d19941cf21efdf7341379";
      sha256 = "WSXrx+/iAiGa8qIJc7Wt6VxL9adw5KFt6FfaiOH/mjg=";
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
    displayName = "Haskell";

    settingsSchema = callPackage ./settings_schema.nix { inherit version; };

    meta = {
      baseName = "haskell-" + compilerName;
      name = "haskell-" + compilerName;
      description = "An advanced, purely functional programming language (GHC ${version})";
      inherit version displayName settingsSchema;
      icon = ./haskell-logo-64x64.png;
    };

    packageOptions = snapshot;
    versions = {
      ghc = snapshot.ghc.version;
      haskell-language-server = snapshot.haskell-language-server.version;
    };

    # Grab the meta from the library component
    # Could also search over other components?
    packageSearch = common.searcher (mapAttrs (name: value:
      let meta = (attrByPath ["components" "library" "meta"] null value); in
      if meta == null then value else value // { inherit meta; }) packageOptions);

  in {
    name = meta.baseName;
    value = lib.makeOverridable ({
      packages ? []
      , attrs ? [meta.baseName "haskell"]
      , extensions ? ["hs"]
      , settings ? {}
    }@args:
      let
        settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
        ghc = snapshot.ghcWithPackages (ps:
          [ps.directory]
          ++ (map (x: builtins.getAttr x ps) packages)
          ++ (if (common.isTrue settingsToUse "lsp.haskell-language-server.enable") then [ps.haskell-language-server] else [])
        );
        languageServers = chooseLanguageServers settingsToUse snapshot ghc meta.baseName;

      in symlinkJoin {
        name = meta.baseName;

        paths = [
          (callPackage ./kernel.nix {
            inherit displayName attrs extensions snapshot;

            language = "haskell";

            ihaskell = if settingsToUse.enableHlintOutput then snapshot.ihaskell else snapshot.ihaskell.overrideAttrs (oldAttrs: {
              configureFlags = ["-f" "-use-hlint"];
            });
            inherit ghc;

            # enableVariableInspector = settingsToUse.enableVariableInspector;
          })

          ghc
        ]
        ++ languageServers
        ;

        passthru = {
          args = args // { baseName = meta.baseName; };
          inherit meta packageOptions packageSearch versions;
          inherit settingsSchema settings;
          repls = repls ghc;
          modes = {
            inherit attrs extensions;
            code_mirror_mode = "haskell";
          };
          languageServerNames = map (x: x.languageServerName) languageServers;
        };
      }
    ) {}
    ;
  }
) (lib.filterAttrs (k: _: !(hasPrefix "override") k) compilers))
