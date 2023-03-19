{ lib
, callPackage
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
      target = "enableHlintOutput";
      title = "Enable hlint warnings in code output.";
      description = "Show hlint warnings as part of Jupyter kernel output. Normally you don't want this because it is provided by haskell-language-server.";
      type = "boolean";
      defaultValue = false;
    }

    # haskell-language-server.
    {
      target = "haskell-language-server.debug";
      title = "Haskell-language-server: enable debug output";
      description = "Print verbose debug output.";
      type = "boolean";
      defaultValue = false;
    }
  ];
  defaultSettings = {
    enableHlintOutput = false;
    "haskell-language-server.debug" = false;
  };

  hls = snapshot: ghc: kernelName: focusedSettings: callPackage ./language-server-hls/config.nix {
    inherit kernelName;

    ghc = snapshot;

    settings = focusedSettings;

    haskell-language-server = stdenv.mkDerivation {
      pname = "haskell-language-server-wrapped";
      version = snapshot.haskell-language-server.version;

      buildInputs = [makeWrapper];

      dontUnpack = true;
      dontConfigure = true;
      buildPhase = ''
        mkdir -p $out/bin
        makeWrapper ${ghc}/bin/haskell-language-server $out/bin/haskell-language-server \
                    --set NIX_GHC_LIBDIR "${ghc.out}/lib/${ghc.meta.name}" \
                    --prefix PATH ':' ${ghc}/bin
      '';
      dontInstall = true;

      inherit (snapshot.haskell-language-server) meta;
    };
  };

  allLanguageServerOptions = snapshot: ghc: kernelName: focusedSettings: {
    haskell-language-server = hls snapshot ghc kernelName focusedSettings;
  };

  compilers = {
    # ghc865 = haskell.packages.ghc865Binary; # Fails with error: attribute 'exceptions_0_10_4' missing
    # ghc884 = haskell.packages.ghc884; # hlint HLS plugin not working

    ghc8107 = haskell.packages.ghc8107;
    ghc902 = haskell.packages.ghc902;

    ghc924 = haskell.packages.ghc924.override {
      overrides = self: super: {
        ghc-parser = self.callCabal2nix "ghc-parser" (builtins.fetchTarball {
          url = "https://hackage.haskell.org/package/ghc-parser-0.2.4.0/ghc-parser-0.2.4.0.tar.gz";
          sha256 = "13ih9b417nyglzs46z1n68apw9w3pvmp1sfcxpmr24s2nshl1r74";
        }) {};
      };
    };

    # ghc942 = haskell.packages.ghc942; # base-compat not building
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
      inherit version displayName;
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

      languageServerOptions = allLanguageServerOptions snapshot (snapshot.ghcWithPackages (ps: [])) "haskell";
      languageServerSearch = common.searcher languageServerOptions;

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
          ghc = snapshot.ghcWithPackages (ps:
            [ps.directory]
            ++ (map (x: builtins.getAttr x ps) packages)
            ++ (if any (x: x == "haskell-language-server") languageServers then [ps.haskell-language-server] else [])
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
          ++ (if metaOnly then [] else (map (y: builtins.getAttr y (allLanguageServerOptions snapshot ghc meta.baseName (common.focusSettings "haskell-language-server." settingsToUse))) languageServers))
          ;

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
) compilers)
