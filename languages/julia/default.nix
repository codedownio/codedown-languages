{ lib
, julia_16-bin
, julia_18
, python3
, callPackage
, fetchFromGitHub
, writeTextDir
, stdenv
, runCommand
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  juliaWithPackages = callPackage ./julia-modules {};

  settingsSchema = [
    # LanguageServer.
    {
      target = "LanguageServer.index";
      title = "LanguageServer: auto-index packages when building environment";
      description = "Automatically build SymbolServer.jl indices when realizing environment (may increase build time).";
      type = "boolean";
      defaultValue = true;
    }
  ];

  baseCandidates = rec {
    julia = julia18;

    julia16 = juliaWithPackages.override { julia = julia_16-bin; };

    julia18 = juliaWithPackages.override { julia = julia_18; };
  };

  packageSet = lib.listToAttrs (map (x: {
    name = x;
    value = {
      meta = {
        name = x;
        displayName = x;
      };
    };
  }) (import ./julia-modules/package-names.nix));

in

with lib;

mapAttrs (attr: value:
  let
    baseJulia = (value []).julia;

    displayName = "Julia " + baseJulia.version;

    meta = baseJulia.meta // {
      baseName = attr;
      inherit displayName settingsSchema;
      version = baseJulia.version;
      icon = ./logo-64x64.png;
    };

    python = python3;

  in rec {
    packageOptions = {};
    packageSearch = common.searcher' {
      packages = packageSet;
      packageMustBeDerivation = false;
    };

    languageServerOptions = attrs: julia: packageNames: settings: {
      LanguageServer = callPackage ./language-server-LanguageServer.nix {
        inherit attrs julia packageNames settings;
        kernelName = attr;
        juliaLsp = value ["LanguageServer" "SymbolServer"];
      };
    };
    languageServerSearch = common.searcher (languageServerOptions [] baseJulia [] (common.makeDefaultSettings settingsSchema));

    build = args@{
      packages ? []
      , languageServers ? []
      , attrs ? [attr "julia"]
      , extensions ? ["jl"]
      , settings ? {}
      , metaOnly ? false
    }:
      let
        julia = value (["IJulia"] ++ packages);
        settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
      in
        symlinkJoin {
          name = attr;

          paths = [
            (callPackage ./kernel.nix { inherit julia python attrs extensions displayName; })
            (callPackage ./mode_info.nix { inherit attrs extensions; })
          ]
          ++ (if metaOnly then [] else [julia])
          ++ (if metaOnly then [] else (map (y: builtins.getAttr y (languageServerOptions attrs julia packages (common.focusSettings "LanguageServer." settingsToUse))) languageServers))
          ;

          passthru = {
            args = args // { baseName = attr; };
            inherit meta packageOptions;
            languageServerOptions = languageServerOptions julia attrs;
          };
        };

    inherit meta;
  }
) baseCandidates
