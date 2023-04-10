{ lib
, julia_16-bin
, julia_18
, python3
, callPackage
, writeTextDir
, stdenv
, runCommand
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  juliaWithPackages = callPackage ./julia-modules {};

  baseCandidates = rec {
    julia = julia18;

    julia16 = juliaWithPackages.override { julia = julia_16-bin; };

    julia18 = juliaWithPackages.override { julia = julia_18; };
  };

  packageSet = lib.listToAttrs (map (x: {
    name = x;
    value = {};
  }) (import ./julia-modules/package-names.nix));

in

with lib;

mapAttrs (attr: value:
  let
    baseJulia = (value []).julia;

    displayName = "Julia " + baseJulia.version;

    meta = baseJulia.meta // {
      baseName = attr;
      inherit displayName;
      version = baseJulia.version;
      icon = ./logo-64x64.png;
    };

    python = python3;

  in rec {
    packageOptions = {};
    packageSearch = common.searcher packageSet;

    languageServerOptions = julia: attrs: {
      LanguageServer = callPackage ./language-server-LanguageServer.nix {
        inherit julia attrs;
        kernelName = attr;
        juliaLsp = value ["LanguageServer"];
      };
    };
    languageServerSearch = common.searcher (languageServerOptions baseJulia []);

    build = args@{
      packages ? []
      , languageServers ? []
      , attrs ? [attr "julia"]
      , extensions ? ["jl"]
      , metaOnly ? false
    }:
      let
        julia = value packages;
      in
        symlinkJoin {
          name = attr;

          paths = [
            (callPackage ./kernel.nix { inherit julia python attrs extensions displayName; })
            (callPackage ./mode_info.nix { inherit attrs extensions; })
          ]
          ++ (if metaOnly then [] else [julia])
          ++ (if metaOnly then [] else (map (y: builtins.getAttr y (languageServerOptions julia attrs)) languageServers))
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
