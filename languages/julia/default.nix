{ lib
, pkgs
, callPackage
, writeTextDir
, stdenv
, runCommand
, symlinkJoin
}:

let
  common = callPackage ../common.nix {};

  baseCandidates = rec {
    julia = julia18;

    julia16 = {
      baseJulia = pkgs.julia_16-bin;
      depot = ./depot_16;
      lspDepot = ./depot_LanguageServer_16;
    };

    julia18 = {
      baseJulia = pkgs.julia_18-bin;
      depot = ./depot_18;
      lspDepot = ./depot_LanguageServer_18;
    };
  };

in

with lib;

mapAttrs (attr: value:
  let
    baseJulia = value.baseJulia;
    depot = value.depot;
    lspDepot = value.lspDepot;

    displayName = "Julia " + baseJulia.version;

    meta = baseJulia.meta // {
      baseName = attr;
      inherit displayName;
      version = baseJulia.version;
      icon = ./logo-64x64.png;
    };

    python = pkgs.python3;

  in rec {
    packageOptions = {};
    packageSearch = common.searcher {};

    languageServerOptions = julia: attrs: {
      LanguageServer = callPackage ./language-server-LanguageServer.nix {
        inherit julia attrs;
        kernelName = attr;
        juliaLsp = callPackage lspDepot {
          inherit baseJulia python;
        };
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
        julia = callPackage depot {
          inherit baseJulia python;
        };
      in
        symlinkJoin {
          name = "julia";

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
