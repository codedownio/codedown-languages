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
    languageServerOptions = value.languageServerOptions;
    lspDepot = value.lspDepot;

    displayName = "Julia " + baseJulia.version;

    meta = baseJulia.meta // {
      baseName = attr;
      inherit displayName;
      version = baseJulia.version;
      icon = ./logo-64x64.png;
    };

    python = pkgs.python3;

    packageOptions = {};
    packageSearch = common.searcher {};

    languageServerSearch = common.searcher languageServerOptions;

  in {
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

        languageServerOptions = {
          LanguageServer = callPackage ./language-server-LanguageServer.nix {
            inherit baseJulia attrs;
            depot = callPackage lspDepot {
              inherit baseJulia python;
            };
          };
        };
        availableLanguageServers = languageServerOptions;
      in
        symlinkJoin {
          name = "julia";

          paths = [
            (callPackage ./kernel.nix { inherit julia python attrs extensions displayName; })
            (callPackage ./mode_info.nix { inherit attrs extensions; })
            (writeTextDir "lib/codedown/language-servers/julia.yaml" (
              pkgs.lib.generators.toYAML {} (map (x: x.config) (map (x: getAttr x availableLanguageServers) languageServers))
            ))
          ]
          ++ (if metaOnly then [] else [julia])
          ;

          passthru = {
            args = args // { baseName = attr; };
            inherit meta packageOptions languageServerOptions;
          };
        };

    inherit meta;
  }
) baseCandidates
