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

  baseCandidates = [
    "julia"
    "julia-bin"
    "julia-stable"
    "julia-stable-bin"

    "julia-lts"
    "julia-lts-bin"

    "julia_16-bin"
    "julia_17-bin"
    "julia_18-bin"
  ];

  validCandidate = x:
    (lib.hasAttr x pkgs)
    && (builtins.tryEval (pkgs."${x}".meta)).success
    && !(lib.attrByPath [x "meta" "broken"] false pkgs);

  depot = x:
    if lib.elem x ["julia_18-bin" "julia-stable" "julia-stable-bin"] then ./depot_18
    else ./depot_16;

in

with lib;

listToAttrs (map (x:
  let
    baseJulia = getAttr x pkgs;

    displayName = "Julia " + baseJulia.version;

    meta = baseJulia.meta // {
      baseName = x;
      inherit displayName;
      version = baseJulia.version;
      icon = ./logo-64x64.png;
    };

    python = pkgs.python3;

  in {
    name = x;
    value = rec {
      packageOptions = {};
      packageSearch = common.searcher {};

      languageServerOptions = {};
      languageServerSearch = common.searcher languageServerOptions;

      build = args@{
        packages ? []
        , languageServers ? []
        , attrs ? [x "julia"]
        , extensions ? ["jl"]
        , metaOnly ? false
      }:
        let
          julia = callPackage (depot x) {
            inherit baseJulia python;
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
              args = args // { baseName = x; };
              inherit meta packageOptions languageServerOptions;
            };
          };

      inherit meta;
    };
  }
) (filter validCandidate baseCandidates))
