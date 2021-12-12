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
    "julia-stable"
    "julia-stable-bin"
    "julia_10"
    "julia_10-bin"
    # "julia_11"
    # "julia_13"
    "julia_15"
    "julia_16"
    "julia_16-bin"
  ];

  validCandidate = x:
    (lib.hasAttr x pkgs)
    && (builtins.tryEval (pkgs."${x}".meta)).success
    && !(lib.attrByPath [x "meta" "broken"] false pkgs);

in

with lib;

listToAttrs (map (x:
  let
    baseJulia = getAttr x pkgs;

    meta = baseJulia.meta // {
      baseName = x;
      displayName = "Julia " + baseJulia.version;
      icon = ./logo-64x64.png;
    };

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
        , attrs ? ["julia"]
        , extensions ? ["jl"]
      }:
        let
          julia = callPackage ./depot {
            julia = baseJulia;
            python = pkgs.python3;
          };
          python = julia.python;
          availableLanguageServers = metadata.languageServerOptions base python.pkgs;
        in
          symlinkJoin {
            name = "julia";

            paths = [
              julia
              (callPackage ./kernel.nix { inherit julia python attrs extensions; })
              (callPackage ./mode_info.nix { inherit attrs extensions; })
              (writeTextDir "lib/codedown/julia-language-servers.yaml" (
                pkgs.lib.generators.toYAML {} (map (x: x.config) (map (x: getAttr x availableLanguageServers) languageServers))
              ))
            ];

            passthru = {
              args = args // { baseName = x; };
              inherit meta packageOptions languageServerOptions;
            };
          };

      inherit meta;
    };
  }
) (filter validCandidate baseCandidates))

  # homeFolderPaths = runCommand "julia-home-folder" {inherit julia python;} ''
  #   mkdir -p $out/home
  #   cp ${./depot/Manifest.toml} $out/home/Manifest.toml
  #   cp ${./depot/Project.toml} $out/home/Project.toml

  #   mkdir -p $out/home/.julia/config
  #   echo "using Pkg" >> $out/home/.julia/config/startup.jl
  #   echo 'Pkg.activate("/home/user")' >> $out/home/.julia/config/startup.jl
  # '';

# extraGitIgnoreLines = [".julia"];
