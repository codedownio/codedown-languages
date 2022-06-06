{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/973910f5c31b9ba6c171c33a8bd7199990b14c72";
  inputs.nixpkgs-unstable.url = "github:NixOS/nixpkgs/ecaf3da9340231e5493eccc3db87604a3705da42";

  # When updating this, make sure to run ./update.sh to generate the new compiler set!
  inputs.haskellNixSrc.url = "github:input-output-hk/haskell.nix/f3ea06dcacc8a46b4a207a6a1fad14bc5ea41b19";

  inputs.ihaskell.url = "github:IHaskell/IHaskell/10c93054debd329a22872c93df21ece5165d74ab";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, nixpkgs-unstable, haskellNixSrc, ihaskell, flake-utils }@inputs:
    # flake-utils.lib.eachDefaultSystem (system:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let
        overlays = [
          haskellNixSrc.outputs.overlay (final: prev: {
            inherit (pkgs.lib.getAttr system ihaskell.packages) ihaskell-884 ihaskell-8107 ihaskell-902 ihaskell-921;
          })
        ];

        pkgs = import nixpkgs { inherit system overlays; };
        pkgsUnstable = import nixpkgs-unstable { inherit system overlays; };

        callEnvironment = path: args: pkgs.callPackage path (rec {
          channels = (pkgs.lib.listToAttrs (map (x: {
            name = x;
            value = {
              tag = "fetch_from_github";
              owner = "NixOS";
              repo = "nixpkgs";
              rev = inputs.${x}.rev;
              sha256 = inputs.${x}.narHash;
            };
          }) ["nixpkgs" "nixpkgs-unstable"])) // {
            codedown = {
              tag = "path";
              path = ./default_old.nix;
            };
          };

          # importedChannels = pkgs.lib.listToAttrs (map (x: {
          #   name = x;
          #   value = import inputs.${x} { inherit system overlays; };
          # }) ["nixpkgs" "nixpkgs-unstable"]);
          importedChannels = {
            nixpkgs = pkgs;
            nixpkgs-unstable = pkgsUnstable;
            codedown = pkgs.callPackage ./codedown.nix {};
          };

          overlays = {};
          importedOverlays = pkgs.lib.mapAttrsToList (name: value: import (channelSpecToChannel name value)) overlays;
        } // args);

        channelSpecToChannel = name: channel:
          if (channel.tag == "fetch_from_github") then pkgs.fetchFromGitHub ((removeAttrs channel ["tag" "name"]))
          else if (channel.tag == "fetch_git") then pkgs.fetchgit (removeAttrs channel ["tag" "name"])
          else if (channel.tag == "path") then channel.path else null;
      in
        rec {
          checks = let checks = with pkgs.lib; import ./checks.nix; in (
            pkgs.lib.listToAttrs (pkgs.lib.flatten ((pkgs.lib.mapAttrsToList (n: v: [{
              name = n + "-build-environment";
              value = callEnvironment ./empty_environment.nix { inherit (v) kernels; };
            } {
              name = n + "-run-code";
              value = pkgs.callPackage ./check_code.nix {
                inherit (v) codeExecutions;
                jupyter_path = "${callEnvironment ./empty_environment.nix { inherit (v) kernels; }}/lib/codedown";
              };
            }])) checks))
          );

          packages = rec {
            exportersSearcher = pkgs.codedown.exportersSearcher;
            shellsSearcher = pkgs.codedown.shellsSearcher;
            languagesSearcher = pkgs.codedown.languagesSearcher;

            haskellCompilers = pkgs.callPackage ./languages/haskell/generate.nix {};

            codedown = pkgs.codedown;

            jupyter-runner = with pkgs; let
              pythonEnv = python38.withPackages (ps: with ps; [papermill]);
              packages = [coreutils findutils pythonEnv];
              in
                runCommand "papermill" { buildInputs = [makeWrapper]; } ''
                  makeWrapper ${pythonEnv}/bin/papermill $out \
                    --set PATH ${lib.makeBinPath packages}
                '';

            environment = callEnvironment ./environment.nix {};

            ci = pkgs.callPackage ./ci.nix { inherit checks; };
          };
        }
    );
}
