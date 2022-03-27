{
  description = "CodeDown languages";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/973910f5c31b9ba6c171c33a8bd7199990b14c72";
  inputs.nixpkgs-unstable.url = "github:NixOS/nixpkgs/ecaf3da9340231e5493eccc3db87604a3705da42";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, nixpkgs-unstable, flake-utils }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import ./default.nix) ];
        pkgs = import nixpkgs { inherit system overlays; };
        pkgsUnstable = import nixpkgs-unstable { inherit system overlays; };

        channelSpecToChannel = name: channel:
          if (channel.tag == "fetch_from_github") then pkgs.fetchFromGitHub ((removeAttrs channel ["tag" "name"]))
          else if (channel.tag == "fetch_git") then pkgs.fetchgit (removeAttrs channel ["tag" "name"])
          else if (channel.tag == "path") then channel.path else null;
      in
        {
          packages = rec {
            exportersSearcher = pkgs.codedown.exportersSearcher;
            languagesSearcher = pkgs.codedown.languagesSearcher;

            environment = pkgs.callPackage ./environment.nix (rec {
              channels = pkgs.lib.listToAttrs (map (x: {
                name = x;
                value = {
                  tag = "fetch_from_github";
                  owner = "NixOS";
                  repo = "nixpkgs";
                  rev = inputs.${x}.rev;
                  sha256 = inputs.${x}.narHash;
                };
              }) ["nixpkgs" "nixpkgs-unstable"]);
              # importedChannels = pkgs.lib.listToAttrs (map (x: {
              #   name = x;
              #   value = import inputs.${x} { inherit system overlays; };
              # }) ["nixpkgs" "nixpkgs-unstable"]);
              importedChannels = { nixpkgs = pkgs; nixpkgs-unstable = pkgsUnstable; };

              overlays = {
                codedown = {
                  tag = "path";
                  path = ./.;
                };
              };
              importedOverlays = pkgs.lib.mapAttrsToList (name: value: import (channelSpecToChannel name value)) overlays;
            });
          };
        }
    );
}
