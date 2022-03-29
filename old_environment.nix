
let
  bootstrapNixpkgs = import <nixpkgs> {};
  fetchgit = bootstrapNixpkgs.fetchgit;
  fetchFromGitHub = bootstrapNixpkgs.fetchFromGitHub;
  lib = bootstrapNixpkgs.lib;

  overlays = {
    codedown = {
      tag = "path";
      path = ./default_old.nix;
    };
  };

  channelsAll = with bootstrapNixpkgs; callPackage ./channels.nix { inherit overlays; };

in

bootstrapNixpkgs.callPackage ./environment.nix {
  inherit overlays;
  inherit (channelsAll) channels importedChannels importedOverlays;
}
