{ pkgs
, pkgsUnstable
, channelSpecToChannel
, inputs
}:

{
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
}
