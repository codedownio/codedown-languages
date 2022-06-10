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
        name = x;
        url = "github:NixOS/nixpkgs/${inputs.${x}.rev}";
        type = "nixpkgs";
      };
    }) ["nixpkgs" "nixpkgs-unstable"])) // {
      codedown = {
        name = "codedown";
        url = path:./default_old.nix;
        type = "flake";
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
