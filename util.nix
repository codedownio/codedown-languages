{ pkgs
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

    overlays = {};
    importedOverlays = pkgs.lib.mapAttrsToList (name: value: import (channelSpecToChannel name value)) overlays;
  } // args);
}
