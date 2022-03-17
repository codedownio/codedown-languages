{ lib
, fetchFromGitHub
, fetchgit
, overlays
}:

let

  channelSpecToChannel = name: channel:
    if (channel.tag == "fetch_from_github") then fetchFromGitHub ((removeAttrs channel ["tag" "name"]))
    else if (channel.tag == "fetch_git") then fetchgit (removeAttrs channel ["tag" "name"])
    else if (channel.tag == "path") then channel.path else null;

in

rec {
  channels = {
    nixpkgs = {
      tag = "fetch_from_github";
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "973910f5c31b9ba6c171c33a8bd7199990b14c72";
      sha256 = "1n1kibmn1fcjazaxp8lydwdx646lknqksv5b2fm33fdq2dvafvj7";
    };

    nixpkgs-unstable = {
      tag = "fetch_from_github";
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "ecaf3da9340231e5493eccc3db87604a3705da42";
      sha256 = "049dcpzklpjj0c7g172njfcqb9xvxkpyf7qjiwvaf8klgd5cippa";
    };
  };

  importedOverlays = lib.mapAttrsToList (name: value: import (channelSpecToChannel name value)) overlays;
  importedChannels = lib.mapAttrs (name: value: import (channelSpecToChannel name value) { overlays = importedOverlays; }) channels;
}
