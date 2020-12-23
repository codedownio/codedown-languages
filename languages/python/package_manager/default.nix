with import <nixpkgs> {};

(callPackage ./package_manager.nix { python = python3; name = "python38", displayName = "Python 3.8"; })
