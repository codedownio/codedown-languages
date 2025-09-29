{ isCodeDown ? true # For introspection using builtins.functionArgs
, overlays ? []
, system ? null
, fetchFromGitHub ? null
, ...
}:

let
  stableRev = "3634657dc244b3e4868a9b37b7243ea33aa786ec"; # nixpkgs-rev
  stableFetchFromGitHub = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = stableRev;
    hash = "sha256-B+oXXmNRTOajmEpzYJDrpZmYRFIBWA/fKCgY5CXCc5M="; # nixpkgs-hash
  };
  stableBuiltins = builtins.fetchTarball {
    url = ''https://github.com/NixOS/nixpkgs/archive/${stableRev}.tar.gz'';
    sha256 = "14vkq8jy861853ghyn01a929i6d5xf860wsak2iyck2icdg1gsh7"; # nixpkgs-sha256
  };
  pkgsStableSrc = if fetchFromGitHub != null then stableFetchFromGitHub else stableBuiltins;
  pkgsStable = import pkgsStableSrc ({
    inherit overlays;
  } // (if system == null then {} else { inherit system; }));

  masterRev = "3813f3fcfd15bac0df363cca1fd35206e66f25a9"; # nixpkgs-master-rev
  masterFetchFromGitHub = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = masterRev;
    hash = "sha256-tdHRCD5+cNRX27JU/7ionH3Gu5R4sNxhVCownep6ibo="; # nixpkgs-master-hash
  };
  masterBuiltins = builtins.fetchTarball {
    url = ''https://github.com/NixOS/nixpkgs/archive/${masterRev}.tar.gz'';
    sha256 = "1fl9gbm9sc1aaihxrc3qjjxwczcwm2wgym5jvdbx8w3y7q4d3ldm"; # nixpkgs-master-sha256
  };
  pkgsMasterSrc = if fetchFromGitHub != null then masterFetchFromGitHub else masterBuiltins;
  pkgsMaster = import pkgsMasterSrc ({
    inherit overlays;
  } // (if system == null then {} else { inherit system; }));

in

pkgsStable.callPackage ./codedown.nix {
  inherit
    pkgsStableSrc pkgsStable
    pkgsMasterSrc pkgsMaster
  ;
}
