{ isCodeDown ? true # For introspection using builtins.functionArgs
, overlays ? []
, system ? null
, fetchFromGitHub ? null
, ...
}:

let
  stableRev = "ca23145d9899c9d4b7f035339e2f21cf3106aa79"; # nixpkgs-rev
  stableFetchFromGitHub = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = stableRev;
    hash = "sha256-UxFzpSVQ7zZdd9bPc/sgCkbtSMwJkAKi82tas5FXgFI="; # nixpkgs-hash
  };
  stableBuiltins = builtins.fetchTarball {
    url = ''https://github.com/NixOS/nixpkgs/archive/${stableRev}.tar.gz'';
    sha256 = "0ll0ay8v6nkbyfi05409ri4fsiha43xp7kynfxfkdvsh4njp64ak"; # nixpkgs-sha256
  };
  pkgsStableSrc = if fetchFromGitHub != null then stableFetchFromGitHub else stableBuiltins;
  pkgsStable = import pkgsStableSrc ({
    inherit overlays;
  } // (if system == null then {} else { inherit system; }));

  # Nothing builds from master. Importing it makes everyone realizing an environment fetch a
  # second Nixpkgs, so it's off. To re-enable, uncomment this block and the pkgsMaster lines in
  # flake.nix, and restore the nixpkgs-master check in .aliases/dev-verify-default-nix.
  # masterRev = "f4d46d85b687293bc8d872010fb66a4f23c23139"; # nixpkgs-master-rev
  # masterFetchFromGitHub = fetchFromGitHub {
  #   owner = "NixOS";
  #   repo = "nixpkgs";
  #   rev = masterRev;
  #   hash = "sha256-j0P9+h7HX67KNlGki6puFfx8xO6wx4Jz23jXg3dpfCw="; # nixpkgs-master-hash
  # };
  # masterBuiltins = builtins.fetchTarball {
  #   url = ''https://github.com/NixOS/nixpkgs/archive/${masterRev}.tar.gz'';
  #   sha256 = "0b3wd5vq7mvqvdrq5ixhxv27rz0mdsm8p92i6v5awpy73vxgshwg"; # nixpkgs-master-sha256
  # };
  # pkgsMasterSrc = if fetchFromGitHub != null then masterFetchFromGitHub else masterBuiltins;
  # pkgsMaster = import pkgsMasterSrc ({
  #   inherit overlays;
  # } // (if system == null then {} else { inherit system; }));

  masterDisabled = throw "pkgsMaster is disabled; see the comment above in default.nix";
  pkgsMasterSrc = masterDisabled;
  pkgsMaster = masterDisabled;

in

pkgsStable.callPackage ./codedown.nix {
  inherit
    pkgsStableSrc pkgsStable
    pkgsMasterSrc pkgsMaster
  ;
}
