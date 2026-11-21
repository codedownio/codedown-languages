# Lean gets its own nixpkgs: 26.05's mathlib is too big for Hydra to cache (NixOS/nixpkgs#510957).
{ fetchFromGitHub
, stdenv
}:

import (fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "0bb8272318b4ca41bf0edc6224df3e3e3046b70d"; # nixos-unstable, 2026-11-19
  hash = "sha256-S5keGWzaKpuXo9MNoIC2FfTIuTyTnx8P4NhYJMEsTLI=";
}) { inherit (stdenv.hostPlatform) system; }
