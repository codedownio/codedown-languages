# { pkgs ? import <nixpkgs> {} }:

# let
#   inherit (pkgs) lib;

#   pyproject-nix = import (builtins.fetchGit {
#     url = "https://github.com/pyproject-nix/pyproject.nix.git";
#   }) {
#     inherit lib;
#   };

#   uv2nix = import (builtins.fetchGit {
#     url = "https://github.com/pyproject-nix/uv2nix.git";
#   }) {
#     inherit pyproject-nix lib;
#   };

#   pyproject-build-systems = import (builtins.fetchGit {
#     url = "https://github.com/pyproject-nix/build-system-pkgs.git";
#   }) {
#     inherit pyproject-nix uv2nix lib;
#   };

# in

# pkgs.mkShell {
#   buildInputs = with pkgs; [
#     python314
#     # uv2nix
#   ];
# }
