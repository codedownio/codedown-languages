{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  environment = import ./environment.nix;

in

mkShell {
  JUPYTER_PATH = "${environment}/lib/codedown";

  buildInputs = [
    environment
    (python3.withPackages (ps: with ps; [jupyter ipython]))
  ];
}
