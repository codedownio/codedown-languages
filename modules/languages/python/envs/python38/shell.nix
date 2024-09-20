{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    (poetry.override { python3 = python38; })
  ];
}
