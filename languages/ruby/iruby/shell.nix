{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    nixpkgs-fmt

    gem
    bundler
    bundix
  ];

  shellHook = ''
    # ...
  '';
}
