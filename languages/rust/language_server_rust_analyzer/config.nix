{ stdenv
, lib
, pkgs
, callPackage
, python
, rust
, kernelName
}:

with lib;

let
  common = callPackage ../../common.nix {};

  rust-analyzer = pkgs.rust-analyzer.override {
    rustPlatform = rust.packages.stable.rustPlatform;
  };

in

common.writeTextDirWithMeta rust-analyzer.meta "lib/codedown/rust-analyzer-language-servers.yaml"
  (lib.generators.toYAML {} [{
    name = "rust-analyzer";
    display_name = "rust-analyzer";
    description = rust-analyzer.meta.description;
    extensions = ["rs" "rlib"];
    notebook_suffix = ".rs";
    kernel_name = kernelName;
    attrs = ["rust"];
    type = "stream";
    args = ["${rust-analyzer}/bin/rust-analyzer"];
    initialization_options = {};
  }])
