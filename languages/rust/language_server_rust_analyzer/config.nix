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

common.writeTextDirWithMeta rust-analyzer.meta "lib/codedown/language-servers/rust-analyzer.yaml"
  (lib.generators.toYAML {} [{
    name = "rust-analyzer";
    display_name = "rust-analyzer";
    description = rust-analyzer.meta.description;
    icon = ./logo-64x64.png;
    extensions = ["rs" "rlib"];
    notebook_suffix = ".rs";
    kernel_name = kernelName;
    attrs = ["rust"];
    type = "stream";
    args = ["${rust-analyzer}/bin/rust-analyzer"];
    initialization_options = {
      "rust-analyzer.linkedProjects" = [];
    };
  }])
