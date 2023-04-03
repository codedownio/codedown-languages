{ stdenv
, lib
, runCommand
, callPackage
, python
, rust
, rust-analyzer
, kernelName
}:

with lib;

let
  common = callPackage ../../common.nix {};

  rustAnalyzerToUse = rust-analyzer.override {
    rustPlatform = rust.packages.stable.rustPlatform;
  };

  rust-env = runCommand "rust-environment" {} ''
    mkdir -p $out
    cp ${./Cargo.toml} $out/Cargo.toml
  '';

in

common.writeTextDirWithMeta rust-analyzer.meta "lib/codedown/language-servers/rust-analyzer.yaml"
  (lib.generators.toYAML {} [{
    name = "rust-analyzer";
    display_name = "rust-analyzer";
    description = rustAnalyzerToUse.meta.description;
    icon = ./logo-64x64.png;
    extensions = ["rs" "rlib"];
    notebook_suffix = ".rs";
    kernel_name = kernelName;
    attrs = ["rust"];
    type = "stream";
    args = ["${rustAnalyzerToUse}/bin/rust-analyzer"];
    initialization_options = {
      "rust-analyzer.linkedProjects" = [
        "${rust-env}/Cargo.toml"
      ];
    };
  }])
