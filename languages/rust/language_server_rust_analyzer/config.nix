{ lib
, callPackage
, runCommand
, stdenv

, haskell
, python
, rust
, rust-analyzer

, kernelName
, settings
}:

with lib;

let
  common = callPackage ../../common.nix {};

  # rnlsSrc = fetchFromGitHub {
  #   owner = "codedownio";
  #   repo = "rust-notebook-language-server";
  #   rev = "d80e47a03ea8bce6c186463d4220fea91475cc51";
  #   sha256 = "14j8cc29ahjbq3m7a4g0939sv3mhb0skhy18r663lky5grkiw6aw";
  # };
  rnlsSrc = /home/tom/tools/rust-notebook-language-server;

  ghc = haskell.packages.ghc924;

  rnls = ghc.callPackage rnlsSrc {
    myers-diff = ghc.callPackage ./myers-diff.nix {};
  };

  rustAnalyzerToUse = rust-analyzer.override {
    rustPlatform = rust.packages.stable.rustPlatform;
  };

  rust-env = runCommand "rust-environment" {} ''
    mkdir -p $out
    cp ${./Cargo.toml} $out/Cargo.toml
    mkdir -p $out/src
    touch $out/src/lib.rs
  '';

  shadowDirTemplate = rust-env;

  config = raw: {
    name = "rust-analyzer${if raw then "-raw" else ""}";
    display_name = "rust-analyzer";
    description = rustAnalyzerToUse.meta.description;
    icon = ./logo-64x64.png;
    extensions = ["rs" "rlib"];
    notebook_suffix = ".rs";
    kernel_name = kernelName;
    attrs = if raw then [] else ["rust"];
    type = "stream";
    args = if raw then [
      "${rustAnalyzerToUse}/bin/rust-analyzer"
    ] else [
      "${rnls}/bin/rust-notebook-language-server"
      "--wrapped-server" "${rustAnalyzerToUse}/bin/rust-analyzer"
      "--shadow-dir-template" "${shadowDirTemplate}"
    ]
    ++ lib.optionals settings.debug ["--log-level" "debug"];
    initialization_options = {
      "rust-analyzer.linkedProjects" = [
        "${rust-env}/Cargo.toml"
      ];
    };
    env = {

    };
  };

in

common.writeTextDirWithMeta rust-analyzer.meta "lib/codedown/language-servers/rust-analyzer.yaml" (lib.generators.toYAML {} [
  (config false)
])
