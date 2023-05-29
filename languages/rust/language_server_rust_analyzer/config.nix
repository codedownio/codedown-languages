{ lib
, callPackage
, coreutils
, fetchFromGitHub
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

  rnlsSrc = fetchFromGitHub {
    owner = "codedownio";
    repo = "rust-notebook-language-server";
    rev = "83597467569a3745daee65fd4cb200c4f53d2921";
    sha256 = "18qjhsrwyhkg1sfvrl3mcngimvbmn00fv55pmfpbah7kwxjmqbk6";
  };
  # rnlsSrc = /home/tom/tools/rust-notebook-language-server;

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
    extensions = if raw then ["rs" "rlib"] else [];
    notebook_suffix = if raw then ".rs" else "";
    kernel_name = kernelName;
    attrs = if raw then [] else ["rust"];
    type = "stream";
    args = if raw then [
      "${rustAnalyzerToUse}/bin/rust-analyzer"
    ] else ([
      "${rnls}/bin/rust-notebook-language-server"
      "--wrapped-server" "${rustAnalyzerToUse}/bin/rust-analyzer"
      "--shadow-dir-template" "${shadowDirTemplate}"
    ]
    ++ lib.optionals settings.debug ["--log-level" "debug"]
    );
    initialization_options = {
      "rust-analyzer.linkedProjects" = [
        "${rust-env}/Cargo.toml"
      ];
    };
    env = {
      "PATH" = lib.makeBinPath [coreutils rust.packages.stable.cargo];
    };
  };

in

common.writeTextDirWithMeta rust-analyzer.meta "lib/codedown/language-servers/rust-${kernelName}-rust-analyzer.yaml" (lib.generators.toYAML {} [
  (config false)
])
