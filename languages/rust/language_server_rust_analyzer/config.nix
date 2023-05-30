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
    rev = "2f03f8052d45f8a83b28ee42f5b1d8f20616ff07";
    sha256 = "1a5hqirv2zv12l854vdzilvyg7gb8p762s16rnz1fd83bf1sim12";
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
      "linkedProjects" = [
        "${rust-env}/Cargo.toml"
      ];

      # Based on https://github.com/emacs-lsp/lsp-mode/blob/c8bbdb80ed2dbfdbf55b6c4058308dab092306b5/clients/lsp-rust.el#L734
      "diagnostics" = {
        "enable" = true;
        "enableExperimental" = false;
        "disabled" = [];
        "warningsAsHint" = [];
        "warningsAsInfo" = [];
      };
      "imports" = {
        "granularity" = {
          "enforce" = false;
          "group" = "crate";
        };
        "group" = true;
        "merge" = {
          "glob" = true;
        };
        "prefix" = "plain";
      };
      "lruCapacity" = null;
      "checkOnSave" = {
        "enable" = true;
        "command" = "check";
        "extraArgs" = [];
        "features" = [];
        "overrideCommand" = [];
      };
      "files" = {
        "exclude" = [];
        "watcher" = "client";
        "excludeDirs" = [];
      };
      "cargo" = {
        "allFeatures" = false;
        "noDefaultFeatures" = false;
        "features" = [];
        "target" = null;
        "runBuildScripts" = true;
        "loadOutDirsFromCheck" = true;
        "autoreload" = true;
        "useRustcWrapperForBuildScripts" = true;
        "unsetTest" = [];
        "sysroot" = "${rust.packages.stable.rustc}";
        "sysrootSrc" = "${rust.packages.stable.rustPlatform.rustLibSrc}";
      };
      "rustfmt" = {
        "extraArgs" = [];
        "overrideCommand" = [];
        "rangeFormatting" = {
          "enable" = false;
        };
      };
      "inlayHints" = {
        "bindingModeHints" = false;
        "chainingHints" = false;
        "closingBraceHints" = {
          "enable" = true;
          "minLines" = 25;
        };
        "closureReturnTypeHints" = false;
        "lifetimeElisionHints" = {
          "enable" = "never";
          "useParameterNames" = false;
        };
        "maxLength" = null;
        "parameterHints" = false;
        "reborrowHints" = "never";
        "renderColons" = true;
        "typeHints" = {
          "enable" = false;
          "hideClosureInitialization" = false;
          "hideNamedConstructor" = false;
        };
      };
      "completion" = {
        "addCallParenthesis" = true;
        "addCallArgumentSnippets" = true;
        "postfix" = {
          "enable" = true;
        };
        "autoimport" = {
          "enable" = true;
        };
        "autoself" = {
          "enable" = true;
        };
      };
      "callInfo" = {
        "full" = true;
      };
      "procMacro" = {
        "enable" = true;
      };
      "rustcSource" = null;
      "highlighting" = {
        "strings" = true;
      };
      "experimental" = {
        "procAttrMacros" = true;
      };
    };
    env = {
      "PATH" = lib.makeBinPath [coreutils rust.packages.stable.cargo];
      # "RA_LOG" = "rust_analyzer=info";
    };
  };

in

common.writeTextDirWithMeta rust-analyzer.meta "lib/codedown/language-servers/rust-${kernelName}-rust-analyzer.yaml" (lib.generators.toYAML {} [
  (config false)
])
