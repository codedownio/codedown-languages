{ lib
, callPackage
, coreutils
, runCommand

, rust
, rust-analyzer
, gcc

, cargoHome

, kernelName
, settings
}:

with lib;

let
  common = callPackage ../../common.nix {};

  rnls = callPackage ./rnls.nix {};

  rustAnalyzerToUse = rust-analyzer.override {
    rustPlatform = rust.packages.stable.rustPlatform;
  };

  shadowDirTemplate = cargoHome;

  raw = false;

  languageServerName = "rust-analyzer${if raw then "-raw" else ""}";

  passthru = {
    inherit languageServerName;
  };

  config = {
    name = languageServerName;
    version = rustAnalyzerToUse.version;
    display_name = "rust-analyzer";
    description = rustAnalyzerToUse.meta.description;
    icon = ./rust-analyzer-logo-64x64.png;
    extensions = ["rs" "rlib"];
    notebook_suffix = if raw then ".rs" else "";

    # Replace notebook end delimiters with semicolons. This will allow us to have cells that end with non-semicolon
    # expressions.
    # If the cell already has a semicolon, rust-analyzer will add a diagnostic complaining about the extra one.
    # But that's fine, because it will get untransformed out of existence.
    replace_end_delimiter_with = ";";

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
      "linkedProjects" = [];

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
      "PATH" = lib.makeBinPath [
        coreutils
        rust.packages.stable.cargo
        gcc.out
      ];
      # "RA_LOG" = "rust_analyzer=info";
    };
  };

in

common.writeTextDirWithMetaAndPassthru rust-analyzer.meta passthru "lib/codedown/language-servers/rust-${kernelName}-rust-analyzer.yaml" (lib.generators.toYAML {} [
  config
])
