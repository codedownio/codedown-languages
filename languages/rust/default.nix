{ lib
, pkgs
, callPackage
, writeTextDir
, symlinkJoin

, darwin
, rust-analyzer
}:

with lib;

let
  common = callPackage ../common.nix {};

  settingsSchema = [
    {
      target = "lsp.rust-analyzer.enable";
      title = "Enable rust-analyzer";
      type = "boolean";
      defaultValue = true;
    }
    {
      target = "lsp.rust-analyzer.debug";
      title = "Rust-analyzer: enable debug output";
      description = "Print verbose debug output.";
      type = "boolean";
      defaultValue = false;
    }
  ];

  chooseLanguageServers = settings: rust: cargoHome: kernelName:
    []
    ++ lib.optionals (common.isTrue settings "lsp.rust-analyzer.enable") [(callPackage ./language_server_rust_analyzer/config.nix {
      inherit rust cargoHome kernelName;
      settings = common.focusSettings "lsp.rust-analyzer." settings;
    })]
  ;

  allPackageNames = import ./all_package_names.nix;

  # Note: update this when the base Nixpkgs is bumped
  baseCandidates = [
    "rust"
    "rust_1_70"
    "rust_1_71"
    "rust_1_72"
    "rust_1_73"
    "rust_1_74"
    "rust_1_75"
    "rust_1_76"
    "rust_1_77"
    "rust_1_78"
    "rust_1_79"
    "rust_1_80"
    "rust_1_81"
    "rust_1_82"
  ];

in

with lib;

listToAttrs (map (x:
  let
    rust = getAttr x pkgs;
    rustPackages = rust.packages.stable;

    displayName = "Rust";

    meta = rustPackages.rustc.meta // {
      baseName = x;
      inherit displayName;
      icon = ./rust-logo-64x64.png;
      inherit settingsSchema;
    };

    evcxrBase = callPackage ./evcxr {
      inherit (darwin.apple_sdk.frameworks) CoreServices Security;
    };

    packageOptions = listToAttrs (map (x: {
      name = x;
      value = {
        meta = {
          name = x;
        };
      };
    }) allPackageNames);

    packageSearch = common.searcher' {
      packageMustBeDerivation = false;
      packages = packageOptions;
    };

    versions = {
      cargo = rustPackages.cargo.version;
      evcxr = evcxrBase.version;
      rust = rustPackages.rustc.version;
      rust-analyzer = rust-analyzer.version;
    };

  in {
    name = x;
    value = lib.makeOverridable ({
      packages ? []
      , attrs ? [x "rust"]
      , extensions ? ["rs" "rlib"]
      , settings ? {}
    }@args: let
      evcxr = (evcxrBase.override {
        rustPlatform = rustPackages.rustPlatform;
        cargo = rustPackages.cargo;
      }).withPackages packages;

      settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
      languageServers = chooseLanguageServers settingsToUse rust evcxr.cargoHome x;
    in symlinkJoin {
      name = "rust";

      paths = [
        (callPackage ./kernel.nix {
          inherit evcxr;
          inherit displayName attrs extensions;
          version = rustPackages.rustc.version;
        })

        rustPackages.rustc
        rustPackages.cargo
        pkgs.gcc
      ]
      ++ languageServers
      ;

      passthru = {
        args = args // { baseName = x; };
        inherit meta packageOptions packageSearch versions;
        inherit settingsSchema settings;
        inherit (evcxr) cratesIndex;
        modes = {
          inherit attrs extensions;
          code_mirror_mode = "rust";
        };
        languageServerNames = map (x: x.languageServerName) languageServers;
      };
    }
    ) {};
  }
) (filter (x: hasAttr x pkgs) baseCandidates))
