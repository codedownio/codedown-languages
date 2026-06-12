{ lib
, pkgs
, callPackage
, symlinkJoin

, darwin
, rust-analyzer

, rust

, settings
, settingsSchema
, subPackageSettingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

with lib;

let
  common = callPackage ../common.nix {};

  rustPackages = rust.packages.stable;

  evcxrBase = (callPackage ./evcxr {
    inherit (darwin.apple_sdk.frameworks) CoreServices Security;
  }).override {
    rustPlatform = rustPackages.rustPlatform;
    cargo = rustPackages.cargo;
  };
  evcxr = evcxrBase.withPackages packages;

  # Crate -> available Cargo features, parsed once from the pinned index (~40k
  # crates, a few MB). Forced only when per-package settings schemas are built
  # (i.e. the package searcher), not for ordinary environment builds.
  cratesFeatures = builtins.fromJSON (builtins.readFile "${evcxrBase.cratesFeatures}");
  featuresOf = name: cratesFeatures.${name} or [];

  # The generic subPackage settings schema, but with the free-form `features`
  # field replaced by a multi-select enum of the crate's actual features. Crates
  # with no known features keep the free-form field.
  mkSubPackageSettingsSchema = features:
    if features == [] then subPackageSettingsSchema
    else subPackageSettingsSchema // {
      features = subPackageSettingsSchema.features // {
        listType = { type = "enum"; values = features; };
      };
    };

  languageServers =
    []
    ++ lib.optionals settings.lsp.rust-analyzer.enable [(callPackage ./language_server_rust_analyzer/config.nix {
      inherit rust kernelName;
      cargoHome = evcxr.cargoHome;
      settings = settings.lsp.rust-analyzer;
    })]
  ;

  displayName = "Rust";
  kernelName = "rust";

  packageOptions = listToAttrs (map (x: {
    name = x;
    value = {
      meta = {
        name = x;
      };
      settingsSchema = mkSubPackageSettingsSchema (featuresOf x);
    };
  }) (import ./all_package_names.nix));

  packageSearch = common.searcher' {
    packageMustBeDerivation = false;
    packages = packageOptions;
  };

in

symlinkJoin {
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
    meta = rustPackages.rustc.meta // {
      baseName = "rust";
      inherit displayName;
      icon = ./rust-logo-64x64.png;
      iconMonochrome = ./rust-logo-monochrome.svg;
      inherit settingsSchema;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch;
    versions = {
      cargo = rustPackages.cargo.version;
      evcxr = evcxr.version;
      rust = rustPackages.rustc.version;
      rust-analyzer = rust-analyzer.version;
    };
    inherit settingsSchema settings;
    inherit (evcxrBase) cratesIndex;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "rust";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
