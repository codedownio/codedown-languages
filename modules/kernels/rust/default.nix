{ lib
, pkgs
, callPackage
, writeTextDir
, symlinkJoin

, darwin
, rust-analyzer

, rust

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

with lib;

let
  common = callPackage ../common.nix {};

  evcxr = ((callPackage ./evcxr {
    inherit (darwin.apple_sdk.frameworks) CoreServices Security;
  }).override {
    rustPlatform = rustPackages.rustPlatform;
    cargo = rustPackages.cargo;
  }).withPackages packages;

  kernelName = "rust";

  languageServers =
    []
    ++ lib.optionals settings.lsp.rust-analyzer.enable [(callPackage ./language_server_rust_analyzer/config.nix {
      inherit rust kernelName;
      cargoHome = evcxr.cargoHome;
      settings = settings.lsp.rust-analyzer;
    })]
  ;

  allPackageNames = import ./all_package_names.nix;

  rustPackages = rust.packages.stable;

  displayName = "Rust";

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
    inherit (evcxr) cratesIndex;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "rust";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
