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
    "rust_1_63"
    "rust_1_64"
    "rust_1_65"
    "rust_1_66"
    "rust_1_67"
    "rust_1_68"
    "rust_1_69"
    "rust_1_70"
    "rust_1_71"
    "rust_1_72"
    "rust_1_73"
    "rust_1_74"
    "rust_1_75"
  ];

in

with lib;

listToAttrs (map (x:
  let
    rust = getAttr x pkgs;
    rustPackages = rust.packages.stable;

    displayName = "Rust " + rustPackages.rustc.version;

    meta = rustPackages.rustc.meta // {
      baseName = x;
      inherit displayName;
      icon = ./logo-64x64.png;
      inherit settingsSchema;
    };

  in {
    name = x;
    value = rec {
      packageOptions = listToAttrs (map (x: { name = x; value = {
        meta = {
          name = x;
        };
      }; }) allPackageNames);

      packageSearch = common.searcher' {
        packageMustBeDerivation = false;
        packages = packageOptions;
      };

      languageServerOptions = [
        rust-analyzer
      ];

      build = args@{
        packages ? []
        , attrs ? [x "rust"]
        , extensions ? ["rs" "rlib"]
        , settings ? {}
        , metaOnly ? false
      }: let
        evcxr = ((callPackage ./evcxr {
          inherit (darwin.apple_sdk.frameworks) CoreServices Security;
        }).override {
          rustPlatform = rustPackages.rustPlatform;
          cargo = rustPackages.cargo;
        }).withPackages packages;

        settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
      in symlinkJoin {
        name = "rust";

        paths = [
          (callPackage ./kernel.nix {
            inherit evcxr;
            inherit displayName attrs extensions;
          })

          (callPackage ./mode_info.nix { inherit attrs extensions; })
        ]
        ++ (if metaOnly then [] else [rustPackages.rustc rustPackages.cargo pkgs.gcc])
        ++ (if metaOnly then [] else chooseLanguageServers settingsToUse rust evcxr.cargoHome x)
        ;

        passthru = {
          args = args // { baseName = x; };
          inherit meta packageOptions;
        };
      };

      inherit meta;
    };
  }
) (filter (x: hasAttr x pkgs) baseCandidates))
