{ lib
, pkgs
, callPackage
, writeTextDir
, symlinkJoin
}:

with lib;

let
  common = callPackage ../common.nix {};

  settingsSchema = [
    # rust-analyzer.
    {
      target = "rust-analyzer.debug";
      title = "Rust-analyzer: enable debug output";
      description = "Print verbose debug output.";
      type = "boolean";
      defaultValue = false;
    }
  ];

  allLanguageServerOptions = rust: kernelName: focusedSettings: {
    rust-analyzer = callPackage ./language_server_rust_analyzer/config.nix {
      inherit rust kernelName;
      settings = focusedSettings;
    };
  };

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
    };

  in {
    name = x;
    value = rec {
      packageOptions = rustPackages;
      packageSearch = common.searcher packageOptions;

      languageServerOptions = allLanguageServerOptions rust "rust" (common.makeDefaultSettings settingsSchema);
      languageServerSearch = common.searcher languageServerOptions;

      build = args@{
        packages ? []
        , languageServers ? []
        , attrs ? [x "rust"]
        , extensions ? ["rs" "rlib"]
        , settings ? {}
        , metaOnly ? false
      }: let
        settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;
      in symlinkJoin {
        name = "rust";

        paths = [
          (callPackage ./kernel.nix {
            inherit displayName attrs extensions;
            evcxr = pkgs.evcxr.override {
              rustPlatform = rustPackages.rustPlatform;
              cargo = rustPackages.cargo;
            };
            rustLibSrc = rustPackages.rustPlatform.rustLibSrc;
            inherit rustPackages;
          })

          (callPackage ./mode_info.nix { inherit attrs extensions; })
        ]
        ++ (if metaOnly then [] else [
          rustPackages.rustc rustPackages.cargo pkgs.gcc
          (map (y: builtins.getAttr y (allLanguageServerOptions rust x (common.focusSettings "rust-analyzer." settingsToUse))) languageServers)
        ]);

        passthru = {
          args = args // { baseName = x; };
          inherit meta packageOptions languageServerOptions;
        };
      };

      inherit meta;
    };
  }
) (filter (x: hasAttr x pkgs) baseCandidates))
