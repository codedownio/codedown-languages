{ lib
, pkgs
, callPackage
, writeTextDir
, symlinkJoin
}:

with pkgs.lib;

let
  common = callPackage ../common.nix {};

  allLanguageServerOptions = rust: kernelName: {
    rust-analyzer = callPackage ./language_server_rust_analyzer/config.nix { inherit rust kernelName; };
  };

  baseCandidates = [
    "rust_1_40"
    "rust_1_41"
    "rust_1_42"
    "rust_1_43"
    "rust_1_44"
    "rust_1_45"
    "rust_1_46"
    "rust_1_47"
    "rust_1_48"
    "rust_1_49"
    "rust_1_50"
    "rust_1_51"
    "rust_1_52"
    "rust_1_53"
    "rust_1_54"
    "rust_1_55"
    "rust_1_56"
    "rust_1_57"
    "rust_1_58"
    "rust_1_59"
    "rust_1_60"
    "rust_1_61"
    "rust_1_62"
    "rust_1_63"
    "rust_1_64"
    "rust_1_65"
  ];

in

with lib;

listToAttrs (map (x:
  let
    rust = getAttr x pkgs;
    rustPackages = rust.packages.stable;

    meta = rustPackages.rustc.meta // {
      baseName = x;
      displayName = "Rust";
      icon = ./logo-64x64.png;
    };

  in {
    name = x;
    value = rec {
      packageOptions = rustPackages;
      packageSearch = common.searcher packageOptions;

      languageServerOptions = allLanguageServerOptions rust "rust";
      languageServerSearch = common.searcher languageServerOptions;

      build = args@{
        packages ? []
        , languageServers ? []
        , attrs ? ["rust"]
        , extensions ? ["rs" "rlib"]
        , metaOnly ? false
      }: symlinkJoin {
        name = "rust";
        paths = [
          (callPackage ./kernel.nix {
            inherit attrs extensions;
            evcxr = pkgs.evcxr.override {
              # rustPlatform = rustPackages.rustPlatform;
              # cargo = rustPackages.cargo;
            };
          })

          (callPackage ./mode_info.nix { inherit attrs extensions; })
        ]
        ++ (if metaOnly then [] else [
          rustPackages.rustc rustPackages.cargo pkgs.gcc
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
