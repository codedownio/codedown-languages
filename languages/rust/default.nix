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

  # Note: update this when the base Nixpkgs is bumped
  baseCandidates = [
    "rust"
    "rust_1_60"
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
              rustPlatform = rustPackages.rustPlatform;
              cargo = rustPackages.cargo;
            };
            rustLibSrc = rustPackages.rustPlatform.rustLibSrc;
          })

          (callPackage ./mode_info.nix { inherit attrs extensions; })
        ]
        ++ (if metaOnly then [] else [
          rustPackages.rustc rustPackages.cargo pkgs.gcc
          (map (y: builtins.getAttr y (allLanguageServerOptions rust x)) languageServers)
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
