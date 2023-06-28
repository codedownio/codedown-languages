{ lib
, callPackage
, runCommand
, makeWrapper

, evcxr
, rustLibSrc
, rustPackages
, vendoredPackages

, packages
, displayName
, attrs
, extensions
, metaOnly ? false
}:

with lib;

let
  common = callPackage ../common.nix {};

  evcxrWrapped = runCommand "evcxr-${evcxr.version}-wrapped" { buildInputs = [makeWrapper]; } ''
    mkdir -p $out/bin
    makeWrapper ${evcxr}/bin/evcxr_jupyter $out/bin/evcxr_jupyter \
      --suffix PATH : ${rustPackages.rustc}/bin
  '';

  withPackages = (callPackage ./withPackages.nix {
    inherit (rustPackages) cargo rustPlatform;
  });

  evcxrConfigDir = withPackages.evcxrConfigDir packages;

  cargoHome = runCommand "cargo-home" {} ''
    # Set up local index
    mkdir -p $out/index_base
    ln -s ${withPackages.cratesIndex} $out/index_base/index

    # cat <<EOT >> $out/config.toml
    # [source.crates-io]
    # local-registry = "$out/index_base"
    # directory = "${withPackages.vendorDependencies packages}"
    # EOT

    cat <<EOT >> $out/config.toml
    [source.crates-io]
    replace-with = "vendored-sources"

    [source.vendored-sources]
    local-registry = "$out/index_base"
    EOT
  '';

in

common.makeJupyterKernelInner metaOnly (
  listToAttrs [{
    name = head attrs;
    value = {
      inherit displayName;
      argv = [
        "${evcxrWrapped}/bin/evcxr_jupyter"
        "--control_file"
        "{connection_file}"
      ];
      language = lib.head attrs;
      logo32 = ./logo-32x32.png;
      logo64 = ./logo-64x64.png;
      metadata = {
        codedown = {
          inherit attrs extensions;
          priority = 1;
        };
      };
      env = {
        "EVCXR_CONFIG_DIR" = evcxrConfigDir;
        "CARGO_HOME" = cargoHome;
      };
    };
  }]
)
