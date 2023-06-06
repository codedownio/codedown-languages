{ lib
, callPackage
, runCommand
, makeWrapper

, evcxr
, rustLibSrc
, rustPackages
, vendoredPackages

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
        "RUST_VENDORED_PACKAGES" = vendoredPackages;
      };
    };
  }]
)
