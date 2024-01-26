{ lib
, callPackage

, evcxr

, displayName
, attrs
, extensions
, version
, language ? lib.head attrs
, metaOnly ? false
}:

with lib;

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernelInner metaOnly (
  listToAttrs [{
    name = head attrs;
    version = evcxr.version;
    value = {
      inherit displayName;
      argv = [
        "${evcxr}/bin/evcxr_jupyter"
        "--control_file"
        "{connection_file}"
      ];
      inherit language;
      logo32 = ./rust-logo-32x32.png;
      logo64 = ./rust-logo-64x64.png;
      metadata = {
        codedown = {
          inherit attrs extensions;
          language_version = version;
          other_versions = {
            evcxr = evcxr.version;
          };
          priority = 1;
        };
      };
    };
  }]
)
