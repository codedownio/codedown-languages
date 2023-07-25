{ lib
, callPackage

, evcxr

, displayName
, attrs
, extensions
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
    value = {
      inherit displayName;
      argv = [
        "${evcxr}/bin/evcxr_jupyter"
        "--control_file"
        "{connection_file}"
      ];
      inherit language;
      logo32 = ./logo-32x32.png;
      logo64 = ./logo-64x64.png;
      metadata = {
        codedown = {
          inherit attrs extensions;
          priority = 1;
        };
      };
    };
  }]
)
