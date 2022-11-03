{ lib
, callPackage

, attrs
, extensions
, metaOnly ? false
}:

with lib;

let
  common = callPackage ../common.nix {};

  iruby = (callPackage ./iruby {}).iruby;

in

common.makeJupyterKernelInner metaOnly (
  listToAttrs [{
    name = head attrs;
    value = {
      displayName = "Ruby";
      language = head attrs;
      argv = [
        "${iruby}/bin/iruby"
        "kernel"
        "{connection_file}"
      ];
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
