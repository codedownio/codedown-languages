{ lib
, callPackage

, iruby

, attrs
, extensions
, version
}:

with lib;

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernel (
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
      logo32 = ./iruby-32x32.png;
      logo64 = ./iruby-64x64.png;
      metadata = {
        codedown = {
          inherit attrs extensions;
          language_version = version;

          priority = 1;
        };
      };
    };
  }]
)
