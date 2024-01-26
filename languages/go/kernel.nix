{ lib
, callPackage
, gophernotes

, metaOnly

, attrs
, extensions
, version
}:

with lib;

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernelInner metaOnly {
  go = {
    displayName = "Go";
    argv = [
      "${gophernotes}/bin/gophernotes"
      "{connection_file}"
    ];
    language = head attrs;
    logo32 = ./go-logo-32x32.png;
    logo64 = ./go-logo-64x64.png;
    metadata = {
      codedown = {
        inherit attrs extensions;

        language_version = version;

        priority = 1;
      };
    };
  };
}
