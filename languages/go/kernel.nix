{ callPackage
, attrs
, extensions
, gophernotes
}:

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernel {
  go = {
    displayName = "Go";
    argv = [
      "${gophernotes}/bin/gophernotes"
      "{connection_file}"
    ];
    language = "go";
    logo32 = ./logo-32x32.png;
    logo64 = ./logo-64x64.png;
    metadata = {
      codedown = {
        inherit attrs extensions;
        priority = 1;
      };
    };
  };
}
