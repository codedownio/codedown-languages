{ lib
, callPackage
, typst

, attrs
, extensions
}:

let
  common = callPackage ../../kernels/common.nix {};

in

common.makeJupyterKernel {
  bash = {
    displayName = "Typst";
    argv = [
      "echo"
      "hi"
    ];
    language = lib.head attrs;
    logo32 = ./typst.png; # TODO: resize to proper size
    logo64 = ./typst.png; # TODO: resize to proper size
    metadata = {
      codedown = {
        inherit attrs extensions;
        language_version = typst.version;
        priority = 10;
      };
    };
  };
}
