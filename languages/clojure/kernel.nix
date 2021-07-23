{ callPackage }:

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernel {
  clojure = {
    displayName = "Clojure";
    argv = [
      "${callPackage ./clojupyter {}}"
      "{connection_file}"
    ];
    language = "clojure";
    logo32 = ./logo-32x32.png;
    logo64 = ./logo-64x64.png;
    metadata = {
      codedown = {
        priority = 1;
      };
    };
  };
}
