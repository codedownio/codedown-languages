{ callPackage
, attrs
, extensions
, clojupyter
}:

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernel {
  clojure = {
    displayName = "Clojure";
    argv = [
      "${clojupyter.launcher}/bin/clojupyter"
      "{connection_file}"
    ];
    language = "clojure";
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
