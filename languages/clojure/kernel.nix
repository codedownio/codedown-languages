{ callPackage
, attrs
, extensions
, version
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
    logo32 = ./clojure-logo-32x32.png;
    logo64 = ./clojure-logo-64x64.png;
    metadata = {
      codedown = {
        inherit attrs extensions;
        language_version = version;
        priority = 1;
      };
    };
  };
}
