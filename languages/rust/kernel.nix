{ callPackage
, evcxr
, attrs
}:

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernel {
  rust = {
    displayName = "Rust";
    argv = [
      "${evcxr}/bin/evcxr_jupyter"
      "--control_file"
      "{connection_file}"
    ];
    language = "rust";
    logo32 = ./logo-32x32.png;
    logo64 = ./logo-64x64.png;
    metadata = {
      codedown = {
        inherit attrs;
        priority = 1;
      };
    };
  };
}
