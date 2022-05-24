{ callPackage
, evcxr
, attrs
, extensions
, metaOnly ? false
}:

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernelInner metaOnly {
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
        inherit attrs extensions;
        priority = 1;
      };
    };
  };
}
