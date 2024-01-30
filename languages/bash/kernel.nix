{ lib
, callPackage
, python3
, bash

, attrs
, extensions
}:

let
  common = callPackage ../common.nix {};

  python = python3.withPackages (ps: [
    (ps.bash_kernel.override { inherit bash; })
  ]);

in

common.makeJupyterKernel {
  bash = {
    displayName = "Bash";
    argv = [
      "${python}/bin/python"
      "-m"
      "bash_kernel"
      "-f"
      "{connection_file}"
    ];
    language = lib.head attrs;
    logo32 = ./bash-logo-128x128.png;
    logo64 = ./bash-logo-128x128.png;
    metadata = {
      codedown = {
        inherit attrs extensions;
        language_version = bash.version;
        priority = 10;
      };
    };
  };
}
