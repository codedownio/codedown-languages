{ lib
, callPackage
, python3
, bash

, attrs
, extensions
, metaOnly ? false
}:

let
  common = callPackage ../common.nix {};

  python = python3.withPackages (ps: [
    (ps.bash_kernel.override { inherit bash; })
  ]);

in

common.makeJupyterKernelInner metaOnly {
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
    logo32 = ./bash.png;
    logo64 = ./bash.png;
    metadata = {
      codedown = {
        inherit attrs extensions;
        language_version = bash.version;
        priority = 10;
      };
    };
  };
}
