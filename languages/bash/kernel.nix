{ callPackage, python3, bash }:

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
    language = "bash";
    logo32 = ./bash.png;
    logo64 = ./bash.png;
    metadata = {
      codedown = {
        priority = 10;
      };
    };
  };
}
