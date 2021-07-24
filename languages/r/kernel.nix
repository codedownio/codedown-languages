{ callPackage
, rWithPackages
, attrs
}:

let
  common = callPackage ../common.nix {};

in

common.makeJupyterKernel {
  r = {
    displayName = "R";
    argv = [
      "${rWithPackages}/bin/R"
      "--slave"
      "-e"
      "IRkernel::main()"
      "--args"
      "{connection_file}"
    ];
    language = "r";
    logo32 = null;
    logo64 = ./logo-64x64.png;
    metadata = {
      codedown = {
        inherit attrs;
        priority = 1;
      };
    };
  };
}
