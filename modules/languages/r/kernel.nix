{ callPackage
, rWithPackages

# , lib
# , coreutils
# , which

, attrs
, extensions
, version
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
    logo64 = ./r-logo-64x64.png;

    # The kernel calls "which uname"
    # https://github.com/codedownio/codedown-languages/issues/63
    # env = {
    #   PATH = lib.makeBinPath [coreutils which];
    # };

    metadata = {
      codedown = {
        inherit attrs extensions;

        language_version = version;

        priority = 1;
      };
    };
  };
}
