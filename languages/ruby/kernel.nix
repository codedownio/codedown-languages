{ callPackage
, attrs
}:

let
  common = callPackage ../common.nix {};

  iruby = (callPackage ./iruby {}).iruby;

in

common.makeJupyterKernel {
  ruby = {
    displayName = "Ruby";
    argv = [
      "${iruby}/bin/iruby"
      "kernel"
      "{connection_file}"
    ];
    language = "ruby";
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
