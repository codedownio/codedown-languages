{ lib
, callPackage
, gophernotes

, attrs
, extensions
, version
}:

with lib;

let
  common = callPackage ../common.nix {};

  gophernotes' = gophernotes.overrideAttrs (_oldAttrs: {
    # The upstream tests give the kernel a fixed retry budget to come up on a ZMQ port, which
    # isn't long enough on an emulated or loaded builder. The go suite covers the kernel anyway.
    doCheck = false;
  });

  argv = [
    "${gophernotes'}/bin/gophernotes"
    "{connection_file}"
  ];

  # Go has no standard REPL, so the kernel itself is the interactive interpreter.
  repls.console = common.jupyterConsoleRepl {
    displayName = "Go";
    language = head attrs;
    inherit argv;
    icon = ./go-logo-64x64.png;
    iconMonochrome = ./go-monochrome.svg;
  };

in

(common.makeJupyterKernel {
  go = {
    displayName = "Go";
    inherit argv;
    language = head attrs;
    logo32 = ./go-logo-32x32.png;
    logo64 = ./go-logo-64x64.png;
    metadata = {
      codedown = {
        inherit attrs extensions;

        language_version = version;

        repls = common.replsToMetadata "go" repls;

        priority = 1;
      };
    };
  };
}).overrideAttrs (old: {
  passthru = (old.passthru or {}) // { inherit repls; };
})
