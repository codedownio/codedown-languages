{ lib
, callPackage
, stdenv
, gophernotes

, attrs
, extensions
, version
}:

with lib;

let
  common = callPackage ../common.nix {};

  # The upstream tests give the kernel a fixed one second to come up on a ZMQ port, which isn't
  # long enough under the QEMU emulation our aarch64-linux builds run in; the connect fails with
  # ECONNREFUSED. Everywhere else leave the derivation exactly as nixpkgs ships it, so it can be
  # substituted from cache.nixos.org rather than rebuilt. The go suite covers the kernel anyway.
  gophernotes' = if stdenv.hostPlatform.system == "aarch64-linux"
                 then gophernotes.overrideAttrs (_oldAttrs: { doCheck = false; })
                 else gophernotes;

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
