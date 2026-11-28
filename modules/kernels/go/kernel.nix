{ lib
, callPackage
, stdenv
, buildGo126Module
, gophernotes

, displayName
, attrs
, extensions
, version
}:

with lib;

let
  common = callPackage ../common.nix {};

  # gomacro can't handle *types.Alias, and its go.mod turns them off with gotypesalias=0, a
  # setting Go 1.27 removed.
  gophernotesGo126 = gophernotes.override { buildGoModule = buildGo126Module; };

  # The upstream tests give the kernel a fixed one second to come up on a ZMQ port, which isn't
  # long enough under the QEMU emulation our aarch64-linux builds run in; the connect fails with
  # ECONNREFUSED. The go suite covers the kernel anyway.
  gophernotes' = if stdenv.hostPlatform.system == "aarch64-linux"
                 then gophernotesGo126.overrideAttrs (_oldAttrs: { doCheck = false; })
                 else gophernotesGo126;

  argv = [
    "${gophernotes'}/bin/gophernotes"
    "{connection_file}"
  ];

  # Go has no standard REPL, so the kernel itself is the interactive interpreter.
  repls.console = common.jupyterConsoleRepl {
    inherit displayName;
    language = head attrs;
    inherit argv;
    icon = ./go-logo-64x64.png;
    iconMonochrome = ./go-monochrome.svg;
  };

in

(common.makeJupyterKernel {
  go = {
    inherit displayName;
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
