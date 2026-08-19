{ lib
, callPackage
, fetchFromGitHub
, gophernotes

, attrs
, extensions
, version
}:

with lib;

let
  common = callPackage ../common.nix {};

  gophernotesPatched = gophernotes.overrideAttrs (_oldAttrs: {
    src = fetchFromGitHub {
      owner = "codedownio";
      repo = "gophernotes";
      rev = "6b18077f97aa913b73093beeb2152b2d51ee64af";
      hash = "sha256-gSD2zUWka3cur5jkv4siYp2gJdxD+00bmJi6BZd0c+c="; # nixpkgs-hash
    };

    vendorHash = "sha256-bGaXnd0E6dRNiwvGIn7Ptddrt7dRzPfkPThgHPuL2Vo=";
  });

  argv = [
    "${gophernotesPatched}/bin/gophernotes"
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
