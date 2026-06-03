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

in

common.makeJupyterKernel {
  go = {
    displayName = "Go";
    argv = [
      "${gophernotesPatched}/bin/gophernotes"
      "{connection_file}"
    ];
    language = head attrs;
    logo32 = ./go-logo-32x32.png;
    logo64 = ./go-logo-64x64.png;
    metadata = {
      codedown = {
        inherit attrs extensions;

        language_version = version;

        priority = 1;
      };
    };
  };
}
