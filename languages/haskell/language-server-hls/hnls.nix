{ fetchFromGitHub
, fetchzip
, lib
, stdenv

, ghc
, snapshot
}:

let
  # hnlsSrc = fetchFromGitHub {
  #   owner = "codedownio";
  #   repo = "haskell-notebook-language-server";
  #   rev = "66ba3c72badd59aa28bad8a32580808fdab7e8e1";
  #   sha256 = "sha256-PDYgUllJovVXMLRPZ8ps9MnAgGpEgM7do4+fMPtXqbE=";
  # };

  # hnlsSrc = /home/tom/tools/haskell-notebook-language-server;

  # hnlsFromSource = snapshot.callPackage hnlsSrc {
  #   lsp-types = snapshot.callPackage ./lsp-types.nix {};
  #   myers-diff = snapshot.callPackage ./myers-diff.nix {};
  #   sandwich = null;
  #   sandwich-quickcheck = null;
  # };

  ghcVersionToHnls = let
    prebuilt = src: stdenv.mkDerivation {
      pname = "haskell-notebook-language-server";
      version = "0.3.0.0";

      inherit src;

      installPhase = ''
      binary=$(find . -executable -type f | head -n 1)
      mkdir -p $out/bin
      cp "$binary" $out/bin/haskell-notebook-language-server
    '';
    };
  in
    {
      "v810" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.0.0/haskell-notebook-language-server-0.3.0.0-ghc8107-x86_64-linux.tar.gz";
        sha256 = "sha256-jRLBb1CKHVrlMyCljjyGNuEcwPxOKduIjMGbUSWq2jI=";
      });
      "v90" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.0.0/haskell-notebook-language-server-0.3.0.0-ghc902-x86_64-linux.tar.gz";
        sha256 = "sha256-Eio2rE2wAlhZyGdEKgxlc8xoaIFrxmmJLS6ZQFExxSg=";
      });
      "v92" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.0.0/haskell-notebook-language-server-0.3.0.0-ghc928-x86_64-linux.tar.gz";
        sha256 = "sha256-44g/JSih4dVSmrvMpJQviyLwWBUXBGqbYKyDhx2CL6I=";
      });
      "v94" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.0.0/haskell-notebook-language-server-0.3.0.0-ghc948-x86_64-linux.tar.gz";
        sha256 = "sha256-44g/JSih4dVSmrvMpJQviyLwWBUXBGqbYKyDhx2CL6I=";
      });
      "v96" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.0.0/haskell-notebook-language-server-0.3.0.0-ghc964-x86_64-linux.tar.gz";
        sha256 = "sha256-44g/JSih4dVSmrvMpJQviyLwWBUXBGqbYKyDhx2CL6I=";
      });
      "v98" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.0.0/haskell-notebook-language-server-0.3.0.0-ghc982-x86_64-linux.tar.gz";
        sha256 = "sha256-44g/JSih4dVSmrvMpJQviyLwWBUXBGqbYKyDhx2CL6I=";
      });
    };

in

with lib.versions;

if majorMinor ghc.version == "8.10" then ghcVersionToHnls.v810
else if majorMinor ghc.version == "9.0" then ghcVersionToHnls.v90
else if majorMinor ghc.version == "9.2" then ghcVersionToHnls.v92
else if majorMinor ghc.version == "9.4" then ghcVersionToHnls.v94
else if majorMinor ghc.version == "9.6" then ghcVersionToHnls.v96
else if majorMinor ghc.version == "9.8" then ghcVersionToHnls.v98
else throw ("Unsupported GHC version: " + ghc.version)
