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
    version = "0.3.1.0";
    prebuilt = src: stdenv.mkDerivation {
      pname = "haskell-notebook-language-server";
      inherit version;

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
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc8107-x86_64-linux.tar.gz";
        sha256 = "14xl74v9yxy0j83xblmvrlynjr651d23fjlw1sw00aw9fxxi77xx";
      });
      "v90" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc902-x86_64-linux.tar.gz";
        sha256 = "1nkynldgmmbgpsklqgd0rxzb1cbp0vz1j9q2js3p1azj1givbbh9";
      });
      "v92" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc928-x86_64-linux.tar.gz";
        sha256 = "051k2jmikyrk4j7nv7r6r0mrvnb92qy6j5k2kkksyqzalrjaiwxg";
      });
      "v94" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc948-x86_64-linux.tar.gz";
        sha256 = "04s68hf6j4gsgpv3wn5cjjdld4vnwvbfranamadvdpbcc3kamqym";
      });
      "v96" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc964-x86_64-linux.tar.gz";
        sha256 = "1q1m5kx4rh5gb1pibpc6n69pwa34gsm6qd3k7dl7nz1n1z81n4vx";
      });
      "v98" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc982-x86_64-linux.tar.gz";
        sha256 = "0a2ckf85nrzq0v0ap6590ksx37dghlaixsvpmibvjsk5wp9m14as";
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
