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
    version = "0.4.0.0";
    prebuilt = ghcName: src: stdenv.mkDerivation {
      pname = "haskell-notebook-language-server-" + ghcName;
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
      # "ghc8107" = prebuilt (fetchzip {
      #   url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc8107-x86_64-linux.tar.gz";
      #   hash = lib.fakeHash;
      # });
      # "ghc902" = prebuilt (fetchzip {
      #   url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc902-x86_64-linux.tar.gz";
      #   hash = lib.fakeHash;
      # });
      "ghc928" = prebuilt "ghc928" (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc928-x86_64-linux.tar.gz";
        hash = "sha256-KkQZfEn3ZPQOtwCuveQHyhRtRR8tbJ9Q+H/SBildNOI=";
      });
      "ghc948" = prebuilt "ghc948" (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc948-x86_64-linux.tar.gz";
        hash = "sha256-DUKMuKqwoRb1FkEzr8eEUEjgweFOe1q8ZJJgCIG74EA=";
      });
      "ghc964" = prebuilt "ghc964" (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc964-x86_64-linux.tar.gz";
        hash = "sha256-fXcp1JoXFpW4I4YnPL7demmXRCDJ4p/E6qfnBZUs2JE=";
      });
      "ghc982" = prebuilt "ghc982" (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc982-x86_64-linux.tar.gz";
        hash = "sha256-BVnNc/cLUIKMrHNHsWvGPIFT+U1U7pIoPrK3e+e4QEY=";
      });
    };

in

with lib.versions;
# if majorMinor ghc.version == "8.10" then ghcVersionToHnls.ghc8107
# else if majorMinor ghc.version == "9.0" then ghcVersionToHnls.ghc902
if majorMinor ghc.version == "9.2" then ghcVersionToHnls.ghc928
else if majorMinor ghc.version == "9.4" then ghcVersionToHnls.ghc948
else if majorMinor ghc.version == "9.6" then ghcVersionToHnls.ghc964
else if majorMinor ghc.version == "9.8" then ghcVersionToHnls.ghc982
else throw ("Unsupported GHC version: " + ghc.version)
