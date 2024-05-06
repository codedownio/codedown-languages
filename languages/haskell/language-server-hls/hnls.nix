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
      # "ghc8107" = prebuilt (fetchzip {
      #   url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.1.0/haskell-notebook-language-server-0.3.1.0-ghc8107-x86_64-linux.tar.gz";
      #   sha256 = lib.fakeHash;
      # });
      # "ghc902" = prebuilt (fetchzip {
      #   url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.1.0/haskell-notebook-language-server-0.3.1.0-ghc902-x86_64-linux.tar.gz";
      #   sha256 = lib.fakeHash;
      # });
      "ghc928" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.1.0/haskell-notebook-language-server-0.3.1.0-ghc928-x86_64-linux.tar.gz";
        sha256 = "sha256-uD7Pz9sMy0Hy7v0qG43C8GHt7A/57T53XoKvOFWmAn0=";
      });
      "ghc948" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.1.0/haskell-notebook-language-server-0.3.1.0-ghc948-x86_64-linux.tar.gz";
        hash = "sha256-NEz3AmGXfHFgC5Puc9wxqX8t2h0qe+VSapzT4krEKzU=";
      });
      "ghc964" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.1.0/haskell-notebook-language-server-0.3.1.0-ghc964-x86_64-linux.tar.gz";
        sha256 = "sha256-LUUh4w/tHFJjBUwWQ5LmtWxQbiQR1KeyQjWwSl0ApCI=";
      });
      "ghc982" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.1.0/haskell-notebook-language-server-0.3.1.0-ghc982-x86_64-linux.tar.gz";
        sha256 = "sha256-RABRHAvEkkW7GBuZ7MqxbldQ7TMLueugmjR82pt0q0k=";
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
