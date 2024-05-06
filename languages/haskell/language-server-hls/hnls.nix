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
      "ghc8107" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.1.0/haskell-notebook-language-server-0.3.1.0-ghc8107-x86_64-linux.tar.gz";
        sha256 = "04lv7vkli9w87r5vcs85sl47i91rr0jxlj122sv7v8vh5wbjgz61";
      });
      "ghc902" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.1.0/haskell-notebook-language-server-0.3.1.0-ghc902-x86_64-linux.tar.gz";
        sha256 = "08h3sc1b85pjwch2wmplljghfj6dq9slrarka8hrjn3mkhj476kq";
      });
      "ghc928" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.1.0/haskell-notebook-language-server-0.3.1.0-ghc928-x86_64-linux.tar.gz";
        sha256 = "1rrfl255af7hf1rr0d56vfvcn3x02sq8rwyvkkqxqblkjyqqr0ii";
      });
      "ghc948" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.1.0/haskell-notebook-language-server-0.3.1.0-ghc948-x86_64-linux.tar.gz";
        sha256 = "03sva25lq18x5zlhi8wggjfh7l3326g8an9v42qs16rcglyrgxsd";
      });
      "ghc964" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.1.0/haskell-notebook-language-server-0.3.1.0-ghc964-x86_64-linux.tar.gz";
        sha256 = "1rzld09p9ybqg4ymsjh8n190jfgnb6787aadn3mc1qrf2s9vb7fq";
      });
      "ghc982" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.3.1.0/haskell-notebook-language-server-0.3.1.0-ghc982-x86_64-linux.tar.gz";
        sha256 = "0r3rp30vv4l31fya7kmsad3wd4in6nj0mi9z345fhi0r92aq3r88";
      });
    };

in

with lib.versions;

if majorMinor ghc.version == "8.10" then ghcVersionToHnls.ghc8107
else if majorMinor ghc.version == "9.0" then ghcVersionToHnls.ghc902
else if majorMinor ghc.version == "9.2" then ghcVersionToHnls.ghc928
else if majorMinor ghc.version == "9.4" then ghcVersionToHnls.ghc948
else if majorMinor ghc.version == "9.6" then ghcVersionToHnls.ghc964
else if majorMinor ghc.version == "9.8" then ghcVersionToHnls.ghc982
else throw ("Unsupported GHC version: " + ghc.version)
