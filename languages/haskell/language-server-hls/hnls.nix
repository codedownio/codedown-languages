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
        sha256 = "04lv7vkli9w87r5vcs85sl47i91rr0jxlj122sv7v8vh5wbjgz61";
      });
      "v90" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc902-x86_64-linux.tar.gz";
        sha256 = "08h3sc1b85pjwch2wmplljghfj6dq9slrarka8hrjn3mkhj476kq";
      });
      "v92" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc928-x86_64-linux.tar.gz";
        sha256 = "1rrfl255af7hf1rr0d56vfvcn3x02sq8rwyvkkqxqblkjyqqr0ii";
      });
      "v94" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc948-x86_64-linux.tar.gz";
        sha256 = "03sva25lq18x5zlhi8wggjfh7l3326g8an9v42qs16rcglyrgxsd";
      });
      "v96" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc964-x86_64-linux.tar.gz";
        sha256 = "1rzld09p9ybqg4ymsjh8n190jfgnb6787aadn3mc1qrf2s9vb7fq";
      });
      "v98" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc982-x86_64-linux.tar.gz";
        sha256 = "0r3rp30vv4l31fya7kmsad3wd4in6nj0mi9z345fhi0r92aq3r88";
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
