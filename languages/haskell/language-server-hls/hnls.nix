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
        sha256 = "1ci6zgxd4wqr7y6cw1kyd7pl8ssqxx9wlkj4a3rm3jrpwbiigbqg";
      });
      "v90" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc902-x86_64-linux.tar.gz";
        sha256 = "02pzx0zcfifngfah5dfw7k7wan3486gqcigd4a6xg2zfj794qsar";
      });
      "v92" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc928-x86_64-linux.tar.gz";
        sha256 = "sha256-A3ZZqkrpypcn4UhhqCjtSLfNj9wemH58OSpiknrRl0Q=";
      });
      "v94" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc948-x86_64-linux.tar.gz";
        sha256 = "sha256-Uj+V5HKA0QtRrc/jodMtmo+33TodX4LxuTwCRydAUME=";
      });
      "v96" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc964-x86_64-linux.tar.gz";
        sha256 = "sha256-ITQNjAv03Mpaq99Hct1idkQzNITidcdtWbG1MSYbM7w=";
      });
      "v98" = prebuilt (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc982-x86_64-linux.tar.gz";
        sha256 = "sha256-jwJd8IQHIeAI5igX0eilu2e0mDyGn0qPhBf9qnTtbro=";
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
