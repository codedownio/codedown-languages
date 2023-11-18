{ fetchFromGitHub
, fetchzip
, lib
, stdenv

, ghc
, snapshot
}:

let
  hnlsSrc = fetchFromGitHub {
    owner = "codedownio";
    repo = "haskell-notebook-language-server";
    rev = "c5834f1336deca39f32eaf75c131108832739eb1";
    sha256 = "04h9v0cgibglfdzy90g4hpr9m0aprbdscnirk4k1jgdagvyiz7v0";
  };
  # hnlsSrc = /home/tom/tools/haskell-notebook-language-server;

  hnlsFromSource = snapshot.callPackage hnlsSrc {
    lsp-types = snapshot.callPackage ./lsp-types.nix {};
    myers-diff = snapshot.callPackage ./myers-diff.nix {};
    sandwich = null;
    sandwich-quickcheck = null;
  };

  ghcVersionToHnls = let
    prebuilt = src: stdenv.mkDerivation {
      pname = "haskell-notebook-language-server";
      version = "0.1.0.0";

      inherit src;

      installPhase = ''
      binary=$(find . -executable -type f | head -n 1)
      mkdir -p $out/bin
      cp "$binary" $out/bin/haskell-notebook-language-server
    '';
    };
  in
    {
      # "8.10.7" = prebuilt (fetchzip {
      #   url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.1.0.0/haskell-notebook-language-server-0.1.0.0-ghc8107-x86_64-linux.tar.gz";
      #   sha256 = "sha256-jRLBb1CKHVrlMyCljjyGNuEcwPxOKduIjMGbUSWq2jI=";
      # });
      # "9.0.2" = prebuilt (fetchzip {
      #   url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.1.0.0/haskell-notebook-language-server-0.1.0.0-ghc902-x86_64-linux.tar.gz";
      #   sha256 = "sha256-Eio2rE2wAlhZyGdEKgxlc8xoaIFrxmmJLS6ZQFExxSg=";
      # });
      # "9.2.8" = prebuilt (fetchzip {
      #   url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v0.1.0.0/haskell-notebook-language-server-0.1.0.0-ghc928-x86_64-linux.tar.gz";
      #   sha256 = "sha256-44g/JSih4dVSmrvMpJQviyLwWBUXBGqbYKyDhx2CL6I=";
      # });
    };

in

if lib.hasAttr ghc.version ghcVersionToHnls

then ghcVersionToHnls.${ghc.version}

else hnlsFromSource
