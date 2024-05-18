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
    version = "0.4.2.0";
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
      # "ghc810" = prebuilt (fetchzip {
      #   url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc810-x86_64-linux.tar.gz";
      #   hash = lib.fakeHash;
      # });
      # "ghc90" = prebuilt (fetchzip {
      #   url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc90-x86_64-linux.tar.gz";
      #   hash = lib.fakeHash;
      # });
      "ghc92" = prebuilt "ghc92" (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc92-x86_64-linux.tar.gz";
        hash = "sha256-q+4ZieWDxatxO7ei26UjGi8W+jatHiwVZSxqpu4UyAU=";
      });
      "ghc94" = prebuilt "ghc94" (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc94-x86_64-linux.tar.gz";
        hash = "sha256-sPHnQ7tbmI1b6Vl5GSMhOcd7PP+IY18QCZBbSJLNH5E=";
      });
      "ghc96" = prebuilt "ghc96" (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc96-x86_64-linux.tar.gz";
        hash = "sha256-1qnEjaWebho0t9RNXMtXHdO+H602Tb83qIoMHz5DMBs=";
      });
      "ghc98" = prebuilt "ghc98" (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc98-x86_64-linux.tar.gz";
        hash = "sha256-Cqr3X5zyVuzPZolaJTxlD6J5G0j1uht9SzISPQ6BRxc=";
      });
    };

in

/nix/store/2ggwlf7zwkccydblb2q7n358513y64g0-haskell-notebook-language-server-exe-haskell-notebook-language-server-x86_64-unknown-linux-musl-0.4.2.0

# (builtins.getFlake "/home/tom/tools/haskell-notebook-language-server").packages.x86_64-linux.ghc98-static

# with lib.versions;
# # if majorMinor ghc.version == "8.10" then ghcVersionToHnls.ghc810
# # else if majorMinor ghc.version == "9.0" then ghcVersionToHnls.ghc90
# if majorMinor ghc.version == "9.2" then ghcVersionToHnls.ghc92
# else if majorMinor ghc.version == "9.4" then ghcVersionToHnls.ghc94
# else if majorMinor ghc.version == "9.6" then ghcVersionToHnls.ghc96
# else if majorMinor ghc.version == "9.8" then ghcVersionToHnls.ghc98
# else throw ("Unsupported GHC version: " + ghc.version)
