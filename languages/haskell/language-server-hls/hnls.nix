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
    version = "0.4.2.1";
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
        hash = "sha256-G08P/WOg5EbyCRT98n6gdgjq0+DUW7Wbp3O4qNrlmsw=";
      });
      "ghc94" = prebuilt "ghc94" (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc94-x86_64-linux.tar.gz";
        hash = "sha256-gZvGh2Y8SdgO1JrcDQkLvkHzbFn86yQKVnNXXjy/2xo=";
      });
      "ghc96" = prebuilt "ghc96" (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc96-x86_64-linux.tar.gz";
        hash = "sha256-nDBz3Fzq4x47sX50VohqWcTjkXCQvZSMGaLu2nsWcJ8=";
      });
      "ghc98" = prebuilt "ghc98" (fetchzip {
        url = "https://github.com/codedownio/haskell-notebook-language-server/releases/download/v${version}/haskell-notebook-language-server-${version}-ghc98-x86_64-linux.tar.gz";
        hash = "sha256-qq5Iwoy8U0DNZBKPsUSsNwo8jNapxggUPBf/VuyKB5s=";
      });
    };

in

(builtins.getFlake "github:codedownio/haskell-notebook-language-server/1e6e7a49a8adc1d138a80270627469b07b257e62").packages.x86_64-linux.ghc96-static

# with lib.versions;
# # if majorMinor ghc.version == "8.10" then ghcVersionToHnls.ghc810
# # else if majorMinor ghc.version == "9.0" then ghcVersionToHnls.ghc90
# if majorMinor ghc.version == "9.2" then ghcVersionToHnls.ghc92
# else if majorMinor ghc.version == "9.4" then ghcVersionToHnls.ghc94
# else if majorMinor ghc.version == "9.6" then ghcVersionToHnls.ghc96
# else if majorMinor ghc.version == "9.8" then ghcVersionToHnls.ghc98
# else throw ("Unsupported GHC version: " + ghc.version)
