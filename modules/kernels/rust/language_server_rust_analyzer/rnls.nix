{ fetchzip
, stdenv
, system
}:

# Build from source method. Blows up the closure!
# let
#   # src = fetchFromGitHub {
#   #   owner = "codedownio";
#   #   repo = "rust-notebook-language-server";
#   #   rev = "13d268df7cee4cafb4c95917473dfc4e76d93746";
#   #   sha256 = "0yc1shq8y78kpyiq4zci48nlsqw4291qim9fsrvj218mh9nx9y2w";
#   # };
#   src = /home/tom/tools/rust-notebook-language-server;
#   ghc = haskell.packages.ghc924;
# in ghc.callPackage src {
#   lsp-types = ghc.callPackage ./lsp-types.nix {};
#   myers-diff = ghc.callPackage ./myers-diff.nix {};
# }

# Fetch a static binary, only ~5MB
{
  # HASHES_START
  "aarch64-linux" = fetchzip {
    name = "rust-notebook-language-server-0.2.4.1";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.4.1/rust-notebook-language-server-0.2.4.1-aarch64-linux.tar.gz";
    hash = "sha256-cueCWa2AgDxxgPgEYDj9m9qJA58Kh8rLsB7UR/TfDM8=";
  };
  "x86_64-linux" = fetchzip {
    name = "rust-notebook-language-server-0.2.4.1";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.4.1/rust-notebook-language-server-0.2.4.1-x86_64-linux.tar.gz";
    hash = "sha256-1a9qcTKIYNOcGSo5rh+LnjDj1nb/N84Vyn3NMujfjw0=";
  };
  "x86_64-darwin" = fetchzip {
    name = "rust-notebook-language-server-0.2.4.1";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.4.1/rust-notebook-language-server-0.2.4.1-x86_64-darwin.tar.gz";
    hash = "sha256-xRilifTxdhOP8eep2Lf/dEKhb1nlrEHbh0O8zPU/S/8=";
  };
  "aarch64-darwin" = fetchzip {
    name = "rust-notebook-language-server-0.2.4.1";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.4.1/rust-notebook-language-server-0.2.4.1-aarch64-darwin.tar.gz";
    hash = "sha256-Krsoh3WLG8TkXKTEtFNCThbCc1UQjlziiDngOgvOtF4=";
  };
  # HASHES_END
}.${system}
