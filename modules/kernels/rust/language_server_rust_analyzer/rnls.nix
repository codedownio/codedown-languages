{ fetchzip
, stdenv
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
    name = "rust-notebook-language-server-0.2.12.0";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.12.0/rust-notebook-language-server-0.2.12.0-aarch64-linux.tar.gz";
    hash = "sha256-l32Oi6ncaE6At1uqKB3xS6am4ujDwxCcv8IYiVl7Jn8=";
  };
  "x86_64-linux" = fetchzip {
    name = "rust-notebook-language-server-0.2.12.0";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.12.0/rust-notebook-language-server-0.2.12.0-x86_64-linux.tar.gz";
    hash = "sha256-8BJgMUK2p/07Q7apOSa1es064f0GA2pN3qN+If5tiuk=";
  };
  "x86_64-darwin" = fetchzip {
    name = "rust-notebook-language-server-0.2.12.0";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.12.0/rust-notebook-language-server-0.2.12.0-x86_64-darwin.tar.gz";
    hash = "sha256-Gs6rI3N1pYzia8xBqg+Bkacw+NOog5SiVNuNngECm70=";
  };
  "aarch64-darwin" = fetchzip {
    name = "rust-notebook-language-server-0.2.12.0";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.12.0/rust-notebook-language-server-0.2.12.0-aarch64-darwin.tar.gz";
    hash = "sha256-cXvs4N+UujXJDMPBf/YSWultTc1o4gF3/PRKGC16MX0=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
