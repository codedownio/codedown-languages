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
    name = "rust-notebook-language-server-0.2.8.0";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.8.0/rust-notebook-language-server-0.2.8.0-aarch64-linux.tar.gz";
    hash = "sha256-DF9kk2lXiUf3zRiZSgCxFz5evfoKZV40WohYiYmaGFg=";
  };
  "x86_64-linux" = fetchzip {
    name = "rust-notebook-language-server-0.2.8.0";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.8.0/rust-notebook-language-server-0.2.8.0-x86_64-linux.tar.gz";
    hash = "sha256-4YB3wjclpRJtYZC4XUxlReqjTJ+C32rDMGNpqX6gYGA=";
  };
  "x86_64-darwin" = fetchzip {
    name = "rust-notebook-language-server-0.2.8.0";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.8.0/rust-notebook-language-server-0.2.8.0-x86_64-darwin.tar.gz";
    hash = "sha256-XxvyldggrmzZBK2OtnkYpU1NUZAD8LB3QhI1tEaObzk=";
  };
  "aarch64-darwin" = fetchzip {
    name = "rust-notebook-language-server-0.2.8.0";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.8.0/rust-notebook-language-server-0.2.8.0-aarch64-darwin.tar.gz";
    hash = "sha256-1GJvb/JAtrguAxJEu0o1aawHQzesmc+Zc5gvOcfGgt0=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
