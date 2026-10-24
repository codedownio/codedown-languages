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
    name = "rust-notebook-language-server-0.2.9.0";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.9.0/rust-notebook-language-server-0.2.9.0-aarch64-linux.tar.gz";
    hash = "sha256-imdzMsCfoYFcxiKgF+bIY7L/dPpSszXMgdEMSFqDcG8=";
  };
  "x86_64-linux" = fetchzip {
    name = "rust-notebook-language-server-0.2.9.0";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.9.0/rust-notebook-language-server-0.2.9.0-x86_64-linux.tar.gz";
    hash = "sha256-Et2oh02MCLWWiAGNYpcrg8fvtWzMD4YXoU8bo/y+D7o=";
  };
  "x86_64-darwin" = fetchzip {
    name = "rust-notebook-language-server-0.2.9.0";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.9.0/rust-notebook-language-server-0.2.9.0-x86_64-darwin.tar.gz";
    hash = "sha256-gkCabCSyLmQYjupr9++93PBsn30hxDurVxaVnkwhWE4=";
  };
  "aarch64-darwin" = fetchzip {
    name = "rust-notebook-language-server-0.2.9.0";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.9.0/rust-notebook-language-server-0.2.9.0-aarch64-darwin.tar.gz";
    hash = "sha256-Cl7Dk1O8JehRRCdqCX2yNwGAHh7ztkR1Vf2AXtnRi/Q=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
