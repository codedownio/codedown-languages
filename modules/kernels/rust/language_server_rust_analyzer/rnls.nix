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
    name = "rust-notebook-language-server-0.2.6.1";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.6.1/rust-notebook-language-server-0.2.6.1-aarch64-linux.tar.gz";
    hash = "sha256-02aJ8pb5z59cExYxP8Lovlhch7kWRYoZF+UtRvxpFhQ=";
  };
  "x86_64-linux" = fetchzip {
    name = "rust-notebook-language-server-0.2.6.1";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.6.1/rust-notebook-language-server-0.2.6.1-x86_64-linux.tar.gz";
    hash = "sha256-f15DYM9CVssMY4fxSaVUJ61FhpLpTHo5Jor1dGp0Hi8=";
  };
  "x86_64-darwin" = fetchzip {
    name = "rust-notebook-language-server-0.2.6.1";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.6.1/rust-notebook-language-server-0.2.6.1-x86_64-darwin.tar.gz";
    hash = "sha256-/pJ2dHYxnvfUNc2IK/+ms594mI4sJ0FdZQNhj3b/64g=";
  };
  "aarch64-darwin" = fetchzip {
    name = "rust-notebook-language-server-0.2.6.1";
    stripRoot = false;
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.6.1/rust-notebook-language-server-0.2.6.1-aarch64-darwin.tar.gz";
    hash = "sha256-WZORteLQCpXHUHq+OwiNE4/IFAMKWg6YmEooXQvAgVE=";
  };
  # HASHES_END
}.${stdenv.hostPlatform.system}
