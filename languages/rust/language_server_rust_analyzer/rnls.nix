{ fetchzip
, stdenv
, haskell
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
stdenv.mkDerivation {
  pname = "rust-notebook-language-server";
  version = "0.2.1.0";

  src = fetchzip {
    url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.1.0/rust-notebook-language-server-0.2.1.0-x86_64-linux.tar.gz";
    sha256 = "sha256-yV6aRtWh8Hg9bh6Mg7izDE2mICGWAkER6d/Q462/7Nk=";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp rust-notebook-language-server-0.2.1.0-x86_64-linux $out/bin/rust-notebook-language-server
  '';
}
