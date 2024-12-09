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

let
  version = import ./rnls-version.nix;
in

# Fetch a static binary, only ~5MB
stdenv.mkDerivation {
  pname = "rust-notebook-language-server";
  inherit version;

  src = {
    # HASHES_START
    "x86_64-linux" = fetchzip {
      url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.3.0/rust-notebook-language-server-0.2.3.0-x86_64-linux.tar.gz";
      hash = "sha256-IaNP7VEmuYc6B3axhHN8/hfJPKKUtq1klgrjlZZqEVU=";
    };
    "x86_64-darwin" = fetchzip {
      url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.3.0/rust-notebook-language-server-0.2.3.0-x86_64-darwin.tar.gz";
      hash = "sha256-fNIzPC5vaYtizFxBYCvyCGlDWozi5oAARBio3bGztBk=";
    };
    "aarch64-darwin" = fetchzip {
      url = "https://github.com/codedownio/rust-notebook-language-server/releases/download/v0.2.3.0/rust-notebook-language-server-0.2.3.0-aarch64-darwin.tar.gz";
      hash = "sha256-KJrH2lmc7nFCTC4zxAJa9ntZW2kobZDUfcI4/yLYbqY=";
    };
    # HASHES_END
  }.${system};

  installPhase = ''
    mkdir -p $out/bin
    cp rust-notebook-language-server-${version}-${system} $out/bin/rust-notebook-language-server
  '';
}
