{ stdenv
, fetchFromGitHub
, nix_2_3
}:

# let

# src = fetchFromGitHub {
#   owner = "codedownio";
#   repo = "haskell-notebook-language-server";
#   rev = "5efcef24ba7b84a0776cc28c2ff3e4f1cc60dc28";
#   sha256 = "sha256-jhR1TmEB8i/ahNGrPgRezNgTAcQqkOOnVTVNGaNV0Gk=";
# };

# in

# (import src).haskell-notebook-language-server.components.exes.haskell-notebook-language-server

stdenv.mkDerivation {
  pname = "haskell-notebook-language-server";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "codedownio";
    repo = "haskell-notebook-language-server";
    rev = "5efcef24ba7b84a0776cc28c2ff3e4f1cc60dc28";
    sha256 = "sha256-jhR1TmEB8i/ahNGrPgRezNgTAcQqkOOnVTVNGaNV0Gk=";
  };

  buildInputs = [nix_2_3];

  dontConfigure = true;
  dontInstall = true;

  buildPhase = ''
    mkdir -p $out/bin

    nix-build -A haskell-notebook-language-server.components.exes.haskell-notebook-language-server -o $out/bin/haskell-notebook-language-server
  '';
}
