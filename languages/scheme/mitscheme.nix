let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;

stdenv.mkDerivation rec {
  name = "mit-scheme-10.1.5";

  src = fetchTarball {
    url = https://ftp.gnu.org/gnu/mit-scheme/stable.pkg/10.1.5/mit-scheme-10.1.5-x86-64.tar.gz;
    sha256 = "12cxzj4hab4hlwyygiqd2q022sca9qcfg3yflqbm3a20r4xhhbhj";
  };

  buildInputs = [gcc m4];

  configurePhase = ''
    cd src;
    mkdir -p $out
    ./configure --prefix=$out
  '';

  buildPhase = ''
    make compile-microcode
  '';

  installPhase = ''
    make install
  '';
}
