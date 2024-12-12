{ stdenv, makeWrapper, man, man-pages-posix }:

stdenv.mkDerivation {
  name = "man-with-pages";

  unpackPhase = "true";

  buildInputs = [makeWrapper man man-pages-posix]; # man-pages
  propagatedBuildInputs = [makeWrapper man man-pages-posix]; # man-pages

  buildPhase = ''
    mkdir -p $out/bin
    makeWrapper ${man}/bin/man $out/bin/man --set MANPATH "${man-pages-posix}/share/man"
  '';

  installPhase = "true";
}
