with import <nixpkgs> {};

rec {
  manWithPages = stdenv.mkDerivation {
    name = "man-with-pages";

    unpackPhase = "true";

    buildInputs = [makeWrapper man posix_man_pages]; # man-pages
    propagatedBuildInputs = [makeWrapper man posix_man_pages]; # man-pages

    buildPhase = ''
      mkdir -p $out/bin
      makeWrapper ${man}/bin/man $out/bin/man --set MANPATH "${posix_man_pages}/share/man"
    '';

    installPhase = "true";
  };
}
