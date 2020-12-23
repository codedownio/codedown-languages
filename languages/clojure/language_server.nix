with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "clojure-lsp";

  src = fetchurl {
    url = https://github.com/snoe/clojure-lsp/releases/download/release-20190614T052638/clojure-lsp;
    sha256 = "079k4w8na8hhgrclcd2vkxiqv7a74hxy98mmckvbn3yxg68vx4fx";
  };

  unpackCmd = ''
      mkdir ./out
      cp $curSrc ./out/clojure-lsp
    '';

  buildPhase = "";

  buildInputs = [makeWrapper];

  installPhase = ''
      mkdir -p $out/bin
      cp clojure-lsp $out/bin/clojure-lsp-wrapped
      makeWrapper ${jdk}/bin/java $out/bin/clojure-lsp --add-flags "-jar $out/bin/clojure-lsp-wrapped"
    '';
}
