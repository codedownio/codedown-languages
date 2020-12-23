let

  nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {};

in

with nixpkgs;

# Note: this is still not working. Filed a PR to attempt to get help here:
# https://github.com/NixOS/nixpkgs/pull/103560
stdenv.mkDerivation rec {
  pname = "clojupyter";
  version = "0.3.2";

  outputHash = "1yzdakgsgvvpcqhanixn502ysxv0alyidsmzcssv971bz20ii9na";
  outputHashAlgo = "sha256";
  outputHashMode = "recursive";

  src = fetchFromGitHub {
    owner = "clojupyter";
    repo = "clojupyter";
    rev = "49b9a41baf7ab284944892ea40939e5e53e6c38e";
    sha256 = "1wphc7h74qlm9bcv5f95qhq1rq9gmcm5hvjblb01vffx996vr6jz";
  };

  configurePhase = "export HOME=$TMP";

  buildInputs = [ (leiningen.override { jdk = jdk11; }) openjdk11 git clojure ];

  buildPhase = ''
    lein uberjar
  '';

  installPhase = ''
    mkdir -p $out/share/java
    cp target/${pname}-${version}-standalone.jar $out/share/java

    mkdir -p $out/bin
    makeWrapper ${jre}/bin/java $out/bin/clojupyter \
      --add-flags "-cp $out/share/java/clojupyter-${version}-standalone.jar clojupyter.kernel.core"
  '';

  # doCheck = false;

  meta = {
    description = "A Jupyter kernel for Clojure";
    homepage = https://github.com/clojupyter/clojupyter;
  };
}
