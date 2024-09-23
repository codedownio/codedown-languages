{ callPackage
, lib
, makeWrapper
, stdenv

, ncurses
}:

let
  util = import ../util.nix;

in

snapshot: ghc: kernelName: focusedSettings: callPackage ./config.nix {
  inherit kernelName;

  inherit ghc snapshot;

  settings = focusedSettings;

  haskell-language-server = stdenv.mkDerivation {
    pname = "haskell-language-server-wrapped";
    version = snapshot.haskell-language-server.version;

    buildInputs = [makeWrapper];

    dontUnpack = true;
    dontConfigure = true;
    buildPhase = ''
      mkdir -p $out/bin
      makeWrapper ${ghc}/bin/haskell-language-server $out/bin/haskell-language-server \
                  --set NIX_GHC_LIBDIR "${util.getLibDir ghc}" \
                  --suffix LD_LIBRARY_PATH : "${lib.makeLibraryPath [ncurses]}" \
                  --prefix PATH ':' ${ghc}/bin
    '';
    dontInstall = true;

    inherit (snapshot.haskell-language-server) meta;
  };
}
