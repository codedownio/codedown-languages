{ callPackage
, makeWrapper
, stdenv
}:

snapshot: ghc: kernelName: focusedSettings: callPackage ./language-server-hls/config.nix {
  inherit kernelName;

  ghc = snapshot;

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
                  --set NIX_GHC_LIBDIR "${ghc.out}/lib/${ghc.meta.name}" \
                  --prefix PATH ':' ${ghc}/bin
    '';
    dontInstall = true;

    inherit (snapshot.haskell-language-server) meta;
  };
}