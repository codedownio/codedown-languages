{ mkDerivation, aeson, aeson-qq, base, bytestring, containers
, data-default, directory, exceptions, filepath, hpack, lib
, lsp-test, monad-control, monad-logger, optparse-applicative
, sandwich, string-interpolate, text, unliftio, unliftio-core
, unordered-containers, vector
}:
mkDerivation {
  pname = "tests";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-qq base bytestring containers data-default directory
    exceptions filepath lsp-test monad-control monad-logger
    optparse-applicative sandwich string-interpolate text unliftio
    unliftio-core unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-qq base bytestring containers data-default directory
    exceptions filepath lsp-test monad-control monad-logger
    optparse-applicative sandwich string-interpolate text unliftio
    unliftio-core unordered-containers vector
  ];
  executableToolDepends = [ sandwich ];
  prePatch = "hpack";
  license = lib.licenses.bsd3;
}
