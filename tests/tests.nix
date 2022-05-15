{ mkDerivation, aeson, aeson-qq, base, bytestring, data-default
, exceptions, filepath, hpack, lib, lsp-test, optparse-applicative
, sandwich, string-interpolate, text, unliftio, unliftio-core
, unordered-containers
}:
mkDerivation {
  pname = "tests";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-qq base bytestring data-default exceptions filepath
    lsp-test optparse-applicative sandwich string-interpolate text
    unliftio unliftio-core unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-qq base bytestring data-default exceptions filepath
    lsp-test optparse-applicative sandwich string-interpolate text
    unliftio unliftio-core unordered-containers
  ];
  executableToolDepends = [ sandwich ];
  prePatch = "hpack";
  license = lib.licenses.bsd3;
}
