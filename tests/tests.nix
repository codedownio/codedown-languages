{ mkDerivation, aeson, aeson-qq, base, data-default, exceptions
, filepath, hpack, lib, lsp-test, optparse-applicative, sandwich
, string-interpolate, text, unliftio, unliftio-core
}:
mkDerivation {
  pname = "tests";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-qq base data-default exceptions filepath lsp-test
    optparse-applicative sandwich string-interpolate text unliftio
    unliftio-core
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-qq base data-default exceptions filepath lsp-test
    optparse-applicative sandwich string-interpolate text unliftio
    unliftio-core
  ];
  executableToolDepends = [ sandwich ];
  prePatch = "hpack";
  license = lib.licenses.bsd3;
}
