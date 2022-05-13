{ mkDerivation, aeson, aeson-qq, base, data-default, hpack, lib
, lsp-test, optparse-applicative, string-interpolate
}:
mkDerivation {
  pname = "tests";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-qq base data-default lsp-test optparse-applicative
    string-interpolate
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson aeson-qq base data-default lsp-test optparse-applicative
    string-interpolate
  ];
  testHaskellDepends = [
    aeson aeson-qq base data-default lsp-test optparse-applicative
    string-interpolate
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/tests#readme";
  license = lib.licenses.bsd3;
}
