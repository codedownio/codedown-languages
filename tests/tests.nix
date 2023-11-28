{ mkDerivation, aeson, base, bytestring, conduit, conduit-aeson
, containers, data-default, directory, ex-pool, exceptions
, filepath, lens, lib, lsp-test, lsp-types, monad-control
, monad-logger, mtl, network, optparse-applicative
, postgresql-libpq, postgresql-simple, process, random, retry
, row-types, safe, sandwich, string-interpolate, temporary, text
, time, unliftio, unliftio-core, vector
}:
mkDerivation {
  pname = "tests";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring conduit conduit-aeson containers data-default
    directory ex-pool exceptions filepath lens lsp-test lsp-types
    monad-control monad-logger mtl network postgresql-libpq
    postgresql-simple process random retry row-types safe sandwich
    string-interpolate temporary text time unliftio unliftio-core
    vector
  ];
  executableHaskellDepends = [
    aeson base containers exceptions lens lsp-test lsp-types
    monad-control monad-logger optparse-applicative safe sandwich
    string-interpolate text vector
  ];
  executableToolDepends = [ sandwich ];
  license = lib.licenses.bsd3;
  mainProgram = "tests-exe";
}
