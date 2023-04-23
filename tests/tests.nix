{ mkDerivation, aeson, base, bytestring, conduit, conduit-aeson
, containers, data-default, directory, exceptions, filepath
, hashable, lens, lib, lsp-test, lsp-types, monad-control
, monad-logger, mtl, network, optparse-applicative
, postgresql-libpq, postgresql-simple, process, random
, resource-pool, retry, safe, sandwich, string-interpolate
, temporary, text, time, unliftio, unliftio-core, vector
}:
mkDerivation {
  pname = "tests";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring conduit conduit-aeson containers data-default
    directory exceptions filepath hashable lens lsp-test lsp-types
    monad-control monad-logger mtl network postgresql-libpq
    postgresql-simple process random resource-pool retry safe sandwich
    string-interpolate temporary text time unliftio unliftio-core
    vector
  ];
  executableHaskellDepends = [
    aeson base bytestring containers exceptions hashable lens lsp-test
    lsp-types monad-control optparse-applicative sandwich
    string-interpolate text unliftio-core vector
  ];
  executableToolDepends = [ sandwich ];
  license = lib.licenses.bsd3;
  mainProgram = "tests-exe";
}
