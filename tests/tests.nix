{ mkDerivation, aeson, base, bytestring, conduit, conduit-aeson
, containers, data-default, directory, exceptions, filepath, lens
, lib, lsp-test, lsp-types, monad-control, monad-logger, mtl
, optparse-applicative, postgresql-simple, row-types, safe
, sandwich, sandwich-contexts, string-interpolate, temporary, text
, unliftio, unliftio-core, vector
}:
mkDerivation {
  pname = "tests";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring conduit conduit-aeson containers data-default
    directory exceptions filepath lens lsp-test lsp-types monad-control
    monad-logger mtl optparse-applicative postgresql-simple row-types
    sandwich sandwich-contexts string-interpolate temporary text
    unliftio unliftio-core vector
  ];
  executableHaskellDepends = [
    aeson base containers exceptions filepath lens lsp-test lsp-types
    monad-control monad-logger mtl optparse-applicative row-types safe
    sandwich sandwich-contexts string-interpolate text unliftio
    unliftio-core vector
  ];
  executableToolDepends = [ sandwich ];
  license = lib.licenses.bsd3;
  mainProgram = "tests";
}
