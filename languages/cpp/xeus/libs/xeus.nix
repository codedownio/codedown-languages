{ callPackage
, clangStdenv
, cmake
, fetchFromGitHub
, libuuid
, openssl
, zeromq
}:

clangStdenv.mkDerivation {
  pname = "xeus";
  version = "3.0.5";

  src = fetchFromGitHub {
    owner = "jupyter-xeus";
    repo = "xeus";
    rev = "e884e1255d8381980e35fce650daf8a1e5705ebe";
    sha256 = "099vwnw2dmmq97v4zjy2clvg027n4zrx7ahjj6mg2aalkly3ir9d";
  };

  nativeBuildInputs = [ cmake ];
  buildInputs = [
    zeromq
    (callPackage ./cppzmq.nix {})
    openssl
    (callPackage ./xtl.nix {})
    libuuid
  ];
  propagatedBuildInputs = [
    (callPackage ./nlohmann-json.nix {})
  ];
}
