{ callPackage
, clangStdenv
, cmake
, fetchFromGitHub
, libuuid
, openssl
, zeromq
}:

let
  cppzmq = callPackage ./cppzmq.nix {};
  xeus = callPackage ./xeus.nix {};
  xtl = callPackage ./xtl.nix {};

in

clangStdenv.mkDerivation {
  pname = "xeus-zmq";
  version = "1.1.0";

  src = fetchFromGitHub {
    owner = "jupyter-xeus";
    repo = "xeus-zmq";
    rev = "8e0055035247c0c8c92d0c1830ceceefc5020c59";
    sha256 = "0m9g0fqm001d0jk91yk139d42azcinzhkqhaw7qhx3dh18zcsvcg";
  };

  nativeBuildInputs = [ cmake ];
  buildInputs = [ cppzmq openssl libuuid xeus xtl zeromq ];
  propagatedBuildInputs = [ (callPackage ./nlohmann-json.nix {}) ];
}
