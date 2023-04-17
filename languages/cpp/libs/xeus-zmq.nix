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
  version = "1.0.2";

  src = fetchFromGitHub {
    owner = "jupyter-xeus";
    repo = "xeus-zmq";
    rev = "51d1c3d8c69d38d8be3a8c56748504306bf72796";
    sha256 = "1xb90jlrk19zqdbin847lwf2b9p4r9llc63x8843cbmciq27yjp9";
  };

  nativeBuildInputs = [ cmake ];
  buildInputs = [ cppzmq openssl libuuid xeus xtl zeromq ];
  propagatedBuildInputs = [ (callPackage ./nlohmann-json.nix {}) ];
}
