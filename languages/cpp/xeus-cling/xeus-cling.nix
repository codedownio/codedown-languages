{ lib
, callPackage
, cmake
, fetchFromGitHub
, gcc
, git

, clangStdenv
, llvmPackages_9

, argparse
, cling
, cppzmq
, libuuid
, ncurses
, openssl
, pugixml
, xeus
, xeus-zmq
, xtl
, zeromq
, zlib

, debug ? false
}:

clangStdenv.mkDerivation rec {
  pname = "xeus-cling";
  version = "0.15.3";

  src = fetchFromGitHub {
    owner = "QuantStack";
    repo = "xeus-cling";
    rev = "${version}";
    hash = "sha256-OfZU+z+p3/a36GntusBfwfFu3ssJW4Fu7SV3SMCoo1I=";
  };

  patches = [
    ./0001-Fix-bug-in-extract_filename.patch
    ./0002-Don-t-pass-extra-includes-configure-this-with-flags.patch
  ];

  nativeBuildInputs = [ cmake ];
  buildInputs = [
    argparse
    cling.unwrapped
    cppzmq
    libuuid
    llvmPackages_9.llvm
    ncurses
    openssl
    pugixml
    xeus
    xeus-zmq
    xtl
    zeromq
    zlib
  ];

  cmakeFlags = lib.optionals debug [
    "-DCMAKE_BUILD_TYPE=Debug"
  ];

  dontStrip = debug;

  meta = {
    description = "Jupyter kernel for the C++ programming language";
    homepage = "https://github.com/jupyter-xeus/xeus-cling";
    maintainers = with lib.maintainers; [ thomasjm ];
    platforms = lib.platforms.unix;
    license = lib.licenses.mit;
  };
}
