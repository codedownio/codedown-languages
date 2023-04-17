{ lib
, callPackage
, clangStdenv
, fetchFromGitHub
, cmake
, zeromq
, libuuid
, cling
, gcc
, ncurses
, zlib
, openssl
, llvm_9
, git
}:

let
  xtl = callPackage ./libs/xtl.nix {};

in

rec {
  xeus-zmq = clangStdenv.mkDerivation {
    pname = "xeus-zmq";
    version = "1.0.2";

    src = fetchFromGitHub {
      owner = "jupyter-xeus";
      repo = "xeus-zmq";
      rev = "51d1c3d8c69d38d8be3a8c56748504306bf72796";
      sha256 = "1xb90jlrk19zqdbin847lwf2b9p4r9llc63x8843cbmciq27yjp9";
    };

    nativeBuildInputs = [ cmake ];
    buildInputs = [ zeromq cppzmq openssl xtl libuuid ];
    propagatedBuildInputs = [ nlohmannJson ];
  };

  nlohmannJson = clangStdenv.mkDerivation {
    pname = "nlohmannJson";
    version = "3.11.2";

    src = fetchFromGitHub {
      owner = "nlohmann";
      repo = "json";
      rev = "bc889afb4c5bf1c0d8ee29ef35eaaf4c8bef8a5d";
      sha256 = "0g6rfsbkvrxmacchz4kbr741yybj7mls3r4hgyfdd3pdbqhn2is9";
    };

    nativeBuildInputs = [ cmake ];
  };

  cppzmq = clangStdenv.mkDerivation {
    pname = "cppzmq";
    version = "4.4.1";

    src = fetchFromGitHub {
      owner = "zeromq";
      repo = "cppzmq";
      rev = "f5b36e563598d48fcc0d82e589d3596afef945ae";
      sha256 = "15dgkv51csfkafplf0n0vqbjdr4pxqxq44dln0dcizhsn1p0a57q";
    };

    cmakeFlags = ["-DCPPZMQ_BUILD_TESTS=OFF"];

    nativeBuildInputs = [ cmake git ];
    buildInputs = [ zeromq ];
  };

  cpp-argparse = clangStdenv.mkDerivation {
    pname = "cpp-argparse";
    version = "2.9";

    src = fetchFromGitHub {
      owner = "p-ranav";
      repo = "argparse";
      rev = "997da9255618311d1fcb0135ce86022729d1f1cb";
      sha256 = "1wdpy45qcipfyw9bbr9s42v67b88bkyniy76yvh0grp2wf8zidxx";
    };

    postPatch = ''
      substituteInPlace CMakeLists.txt \
        --replace '$'{CMAKE_INSTALL_LIBDIR_ARCHIND} '$'{CMAKE_INSTALL_LIBDIR}
      substituteInPlace packaging/pkgconfig.pc.in \
        --replace '$'{prefix}/@CMAKE_INSTALL_INCLUDEDIR@ @CMAKE_INSTALL_FULL_INCLUDEDIR@
    '';

    nativeBuildInputs = [ cmake ];

    meta = with lib; {
      description = "Argument Parser for Modern C++";
      homepage = "https://github.com/p-ranav/argparse";
      platforms = platforms.unix;
      license = licenses.mit;
    };
  };

  pugixml = clangStdenv.mkDerivation {
    pname = "pugixml";
    version = "1.8.1";

    src = fetchFromGitHub {
      owner = "zeux";
      repo = "pugixml";
      rev = "d2deb420bc70369faa12785df2b5dd4d390e523d";
      sha256 = "1zi1qp0gj40dhvxpgjgr3zwb8cdqf0kq1gnhxy4bl8b7b0vx7f1d";
    };

    nativeBuildInputs = [ cmake ];
  };

  xeusCling = clangStdenv.mkDerivation {
    pname = "xeusCling";
    version = "0.15.1";

    src = fetchFromGitHub {
      owner = "QuantStack";
      repo = "xeus-cling";
      rev = "c088a4a2181c1aa1b7c5b71b4107a32ff00d56f9";
      sha256 = "19dr7xxk3w61frh1qkpnswqk1ccwbqqhj75ryjan1n8gb7b11jc9";
    };

    patches = [
      ./xeus-cling.patch
    ];

    nativeBuildInputs = [ cmake ];
    buildInputs = [ zeromq cppzmq xeus-zmq libuuid xtl cling pugixml cpp-argparse ncurses zlib openssl llvm_9 ];
  };
}
