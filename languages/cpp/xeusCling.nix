{ clangStdenv
, fetchFromGitHub
, cmake
, zeromq
, libuuid
, cling
, pugixml
, gcc
, ncurses
, zlib
, openssl
, llvm_5
, llvmPackages_5
, git
}:

rec {
  xeus = clangStdenv.mkDerivation {
    pname = "xeus";
    version = "0.24.1";

    src = fetchFromGitHub {
      owner = "QuantStack";
      repo = "xeus";
      rev = "424b7cd177886906a59eee535b7de59088461910";
      sha256 = "0a3kalcz65mgjjqcri6rd0gb5fvd37b4d176dxi39i0w9z3iqbsg";
    };

    nativeBuildInputs = [ cmake ];
    buildInputs = [ zeromq cppzmq openssl xtl libuuid ];
    propagatedBuildInputs = [ nlohmannJson ];
  };

  xtl = clangStdenv.mkDerivation {
    pname = "xtl";
    version = "0.6.5";

    src = fetchFromGitHub {
      owner = "QuantStack";
      repo = "xtl";
      rev = "0d1f896ba90664248279baaea0059699cff5ab9b";
      sha256 = "1nsgm7kz3w34ksiaj2ixd9wl477c5lbxbd9cz89yyqvikp3p8pyr";
    };

    nativeBuildInputs = [ cmake ];
  };

  nlohmannJson = clangStdenv.mkDerivation {
    pname = "nlohmannJson";
    version = "3.6.1";

    src = fetchFromGitHub {
      owner = "nlohmann";
      repo = "json";
      rev = "1126c9ca74fdea22d2ce3a065ac0fcb5792cbdaf";
      sha256 = "1dgx3j9pb0f52dh73z8dpwdy79bra1qi5vpl66b9inq4gamf813z";
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

  cxxopts = clangStdenv.mkDerivation {
    pname = "cxxopts";
    version = "2.1.2";

    src = fetchFromGitHub {
      owner = "jarro2783";
      repo = "cxxopts";
      rev = "a0de9f3ba1035a3c4f5ffcd960cb94e4e12d40c5";
      sha256 = "0zsvb14gl2qv6y7mvqj6xc78czrma1hnwh548r204h7dnhyp5lyk";
    };

    nativeBuildInputs = [ cmake ];
  };

  xeusCling = clangStdenv.mkDerivation {
    pname = "xeusCling";
    version = "0.10.0";

    src = fetchFromGitHub {
      owner = "QuantStack";
      repo = "xeus-cling";
      rev = "f2df30e80ba2ca88ed8850231372e31ad5ec3ea6";
      sha256 = "0qs03815li31p9w2rmd1max7la24k4d7fcax76iafy8hdjgsnzfm";
    };

    patches = [
      ./xeus-cling.patch
    ];

    nativeBuildInputs = [ cmake ];
    buildInputs = [ zeromq cppzmq xeus libuuid xtl cling pugixml cxxopts ncurses zlib openssl llvm_5 ];
  };
}
