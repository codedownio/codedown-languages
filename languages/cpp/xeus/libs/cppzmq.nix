{ clangStdenv
, cmake
, fetchFromGitHub
, git
, zeromq
}:

clangStdenv.mkDerivation {
  pname = "cppzmq";
  version = "4.8.1";

  src = fetchFromGitHub {
    owner = "zeromq";
    repo = "cppzmq";
    rev = "dd663fafd830466d34cba278c2cfd0f92eb67614";
    sha256 = "0zzq20wzk5grshxfqhqgqqfwb38w3k83r821isvyaxghsglpwks3";
  };

  cmakeFlags = ["-DCPPZMQ_BUILD_TESTS=OFF"];

  nativeBuildInputs = [ cmake git ];
  buildInputs = [ zeromq ];
}
