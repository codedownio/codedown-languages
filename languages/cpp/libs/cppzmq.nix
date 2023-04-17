{ clangStdenv
, cmake
, fetchFromGitHub
, git
, zeromq
}:

clangStdenv.mkDerivation {
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
}
