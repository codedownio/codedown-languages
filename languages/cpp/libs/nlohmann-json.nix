{ clangStdenv
, cmake
, fetchFromGitHub
}:

clangStdenv.mkDerivation {
  pname = "nlohmannJson";
  version = "3.11.2";

  src = fetchFromGitHub {
    owner = "nlohmann";
    repo = "json";
    rev = "bc889afb4c5bf1c0d8ee29ef35eaaf4c8bef8a5d";
    sha256 = "0g6rfsbkvrxmacchz4kbr741yybj7mls3r4hgyfdd3pdbqhn2is9";
  };

  nativeBuildInputs = [ cmake ];
}
