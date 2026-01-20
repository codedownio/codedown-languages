{ lib
, stdenv
, fetchFromGitHub
, cmake
, cling
, zlib
, ncurses
}:

stdenv.mkDerivation rec {
  pname = "cling-parser";
  version = import ./cnls-version.nix;

  src = fetchFromGitHub {
    owner = "codedownio";
    repo = "cpp-notebook-language-server";
    rev = "v${version}";
    hash = "sha256-6dKv2PMqam8R196IAEjh2uRROQLGOYKw4n/gztmmr6A=";
  };

  sourceRoot = "${src.name}/cling-parser";

  nativeBuildInputs = [ cmake ];

  buildInputs = [ cling.unwrapped zlib ncurses ];

  cmakeFlags = [
    "-DLLVM_CONFIG=${cling.unwrapped}/bin/llvm-config"
  ];

  meta = with lib; {
    description = "Minimal Cling parser for cpp-notebook-language-server";
    homepage = "https://github.com/codedownio/cpp-notebook-language-server";
    license = licenses.bsd3;
    platforms = platforms.all;
  };
}
