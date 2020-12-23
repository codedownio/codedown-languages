{ clangStdenv
, fetchFromGitHub
, cmake
, xtl
, blas
, openblas
, liblapack
, gfortran
}:

rec {
  xtensor = clangStdenv.mkDerivation {
    pname = "xtensor";
    version = "0.20.8";

    src = fetchFromGitHub {
      owner = "QuantStack";
      repo = "xtensor";
      rev = "b2d81961ac1b335bf2c362e6b7f792ebacb8abf8";
      sha256 = "06fyqfdrjmgqb83s7sahff74ld2mp9v168iciq0cr3fypg6kh54v";
    };

    nativeBuildInputs = [ cmake ];
    buildInputs = [ xtl ];
  };

  xtensorBlas = clangStdenv.mkDerivation {
    pname = "xtensor-blas";
    version = "0.16.1";

    src = fetchFromGitHub {
      owner = "QuantStack";
      repo = "xtensor-blas";
      rev = "1eb29c84608b60c21c50771bd2b02001d624cd2e";
      sha256 = "1yr821qnfqkqcv3416c9wcyhbcli635x3nwm04nc91jhwg4pmx2j";
    };

    nativeBuildInputs = [ cmake ];
    buildInputs = [ xtl xtensor openblas liblapack gfortran ];
  };

  liblapackShared = liblapack.override { shared = true; };
}
