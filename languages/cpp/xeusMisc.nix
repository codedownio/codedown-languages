{ callPackage
, clangStdenv
, fetchFromGitHub
, cmake
, xtl
, blas
, openblas
, liblapack
, gfortran
}:

let
  xtl = callPackage ./libs/xtl.nix {};

in

rec {
  xtensor = clangStdenv.mkDerivation {
    pname = "xtensor";
    version = "0.24.6";

    src = fetchFromGitHub {
      owner = "QuantStack";
      repo = "xtensor";
      rev = "e534928cc30eb3a4a05539747d98e1d6868c2d62";
      sha256 = "0gf5m5p61981pv7yh5425lcv8dci948ri37hn1zlli7xg54x0g3i";
      # date = "2023-03-20T10:44:36+01:00";
    };

    nativeBuildInputs = [ cmake ];
    buildInputs = [ xtl xsimd ];
  };

  xsimd = clangStdenv.mkDerivation {
    pname = "xsimd";
    version = "0.10.0";

    src = fetchFromGitHub {
      owner = "xtensor-stack";
      repo = "xsimd";
      rev = "e12bf0a928bd6668ff701db55803a9e316cb386c";
      sha256 = "0da9m25l2r177r1zlj5k721v0573irpcxl298db37bmyqxnhmv7r";
    };

    nativeBuildInputs = [ cmake ];
    buildInputs = [ xtl xsimd ];
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
