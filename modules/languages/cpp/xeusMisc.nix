{ callPackage
, blas
, clangStdenv
, cmake
, fetchFromGitHub
, gfortran
, liblapack
, openblas
, xtl
}:

let
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
    buildInputs = [ xtl ];
  };

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
    };

    nativeBuildInputs = [ cmake ];
    buildInputs = [ xtl xsimd ];
  };

  xtensorBlas = clangStdenv.mkDerivation {
    pname = "xtensor-blas";
    version = "0.20.0";

    src = fetchFromGitHub {
      owner = "xtensor-stack";
      repo = "xtensor-blas";
      rev = "66ab0fa7cd53d0b914f89d4d451576a9240ea457";
      sha256 = "0j4xvafxbxkb1gf966858kqkkcxkcwy8232xmq682gz8pld378c0";
    };

    nativeBuildInputs = [ cmake ];
    buildInputs = [ xtl xtensor openblas liblapack gfortran ];
  };

  liblapackShared = liblapack.override { shared = true; };
}
