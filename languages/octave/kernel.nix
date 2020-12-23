let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with python3Packages;

let
  metakernel = import ./metakernel.nix;
  octaveWithBinaries = import ./octave.nix;

in

buildPythonPackage rec {
  pname = "octave_kernel";
  version = "0.30.3";

  src = fetchPypi {
    inherit pname version;
    sha256 = "1kl5saaicjbdqq4zksh82ia6mwb81al167n4ai1d23qrav5gcyfi";
  };

  buildInputs = [ jupyter_client metakernel ipykernel ghostscript fontconfig ];
  propagatedBuildInputs = [ metakernel ipykernel octaveWithBinaries ];

  # Tests failing because jupyter_kernel_test not available
  doCheck = false;

  meta = {
    description = "A Jupyter kernel for Octave.";
    homepage = https://github.com/Calysto/octave_kernel;
  };
}
