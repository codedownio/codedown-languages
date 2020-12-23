let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with beamPackages;

buildMix {
  name = "IElixir";
  version = "0.9.14";

  src = fetchFromGitHub {
    owner = "pprzetacznik";
    repo = "IElixir";
    rev = "8053ae32528f540188b5ba9e090464cd68480ce4";
    sha256 = "1mar83vrq77092riz2f64z984x384g7zfbjjf76fqy70lr4x0jwv";
  };

  beamDeps = [ plug absinthe ];

  meta = {
    description = "Jupyter's kernel for Elixir programming language";
    homepage = https://github.com/pprzetacznik/IElixir;
  };
}
