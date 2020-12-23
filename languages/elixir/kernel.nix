with import <nixpkgs> {};
with beamPackages;

buildMix {
  name = "IElixir";
  version = "0.9.21";

  src = fetchFromGitHub {
    owner = "pprzetacznik";
    repo = "IElixir";
    rev = "638c60eb4d639c89bc7c1768ab4256de35e2b034";
    sha256 = "0mar83vrq77092riz2f64z984x384g7zfbjjf76fqy70lr4x0jwv";
  };

  beamDeps = [  ];

  meta = {
    description = "Jupyter's kernel for Elixir programming language";
    homepage = https://github.com/pprzetacznik/IElixir;
  };
}
