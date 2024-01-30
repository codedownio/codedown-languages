with import <nixpkgs> {};
with beamPackages;

buildMix {
  name = "IElixir";
  version = "0.9.21";

  src = fetchFromGitHub {
    owner = "pprzetacznik";
    repo = "IElixir";
    rev = "638c60eb4d639c89bc7c1768ab4256de35e2b034";
    sha256 = "1i97whvmqp7n2i2w7a8y797n0nfwwnhaskk2ncbyg6jcyqlslf5k";
  };

  beamDeps = [  ];

  meta = {
    description = "Jupyter's kernel for Elixir programming language";
    homepage = https://github.com/pprzetacznik/IElixir;
  };
}
