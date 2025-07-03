{ codedown
, ...
}:


codedown.makeEnvironment {
  name = "mega";

  ### Exporters ###

  exporters.nbconvert.enable = true;
  exporters.nbconvert.texliveScheme = "scheme-full";

  ### Kernels ###

  kernels.bash.enable = true;

  kernels.clojure.enable = true;

  kernels.cpp.enable = true;
  kernels.cpp.flavor = "c++14";

  kernels.julia.enable = true;
  kernels.julia.juliaPackage = "julia_110";
  kernels.julia.packages = ["JSON3" "Plots"];
  kernels.julia.lsp.LanguageServer.enable = true;
  kernels.julia.lsp.LanguageServer.index = true;
  kernels.julia.lsp.LanguageServer.debug = false;

  kernels.haskell.enable = true;
  kernels.haskell.packages = ["aeson"];
  kernels.haskell.lsp.haskell-language-server.debug = true;

  kernels.octave.enable = true;
  kernels.octave.packages = ["doctest"];
  kernels.octave.extraJupyterConfig = ''
    c.OctaveKernel.plot_settings = dict(format='svg')
  '';

  kernels.R.enable = true;
  kernels.R.packages = ["ggplot2"];

  kernels.python3.enable = true;
  kernels.python3.packages = ["matplotlib" "scipy" "rope"];
  kernels.python3.lsp.jedi.enable = true;
  kernels.python3.lsp.pyright.enable = true;
  kernels.python3.lsp.pylint.enable = true;
  kernels.python3.lsp.flake8.enable = true;
  kernels.python3.lsp.pycodestyle.enable = true;
  kernels.python3.lsp.python-lsp-server.enable = true;
  kernels.python3.misc.permitUserSite = false;

  # kernels.pypy3.enable = true;
  # kernels.pypy3.misc.permitUserSite = false;

  kernels.ruby.enable = true;

  kernels.rust.enable = true;
  kernels.rust.packages = ["rand"];

  kernels.go.enable = true;

  kernels.postgres.enable = true;

  kernels.coq.enable = true;

  ### Language servers ###

  language-servers.spellchecker.enable = true;

  ### Shells ###

  shells.bash.enable = true;
  shells.fish.enable = true;
  shells.zsh.enable = true;
}
