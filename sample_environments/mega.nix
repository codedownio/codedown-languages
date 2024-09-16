{ codedown
, pkgsStable
, channels ? {}
, ...
}:


codedown.makeEnvironment channels {
  kernels.bash.enable = true;

  kernels.clojure.enable = true;

  kernels.cpp.enable = true;
  kernels.cpp.flavor = "c++14";

  kernels.julia.enable = true;
  kernels.julia.packages = ["JSON3" "Plots"];
  kernels.julia.settings.lsp.LanguageServer.enable = true;
  kernels.julia.settings.lsp.LanguageServer.index = true;
  kernels.julia.settings.lsp.LanguageServer.debug = false;

  kernels.haskell.enable = true;
  kernels.haskell.packages = ["aeson"];
  kernels.haskell.settings.lsp.haskell-language-server.debug = true;

  kernels.octave.enable = true;
  kernels.octave.packages = ["arduino"];
  kernels.octave.settings.extraJupyterConfig = ''
    c.OctaveKernel.plot_settings = dict(format='svg')
  '';

  kernels.R.enable = true;
  kernels.R.packages = ["ggplot2"];

  kernels.python3.enable = true;
  kernels.python3.packages = ["matplotlib" "scipy" "rope"];
  kernels.python3.settings.permitUserSite = false;
  kernels.python3.settings.lsp.jedi.enable = true;
  kernels.python3.settings.lsp.pyright.enable = true;
  kernels.python3.settings.lsp.pylint.enable = true;
  kernels.python3.settings.lsp.flake8.enable = true;
  kernels.python3.settings.lsp.pycodestyle.enable = true;
  kernels.python3.settings.lsp.python-lsp-server.enable = true;

  # kernels.pypy3.enable = true;
  # kernels.pypy3.settings.permitUserSite = false;

  kernels.ruby.enable = true;

  kernels.rust.enable = true;
  kernels.rust.packages = ["rand"];

  kernels.go.enable = true;

  kernels.postgres.enable = true;

  kernels.coq.enable = true;

  shells.bash.enable = true;
  shells.fish.enable = true;
  shells.zsh.enable = true;

  exporters.nbconvert-exporters.enable = true;
  exporters.nbconvert-exporters.texliveScheme = "scheme-full";

  # otherPackages = [
  #   { channel = "codedown"; attr = "spellchecker"; contents = codedown.spellchecker; }

  #   { channel = "nixpkgs"; attr = "htop"; contents = pkgsStable.htop; }
  # ];
}
