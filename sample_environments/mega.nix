{ codedown
, pkgsStable
, channels ? {}
, ...
}:


codedown.makeEnvironment channels {
  kernels.bash.enable = true;
  kernels.clojure.enable = true;
  # kernels.cpp11.enable = true;
  # kernels.cpp14.enable = true;
  # kernels.cpp17.enable = true;
  # kernels.cpp20.enable = true;
  # kernels.cpp23.enable = true;

  # kernels.julia.enable = true;
  # kernels.julia.packages = ["JSON3" "Plots"];
  # kernels.julia.settings.lsp.LanguageServer.enable = true;
  # kernels.julia.settings.lsp.LanguageServer.index = true;
  # kernels.julia.settings.lsp.LanguageServer.debug = false;

  # kernels.julia16.enable = true;
  # kernels.julia16.packages = ["JSON3" "Plots"];

  # kernels.haskell-ghc92.enable = true;
  # kernels.haskell-ghc92.packages = ["aeson"];
  # kernels.haskell-ghc92.settings.lsp.haskell-language-server.debug = true;

  # kernels.haskell-ghc94.enable = true;
  # kernels.haskell-ghc94.packages = ["aeson"];
  # kernels.haskell-ghc94.settings.lsp.haskell-language-server.debug = true;

  # kernels.haskell-ghc96.enable = true;
  # kernels.haskell-ghc96.packages = ["aeson"];
  # kernels.haskell-ghc96.settings.lsp.haskell-language-server.debug = true;

  # kernels.haskell-ghc98.enable = true;
  # kernels.haskell-ghc98.packages = ["aeson"];
  # kernels.haskell-ghc98.settings.lsp.haskell-language-server.debug = true;

  kernels.octave.enable = true;
  kernels.octave.packages = ["arduino"];
  kernels.octave.settings.extraJupyterConfig = ''
    c.OctaveKernel.plot_settings = dict(format='svg')
  '';

  kernels.R.enable = true;
  kernels.R.packages = ["ggplot2"];

  # kernels.python3.enable = true;
  # kernels.python3.packages = ["matplotlib" "scipy" "rope"];
  # kernels.python3.settings.permitUserSite = false;
  # kernels.python3.settings.lsp.jedi.enable = true;
  # kernels.python3.settings.lsp.pyright.enable = true;
  # kernels.python3.settings.lsp.pylint.enable = true;
  # kernels.python3.settings.lsp.flake8.enable = true;
  # kernels.python3.settings.lsp.pycodestyle.enable = true;
  # kernels.python3.settings.lsp.python-lsp-server.enable = true;

  # kernels.pypy3.enable = true;
  # kernels.pypy3.settings.permitUserSite = false;

  kernels.ruby.enable = true;

  # kernels.rust.enable = true;
  # kernels.rust.packages = ["rand"];

  kernels.go.enable = true;

  # kernels.postgres.enable = true;

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
