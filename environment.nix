{ codedown
, channels
, overlays
, ...
}:


codedown.mkCodeDownEnvironment {
  inherit channels overlays;
  # metaOnly = true;

  kernels = [
    ({
      name = "bashInteractive";
      channel = "codedown";
      args = {
        packages = [];
      };
    })

    # ({
    #   name = "graphviz";
    #   channel = "codedown";
    #   args = {
    #     packages = [];
    #   };
    # })

    ({
      name = "clojure";
      channel = "codedown";
      args = {
        packages = [];
      };
    })

    ({
      name = "cpp11";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp11" "cpp"];
      };
    })

    ({
      name = "julia";
      channel = "codedown";
      args = {
        packages = ["JSON3" "Plots"];
        settings = {
          "lsp.LanguageServer.enable" = true;
          "lsp.LanguageServer.index" = true;
          "lsp.LanguageServer.debug" = false;
        };
      };
    })

    ({
      name = "julia16";
      channel = "codedown";
      args = {
        packages = ["JSON3" "Plots"];
      };
    })

    ({
      name = "haskell-ghc8107";
      channel = "codedown";
      args = {
        packages = ["aeson"];
      };
    })

    ({
      name = "haskell-ghc902";
      channel = "codedown";
      args = {
        packages = ["aeson"];
        settings = {
          "lsp.haskell-language-server.debug" = true;
        };
      };
    })

    ({
      name = "haskell-ghc924";
      channel = "codedown";
      args = {
        packages = ["aeson"];
      };
    })

    ({
      name = "R";
      channel = "codedown";
      args = {
        packages = ["ggplot2"];
      };
    })

    ({
      name = "octave";
      channel = "codedown";
      args = {
        packages = ["arduino"];
        extraJupyterConfig = ''
          c.OctaveKernel.plot_settings = dict(format='svg')
        '';
      };
    })

    ({
      name = "python3";
      channel = "codedown";
      args = {
        packages = ["matplotlib" "scipy" "rope"];
        settings = {
          permitUserSite = false;
          "lsp.jedi.enable" = true;
          "lsp.pyright.enable" = true;
          "lsp.pylint.enable" = true;
          "lsp.flake8.enable" = true;
          "lsp.pycodestyle.enable" = true;
          "lsp.python-lsp-server.enable" = true;
        };
      };
    })

    # ({
    #   name = "pypy3";
    #   channel = "codedown";
    #   args = {
    #     packages = [];
    #     settings = {
    #       permitUserSite = false;
    #     };
    #   };
    # })

    ({
      name = "ruby";
      channel = "codedown";
      args = {
        packages = [];
      };
    })

    ({
      name = "rust";
      channel = "codedown";
      args = {
        packages = [];
      };
    })

    ({
      name = "go";
      channel = "codedown";
      args = {
        packages = [];
      };
    })

    ({
      name = "postgres";
      channel = "codedown";
      args = {
        packages = [];
      };
    })

    ({
      name = "coq";
      channel = "codedown";
      args = {
        packages = ["ceres"];
      };
    })
  ];

  otherPackages = [
    { channel = "codedown"; attr = "spellchecker"; contents = codedown.spellchecker; }

    { channel = "codedown"; attr = "shells.zsh"; contents = codedown.shells.zsh; }
    { channel = "codedown"; attr = "shells.fish"; contents = codedown.shells.fish; }
    { channel = "codedown"; attr = "shells.bash"; contents = codedown.shells.bash; }

    { channel = "codedown"; attr = "exporters.nbconvert-small"; contents = codedown.exporters.nbconvert-small; }
    { channel = "codedown"; attr = "exporters.nbconvert-large"; contents = codedown.exporters.nbconvert-large; }
  ];
}
