{ codedown
, pkgsStable
, channels ? {}
, ...
}:


codedown.mkCodeDownEnvironment {
  inherit channels;

  kernels = [
    ({
      name = "bash";
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
      name = "cpp14";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp14" "cpp"];
      };
    })

    ({
      name = "cpp17";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp17" "cpp"];
      };
    })

    ({
      name = "cpp20";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp20" "cpp"];
      };
    })

    ({
      name = "cpp23";
      channel = "codedown";
      args = {
        packages = [];
        attrs = ["cpp23" "cpp"];
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

    # ({
    #   name = "haskell-ghc810";
    #   channel = "codedown";
    #   args = {
    #     packages = ["aeson"];
    #     settings = {
    #       "lsp.haskell-language-server.debug" = true;
    #     };
    #   };
    # })

    ({
      name = "haskell-ghc92";
      channel = "codedown";
      args = {
        packages = ["aeson"];
        settings = {
          "lsp.haskell-language-server.debug" = true;
        };
      };
    })

    ({
      name = "haskell-ghc94";
      channel = "codedown";
      args = {
        packages = ["aeson"];
        settings = {
          "lsp.haskell-language-server.enable" = true;
        };
      };
    })

    ({
      name = "haskell-ghc96";
      channel = "codedown";
      args = {
        packages = ["aeson"];
        settings = {
          "lsp.haskell-language-server.enable" = true;
        };
      };
    })

    ({
      name = "haskell-ghc98";
      channel = "codedown";
      args = {
        packages = ["aeson"];
        settings = {
          "lsp.haskell-language-server.debug" = true;
        };
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
      name = "R";
      channel = "codedown";
      args = {
        packages = ["ggplot2"];
      };
    })

    # ({
    #   name = "python3";
    #   channel = "codedown";
    #   args = {
    #     packages = ["matplotlib" "scipy" "rope"];
    #     settings = {
    #       permitUserSite = false;
    #       "lsp.jedi.enable" = true;
    #       "lsp.pyright.enable" = true;
    #       "lsp.pylint.enable" = true;
    #       "lsp.flake8.enable" = true;
    #       "lsp.pycodestyle.enable" = true;
    #       "lsp.python-lsp-server.enable" = true;
    #     };
    #   };
    # })

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
        packages = ["rand"];
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
        packages = [];
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

    { channel = "nixpkgs"; attr = "htop"; contents = pkgsStable.htop; }
  ];
}
