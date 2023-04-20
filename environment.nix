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
        languageServers = ["bash-language-server"];
      };
    })

    # ({
    #   name = "graphviz";
    #   channel = "codedown";
    #   args = {
    #     packages = [];
    #     languageServers = [];
    #   };
    # })

    ({
      name = "clojure";
      channel = "codedown";
      args = {
        packages = [];
        # languageServers = ["clojure-lsp"];
        languageServers = [];
      };
    })

    ({
      name = "cpp11";
      channel = "codedown";
      args = {
        packages = [];
        languageServers = [];
        attrs = ["cpp11" "cpp"];
      };
    })

    ({
      name = "julia";
      channel = "codedown";
      args = {
        packages = ["JSON3" "Plots"];
        languageServers = ["LanguageServer"];
        settings = {
            "LanguageServer.index" = true;
        };
      };
    })

    ({
      name = "julia16";
      channel = "codedown";
      args = {
        packages = ["JSON3" "Plots"];
        # languageServers = ["LanguageServer"];
        # settings = {
        #     "LanguageServer.index" = true;
        # };
      };
    })

    ({
      name = "haskell-ghc8107";
      channel = "codedown";
      args = {
        packages = ["aeson"];
        languageServers = ["haskell-language-server"];
      };
    })

    ({
      name = "haskell-ghc902";
      channel = "codedown";
      args = {
        packages = ["aeson"];
        languageServers = ["haskell-language-server"];
        settings = {
          "haskell-language-server.debug" = true;
        };
      };
    })

    ({
      name = "haskell-ghc924";
      channel = "codedown";
      args = {
        packages = ["aeson"];
        languageServers = ["haskell-language-server"];
      };
    })

    ({
      name = "R";
      channel = "codedown";
      args = {
        packages = ["ggplot2"];
        languageServers = ["languageserver"];
      };
    })

    ({
      name = "octave";
      channel = "codedown";
      args = {
        packages = ["arduino"];
        languageServers = [];
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
        languageServers = ["jedi" "pyright" "pylint" "flake8" "pycodestyle" "python-lsp-server"];
        settings = {
          permitUserSite = false;
        };
      };
    })

    # ({
    #   name = "pypy3";
    #   channel = "codedown";
    #   args = {
    #     packages = [];
    #     languageServers = [];
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
        languageServers = ["solargraph"];
      };
    })

    ({
      name = "rust";
      channel = "codedown";
      args = {
        packages = [];
        languageServers = ["rust-analyzer"];
      };
    })

    ({
      name = "go";
      channel = "codedown";
      args = {
        packages = [];
        languageServers = ["gopls"];
      };
    })

    ({
      name = "postgres";
      channel = "codedown";
      args = {
        packages = [];
        languageServers = [];
      };
    })

    ({
      name = "coq";
      channel = "codedown";
      args = {
        packages = ["ceres"];
        languageServers = [];
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
