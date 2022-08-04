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
      channel = "codedown";
      language = "bashInteractive";
      args = {
        packages = [];
        languageServers = ["bashLanguageServer" "shellcheck"];
      };
    })

    # ({
    #   channel = "codedown";
    #   language = "graphviz";
    #   args = {
    #     packages = [];
    #     languageServers = [];
    #   };
    # })

    ({
      channel = "codedown";
      language = "clojure";
      args = {
        packages = [];
        languageServers = [];
      };
    })

    # ({
    #   channel = "codedown";
    #   language = "cpp11";
    #   args = {
    #     packages = [];
    #     languageServers = [];
    #     attrs = ["cpp11" "cpp"];
    #   };
    # })

    # ({
    #   channel = "codedown";
    #   language = "julia-stable-bin";
    #   args = {
    #     packages = [];
    #     languageServers = [];
    #   };
    # })

    ({
      channel = "codedown";
      language = "haskell-ghc8107";
      args = {
        packages = ["aeson"];
        languageServers = ["haskell-language-server"];
      };
    })

    ({
      channel = "codedown";
      language = "haskell-ghc902";
      args = {
        packages = ["aeson"];
        languageServers = ["haskell-language-server"];
      };
    })

    # ({
    #   channel = "codedown";
    #   language = "haskell-ghc922";
    #   args = {
    #     packages = ["aeson"];
    #     languageServers = ["haskell-language-server"];
    #   };
    # })

    ({
      channel = "codedown";
      language = "R";
      args = {
        packages = ["ggplot2"];
        languageServers = ["languageserver"];
      };
    })

    # ({
    #   channel = "codedown";
    #   language = "octave";
    #   args = {
    #     packages = ["arduino"];
    #     languageServers = [];
    #     extraJupyterConfig = ''
    #       c.OctaveKernel.plot_settings = dict(format='svg')
    #     '';
    #   };
    # })

    ({
      channel = "codedown";
      language = "python38";
      args = {
        packages = ["matplotlib" "scipy" "rope"];
        languageServers = ["jedi" "pyright" "pylint" "flake8" "pycodestyle" "microsoft" "python-lsp-server"];
        settings = {
          permitUserSite = false;
        };
      };
    })

    # ({
    #   channel = "codedown";
    #   language = "pypy27";
    #   args = {
    #     packages = [];
    #     languageServers = [];
    #     settings = {
    #       permitUserSite = false;
    #     };
    #   };
    # })

    # ({
    #   channel = "codedown";
    #   language = "ruby_2_7";
    #   args = {
    #     packages = [];
    #     languageServers = [];
    #   };
    # })

    ({
      channel = "codedown";
      language = "rust_1_60";
      args = {
        packages = [];
        languageServers = ["rust-analyzer"];
      };
    })

    ({
      channel = "codedown";
      language = "go";
      args = {
        packages = [];
        languageServers = ["go-langserver"];
      };
    })

    ({
      channel = "codedown";
      language = "postgres";
      args = {
        packages = [];
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
