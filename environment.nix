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
      channel = "nixpkgs";
      language = "bashInteractive";
      args = {
        packages = [];
        languageServers = ["bashLanguageServer" "shellcheck"];
      };
    })

    # ({
    #   channel = "nixpkgs-unstable";
    #   language = "graphviz";
    #   args = {
    #     packages = [];
    #     languageServers = [];
    #   };
    # })

    ({
      channel = "nixpkgs";
      language = "clojure";
      args = {
        packages = [];
        languageServers = [];
      };
    })

    # ({
    #   channel = "nixpkgs-unstable";
    #   language = "cpp11";
    #   args = {
    #     packages = [];
    #     languageServers = [];
    #     attrs = ["cpp11" "cpp"];
    #   };
    # })

    # ({
    #   channel = "nixpkgs-unstable";
    #   language = "julia_15";
    #   args = {
    #     packages = [];
    #     languageServers = [];
    #   };
    # })

    ({
      channel = "nixpkgs";
      language = "haskell-ghc8107";
      args = {
        packages = ["aeson"];
        languageServers = ["haskell-language-server"];
      };
    })

    ({
      channel = "nixpkgs";
      language = "haskell-ghc902";
      args = {
        packages = ["aeson"];
        languageServers = ["haskell-language-server"];
      };
    })

    ({
      channel = "nixpkgs";
      language = "R";
      args = {
        packages = ["ggplot2"];
        languageServers = ["languageserver"];
      };
    })

    # ({
    #   channel = "nixpkgs-unstable";
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
      channel = "nixpkgs-unstable";
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
    #   channel = "nixpkgs-unstable";
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
    #   channel = "nixpkgs";
    #   language = "ruby_2_7";
    #   args = {
    #     packages = [];
    #     languageServers = [];
    #   };
    # })

    ({
      channel = "nixpkgs-unstable";
      language = "rust_1_60";
      args = {
        packages = [];
        languageServers = ["rust-analyzer"];
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
