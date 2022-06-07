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

    ({
      channel = "nixpkgs-unstable";
      language = "cpp11";
      args = {
        packages = [];
        languageServers = [];
        attrs = ["cpp11" "cpp"];
      };
    })

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
      # language = "haskell-ghc8107";
      language = "haskell-ghc8107";
      args = {
        packages = ["aeson" "aeson-typescript"];
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
        languageServers = ["jedi" "pyright" "pylint" "flake8" "pycodestyle" "microsoft" "python-language-server"];
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
      language = "rust_1_52";
      args = {
        packages = [];
        languageServers = ["rust-analyzer"];
      };
    })
  ];

  otherPackages = [
    { channel = "nixpkgs"; attr = "codedown.spellchecker"; contents = codedown.spellchecker; }

    { channel = "nixpkgs"; attr = "codedown.shells.zsh"; contents = codedown.shells.zsh; }
    { channel = "nixpkgs"; attr = "codedown.shells.fish"; contents = codedown.shells.fish; }
    { channel = "nixpkgs"; attr = "codedown.shells.bash"; contents = codedown.shells.bash; }

    { channel = "nixpkgs"; attr = "codedown.exporters.nbconvert-small"; contents = codedown.exporters.nbconvert-small; }
    { channel = "nixpkgs"; attr = "codedown.exporters.nbconvert-large"; contents = codedown.exporters.nbconvert-large; }
  ];
}
