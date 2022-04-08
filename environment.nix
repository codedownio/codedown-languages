{ channels
, importedChannels
, overlays
, importedOverlays
}:


importedChannels.nixpkgs.codedown.mkCodeDownEnvironment {
  inherit channels importedChannels overlays;

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

    ({
      channel = "nixpkgs-unstable";
      language = "octave";
      args = {
        packages = ["arduino"];
        languageServers = [];
        extraJupyterConfig = ''
          c.OctaveKernel.plot_settings = dict(format='svg')
        '';
      };
    })

    ({
      channel = "nixpkgs-unstable";
      language = "python38";
      args = {
        packages = ["matplotlib" "scipy" "rope"];
        languageServers = ["jedi" "palantir" "pylint" "flake8" "pycodestyle" "microsoft" "palantir" "pythonlsp"];
        settings = {
          permitUserSite = false;
        };
      };
    })

    # ({
    #   channel = "nixpkgs";
    #   language = "ruby_2_7";
    #   args = {
    #     packages = [];
    #     languageServers = [];
    #   };
    # })

    # ({
    #   channel = "nixpkgs-unstable";
    #   language = "rust_1_51";
    #   args = {
    #     packages = [];
    #     languageServers = [];
    #   };
    # })
  ];

  otherPackages = [
    { channel = "nixpkgs"; attr = "codedown.spellchecker"; contents = importedChannels.nixpkgs.codedown.spellchecker; }

    { channel = "nixpkgs"; attr = "codedown.shells.zsh"; contents = importedChannels.nixpkgs.codedown.shells.zsh; }
    { channel = "nixpkgs"; attr = "codedown.shells.fish"; contents = importedChannels.nixpkgs.codedown.shells.fish; }
    { channel = "nixpkgs"; attr = "codedown.shells.bash"; contents = importedChannels.nixpkgs.codedown.shells.bash; }

    { channel = "nixpkgs"; attr = "codedown.exporters.nbconvert-small"; contents = importedChannels.nixpkgs.codedown.exporters.nbconvert-small; }
    { channel = "nixpkgs"; attr = "codedown.exporters.nbconvert-large"; contents = importedChannels.nixpkgs.codedown.exporters.nbconvert-large; }

    # { channel = "nixpkgs"; attr = "tree"; contents = importedChannels.nixpkgs.tree; }
  ];
}
