
let
  bootstrapNixpkgs = import <nixpkgs> {};
  fetchgit = bootstrapNixpkgs.fetchgit;
  fetchFromGitHub = bootstrapNixpkgs.fetchFromGitHub;
  lib = bootstrapNixpkgs.lib;

  # codedownSrc = /home/tom/tools/codedown-languages;
  # codedownSrc = builtins.fetchGit {
  #   url = https://github.com/codedownio/codedown-languages.git;
  #   rev = "8a1386e9b3e900e19f61e8cc5061821483f4db64";
  #   ref = "main";
  # };

  overlays = {
    codedown = {
      tag = "path";
      path = ./.;
    };
  };

  channels = rec {
    nixpkgs = {
      tag = "fetch_from_github";
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "973910f5c31b9ba6c171c33a8bd7199990b14c72";
      sha256 = "1n1kibmn1fcjazaxp8lydwdx646lknqksv5b2fm33fdq2dvafvj7";
    };

    nixpkgs-unstable = {
      tag = "fetch_from_github";
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "ecaf3da9340231e5493eccc3db87604a3705da42";
      sha256 = "049dcpzklpjj0c7g172njfcqb9xvxkpyf7qjiwvaf8klgd5cippa";
    };
  };

  channelSpecToChannel = name: channel:
    if (channel.tag == "fetch_from_github") then fetchFromGitHub ((removeAttrs channel ["tag" "name"]))
    else if (channel.tag == "fetch_git") then fetchgit (removeAttrs channel ["tag" "name"])
    else if (channel.tag == "path") then channel.path else null;

  importedOverlays = lib.mapAttrsToList (name: value: import (channelSpecToChannel name value)) overlays;
  importedChannels = lib.mapAttrs (name: value: import (channelSpecToChannel name value) { overlays = importedOverlays; }) channels;

in

importedChannels.nixpkgs.codedown.mkCodeDownEnvironment {
  inherit channels importedChannels overlays;

  shells = [
    { channel = "nixpkgs"; attr = "codedown.shells.zsh"; contents = importedChannels.nixpkgs.codedown.shells.zsh; }
    { channel = "nixpkgs"; attr = "codedown.shells.fish"; contents = importedChannels.nixpkgs.codedown.shells.fish; }
    { channel = "nixpkgs"; attr = "codedown.shells.bash"; contents = importedChannels.nixpkgs.codedown.shells.bash; }
  ];

  # metaOnly = true;

  kernels = [
    ({
      channel = "nixpkgs";
      language = "bashInteractive";
      args = {
        packages = [];
        languageServers = [];
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

    ({
      channel = "nixpkgs-unstable";
      language = "julia_15";
      args = {
        packages = [];
        languageServers = [];
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
      channel = "nixpkgs";
      language = "python38";
      args = {
        packages = ["matplotlib" "scipy" "rope"];
        languageServers = ["jedi" "pylint"];
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
    { channel = "nixpkgs"; attr = "codedown.spellchecker"; contents = importedChannels.nixpkgs.codedownSpellchecker; }
    # { channel = "nixpkgs"; attr = "ncdu"; contents = importedChannels.nixpkgs.ncdu; }
    # { channel = "nixpkgs"; attr = "tree"; contents = importedChannels.nixpkgs.tree; }
  ];
}
