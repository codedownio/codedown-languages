
let
  bootstrapNixpkgs = import <nixpkgs> {};
  fetchgit = bootstrapNixpkgs.fetchgit;
  lib = bootstrapNixpkgs.lib;

  # codedownSrc = /home/tom/tools/codedown-languages;
  # codedownSrc = builtins.fetchGit {
  #   url = https://github.com/codedownio/codedown-languages.git;
  #   rev = "8a1386e9b3e900e19f61e8cc5061821483f4db64";
  #   ref = "main";
  # };

  overlays = {
    codedown = ./.;
  };

  channels = rec {
    nixpkgs = fetchgit {
      url = https://github.com/NixOS/nixpkgs.git;
      rev = "33824cdf8e4fec30c5b9ddc91b18991c3c375227";
      branchName = "release-20.09";
      sha256 = "1sad0x998k3iid2vp57kv4skvf90yh4gbs61dv3p45c2qi3sql46";
    };

    nixpkgs-unstable = fetchgit {
      url = https://github.com/NixOS/nixpkgs.git;
      rev = "7013a0f2791da4c38c7e6f56d48139aeb344991b";
      branchName = "nixpkgs-unstable";
      sha256 = "1az617wpx535nfn0rz63cyvv8b5rlsp80cdq07da2dws8zzylnbm";
    };
  };

  importedChannels = rec {
    nixpkgs = import channels.nixpkgs { overlays = map (x: import x) (lib.attrValues overlays); };
    nixpkgs-unstable = import channels.nixpkgs-unstable { overlays = map (x: import x) (lib.attrValues overlays); };
  };

in

importedChannels.nixpkgs.codedown.mkCodeDownEnvironment {
  inherit channels overlays;

  kernels = [
    (importedChannels.nixpkgs-unstable.codedown.languages.bashInteractive.build {
      packages = [];
      languageServers = [];
    })

    # (importedChannels.nixpkgs-unstable.codedown.languages.dot.build {
    #   packages = [];
    #   languageServers = [];
    # })

    # (importedChannels.nixpkgs-unstable.codedown.languages.cpp11.build {
    #   packages = [];
    #   languageServers = [];
    #   codeDownAttr = "cpp";
    # })

    # (importedChannels.nixpkgs.codedown.languages.R-4.0.2.build {
    #   packages = ["ggplot2"];
    #   languageServers = [];
    # })

    # (importedChannels.nixpkgs-unstable.codedown.languages.octave.build {
    #   packages = ["arduino"];
    #   languageServers = [];
    #   extraJupyterConfig = ''
    #     c.OctaveKernel.plot_settings = dict(format='svg')
    #   '';
    # })

    (importedChannels.nixpkgs.codedown.languages.python38.build {
      packages = ["matplotlib" "scipy"];
      languageServers = ["jedi"];
    })

    # (importedChannels.nixpkgs.codedown.languages.ruby_2_7.build {
    #   packages = [];
    #   languageServers = [];
    # })

    # (importedChannels.nixpkgs-unstable.codedown.languages.rust_1_45.build {
    #   packages = [];
    #   languageServers = [];
    # })
  ];

  otherPackages = [
    { channel = "nixpkgs"; attr = "codedown.spellchecker"; contents = importedChannels.nixpkgs.codedown.spellchecker; }
    { channel = "nixpkgs"; attr = "ncdu"; contents = importedChannels.nixpkgs.ncdu; }
    { channel = "nixpkgs"; attr = "tree"; contents = importedChannels.nixpkgs.tree; }
  ];
}
