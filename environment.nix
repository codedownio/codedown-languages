
let
  codedownSrc = import ./.;
  # codedownSrc = import /home/tom/tools/codedown-languages;
  # codedownSrc = import (builtins.fetchGit {
  #   url = https://github.com/codedownio/codedown-languages.git;
  #   rev = "8a1386e9b3e900e19f61e8cc5061821483f4db64";
  #   ref = "main";
  # });

  overlays = [(final: prev: {
    codedown = codedownSrc {pkgs = final;};
  })];

  channels = rec {
    nixpkgs-20-09 = import (builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs.git;
      rev = "33824cdf8e4fec30c5b9ddc91b18991c3c375227";
      ref = "release-20.09";
    }) { inherit overlays; };

    nixpkgs-unstable = import (builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs.git;
      rev = "7013a0f2791da4c38c7e6f56d48139aeb344991b";
      ref = "nixpkgs-unstable";
    }) { inherit overlays; };
  };

  nixpkgs = channels.nixpkgs-20-09;

in

nixpkgs.codedown.mkCodeDownEnvironment {
  kernels = [
    (nixpkgs.codedown.languages.octave.build {
      baseName = "octave";
      packages = ps: [];
      languageServers = choices: [];
    })

    (nixpkgs.codedown.languages.python.build {
      baseName = "python38";
      packages = ps: [ps.matplotlib ps.scipy];
      languageServers = choices: [choices.jedi];
    })

    (nixpkgs.codedown.languages.ruby.build {
      baseName = "ruby_3_0";
      packages = ps: [];
      languageServers = choices: [];
    })

    (channels.nixpkgs-unstable.codedown.languages.rust.build {
      baseName = "rust_1_45";
      packages = ps: [];
      languageServers = choices: [];
    })
  ];

  notebookLanguageServers = [
    nixpkgs.codedown.spellchecker
  ];
}
