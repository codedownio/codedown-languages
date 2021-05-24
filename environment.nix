
let
  bootstrapNixpkgs = import <nixpkgs> {};
  fetchgit = bootstrapNixpkgs.fetchgit;

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
    nixpkgs-20-09 = import (fetchgit {
      url = https://github.com/NixOS/nixpkgs.git;
      rev = "33824cdf8e4fec30c5b9ddc91b18991c3c375227";
      branchName = "release-20.09";
      sha256 = "1sad0x998k3iid2vp57kv4skvf90yh4gbs61dv3p45c2qi3sql46";
    }) { inherit overlays; };

    nixpkgs-unstable = import (fetchgit {
      url = https://github.com/NixOS/nixpkgs.git;
      rev = "7013a0f2791da4c38c7e6f56d48139aeb344991b";
      branchName = "nixpkgs-unstable";
      sha256 = "1az617wpx535nfn0rz63cyvv8b5rlsp80cdq07da2dws8zzylnbm";
    }) { inherit overlays; };
  };

  nixpkgs = channels.nixpkgs-20-09;
  nixpkgsUnstable = channels.nixpkgs-unstable;

in

nixpkgs.codedown.mkCodeDownEnvironment {
  specHash = nixpkgs.lib.fakeSha256;
  spec = "THIS IS THE SPEC";

  kernels = [
    (nixpkgsUnstable.codedown.languages.bash.build {
      baseName = "bashInteractive";
      packages = ps: [];
      languageServers = choices: [];
    })

    # (nixpkgsUnstable.codedown.languages.dot.build {
    #   baseName = "graphviz";
    #   packages = ps: [];
    #   languageServers = choices: [];
    # })

    (nixpkgsUnstable.codedown.languages.cpp.build {
      baseName = "cpp11";
      packages = ps: [];
      languageServers = choices: [];
      codeDownAttr = "cpp";
    })

    (nixpkgs.codedown.languages.r.build {
      baseName = "r";
      packages = ps: [ps.ggplot2];
      languageServers = choices: [];
    })

    (nixpkgsUnstable.codedown.languages.octave.build {
      baseName = "octave";
      packages = ps: [ps.arduino];
      languageServers = choices: [];
      extraJupyterConfig = ''
        c.OctaveKernel.plot_settings = dict(format='svg')
      '';
    })

    (nixpkgs.codedown.languages.python.build {
      baseName = "python38";
      packages = ps: [ps.matplotlib ps.scipy];
      languageServers = choices: [choices.jedi];
    })

    (nixpkgs.codedown.languages.ruby.build {
      baseName = "ruby_2_7";
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
