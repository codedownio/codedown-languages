with import <nixpkgs> {};

let 
  codedown = import ./default.nix;

in

codedown.mkCodeDownEnvironment {
  kernels = [
    (callPackage codedown.pythonPack {
      baseName = "python38";
      languageServers = choices: [choices.jedi]; # choices.jedi
      packages = ps: [ps.matplotlib];
    })

    (callPackage codedown.juliaPack {
      baseName = "julia15";
      languageServers = choices: [];
      packages = ps: [];
    })
  ];

  notebookLanguageServers = [

  ];
}
