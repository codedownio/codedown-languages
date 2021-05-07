with import <nixpkgs> {};

let 
  codedown = import ./default.nix;

in

codedown.mkCodeDownEnvironment {
  kernels = [
    (callPackage codedown.pythonPack {
      baseName = "python38";
      languageServers = choices: []; # choices.jedi
      packages = ps: [ps.matplotlib];
    })
  ];

  notebookLanguageServers = [

  ];
}
