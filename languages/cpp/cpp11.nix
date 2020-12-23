with import <nixpkgs> {};
with python3Packages;

let
  shared = import ./shared.nix;

in

rec {
  name = "cpp11";

  binaries = [shared.cling];

  kernel = shared.kernel "C++ 11" "c++11" "cpp11" ./cpp11.png;

  languageServer = shared.languageServer "cpp11";

  homeFolderPaths = [shared.cclsFile];

  extraGitIgnoreLines = [
    ".ccls"
  ];

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [
    shared.modeInfoBase
    (shared.modeInfoBase // { attrName = "cpp11"; })
  ]);
}
