with import <nixpkgs> {};
with python3Packages;

rec {
  name = "cpp14";

  binaries = [shared.cling];

  kernel = shared.kernel "C++ 14" "c++14" "cpp14" ./cpp14.png;

  languageServer = shared.languageServer "cpp14";

  homeFolderPaths = [shared.cclsFile];

  extraGitIgnoreLines = [
    ".ccls"
  ];

  shared = import ./shared.nix;

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [
    shared.modeInfoBase
    (shared.modeInfoBase // { attrName = "cpp14"; })
  ]);
}
