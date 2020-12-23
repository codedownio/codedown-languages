with import <nixpkgs> {};
with python3Packages;

let
  shared = import ./shared.nix;

in

rec {
  name = "cpp2a";

  binaries = [shared.cling];

  kernel = shared.kernel "C++ 2a" "c++2a" "cpp2a" ./cpp2a.png;

  languageServer = shared.languageServer "cpp2a";

  homeFolderPaths = [shared.cclsFile];

  extraGitIgnoreLines = [
    ".ccls"
  ];

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [
    shared.modeInfoBase
    (shared.modeInfoBase // { attrName = "cpp2a"; })
  ]);
}
