let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with makeWrapper;

let
  shared = import ./shared.nix;

in

rec {
  name = "cpp17";

  binaries = [shared.cling];

  kernel = shared.kernel "C++ 17" "c++17" "cpp17" ./cpp17.png;

  languageServer = shared.languageServer "cpp17";

  homeFolderPaths = [shared.cclsFile];

  extraGitIgnoreLines = [
    ".ccls"
  ];

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [
    shared.modeInfoBase
    (shared.modeInfoBase // { attrName = "cpp17"; })
  ]);
}
