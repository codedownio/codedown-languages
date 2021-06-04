{lib, callPackage, writeTextDir, symlinkJoin, cling}:

let
  modeInfoBase = {
    attrName = "cpp";
    codeMirrorMode = "clike";
    codeMirrorMimeType = "text/x-c++src";
    extensionsToHighlight = ["cpp" "hpp" "cxx" "hxx" "c" "h"];
    extensionsToRun = ["cpp" "cxx" "c"];
  };

in

rec {
  metadata = callPackage ./metadata.nix {};

  build = {
    baseName
    , packages ? (_: [])
    , languageServers ? (_: [])
    , codeDownAttr
    , otherLanguageKeys ? []
  }:
    let
      base = lib.findSingle (x: x.name  == baseName) null "multiple" metadata.baseOptions;

      modeInfo = writeTextDir "lib/codedown/cpp-modes.yaml" (lib.generators.toYAML {} [
        modeInfoBase
        (modeInfoBase // { attrName = base.name; })
      ]);

    in symlinkJoin {
      name = base.name;
      paths = [
        cling
        ((callPackage ./kernel.nix {}) base.meta.displayName base.std base.name base.logo)
        modeInfo
      ];
    };
}
