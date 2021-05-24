{lib, callPackage, writeText, cling}:

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
    baseName,
    packages ? (_: []),
    languageServers ? (_: []),
    codeDownAttr,
    otherLanguageKeys ? []
  }:
    let
      base = lib.findSingle (x: x.name  == baseName) null "multiple" metadata.baseOptions;
    in {
      name = base.name;
      binaries = [cling]; # TODO: get cling set up with library paths
      kernel = (callPackage ./kernel.nix {}) base.displayName base.std base.name base.logo;
      languageServer = null; # callPackage ./ccls.nix {};
      modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [
        modeInfoBase
        (modeInfoBase // { attrName = base.name; })
      ]);
    };
}
