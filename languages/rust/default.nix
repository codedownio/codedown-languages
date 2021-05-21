{pkgs, callPackage, writeText}:

with pkgs.lib;

rec {
  metadata = callPackage ./metadata.nix {};

  build = {
    baseName,
    packages ? (_: []),
    languageServers ? (_: []),
    codeDownAttr ? baseName,
    otherLanguageKeys ? []
  }:
    let
      base = findSingle (x: x.name == baseName) null "multiple" metadata.baseOptions;

      rustPackages = base.rust.packages.stable;

      availableLanguageServers = metadata.languageServerOptions base python.pkgs;
    in {
      name = "rust";
      binaries = [rustPackages.rustc rustPackages.cargo pkgs.gcc];
      kernel = callPackage ./kernel.nix {
        evcxr = pkgs.evcxr.override {
          rustPlatform = rustPackages.rustPlatform;
          cargo = rustPackages.cargo;
        };
      };
      languageServer = null;
      modeInfo = writeText "mode_config.yaml" (generators.toYAML {} [{
        attrName = "rust";
        codeMirrorMode = "rust";
        extensionsToHighlight = ["rs" "rc"];
        extensionsToRun = ["rs"];
      }]);
    };
}
