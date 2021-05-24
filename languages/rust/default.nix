{pkgs, callPackage, writeTextDir, symlinkJoin}:

with pkgs.lib;

rec {
  metadata = callPackage ./metadata.nix {};

  modeInfo = writeTextDir "lib/rust-mode-config.yaml" (generators.toYAML {} [{
    attrName = "rust";
    codeMirrorMode = "rust";
    extensionsToHighlight = ["rs" "rc"];
    extensionsToRun = ["rs"];
  }]);

  build = {
    baseName
    , packages ? (_: [])
    , languageServers ? (_: [])
    , codeDownAttr ? baseName
    , otherLanguageKeys ? []
  }:
    let
      base = findSingle (x: x.name == baseName) null "multiple" metadata.baseOptions;

      rustPackages = base.rust.packages.stable;

      availableLanguageServers = metadata.languageServerOptions base python.pkgs;
    in symlinkJoin {
      name = "rust";
      paths = [
        (callPackage ./kernel.nix {
          evcxr = pkgs.evcxr.override {
            # rustPlatform = rustPackages.rustPlatform;
            # cargo = rustPackages.cargo;
          };
        })

        rustPackages.rustc rustPackages.cargo pkgs.gcc

        modeInfo
      ];
    };
}
