{pkgs, lib, callPackage}:

rec {
  metadata = callPackage ./metadata.nix {};

  build = {
    baseName,
    packages ? (_: []),
    languageServers ? (_: []),
    codeDownAttr ? "r",
    otherLanguageKeys ? []
  }:
    let
      base = lib.head metadata.baseOptions;

      rWithPackages = base.rWrapper.override {
        packages = [base.rPackages.IRkernel] ++ packages base.rPackages;
      };
    in {
      name = "r";
      binaries = [rWithPackages];
      kernel = callPackage ./kernel.nix { inherit rWithPackages; };
      languageServer = null;
      modeInfo = callPackage ./mode_info.nix {};
    };
}
