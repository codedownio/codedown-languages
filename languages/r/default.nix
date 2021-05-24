{pkgs, lib, callPackage, symlinkJoin}:

rec {
  metadata = callPackage ./metadata.nix {};

  build = {
    baseName
    , packages ? (_: [])
    , languageServers ? (_: [])
    , codeDownAttr ? "r"
    , otherLanguageKeys ? []
  }:
    let
      base = lib.head metadata.baseOptions;

      rWithPackages = base.rWrapper.override {
        packages = [base.rPackages.IRkernel] ++ packages base.rPackages;
      };
    in symlinkJoin {
      name = "r";
      paths = [
        rWithPackages
        (callPackage ./kernel.nix { inherit rWithPackages; })
        (callPackage ./mode_info.nix {})
      ];
    };
}
