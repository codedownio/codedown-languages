{pkgs
, lib
, callPackage
, symlinkJoin
}:

rec {
  metadata = callPackage ./metadata.nix {};

  build = args@{
    baseName
    , packages ? []
    , languageServers ? []
    , codeDownAttr ? "r"
    , otherLanguageKeys ? []
  }:
    let
      base = lib.head metadata.baseOptions;

      rWithPackages = base.rWrapper.override {
        packages = [base.rPackages.IRkernel] ++ (map (x: lib.getAttr x base.rPackages) packages);
      };
    in symlinkJoin {
      name = "r";
      paths = [
        rWithPackages
        (callPackage ./kernel.nix { inherit rWithPackages; })
        (callPackage ./mode_info.nix {})
      ];
      passthru = {
        inherit args metadata;
        meta = base.meta;
      };
    };
}
