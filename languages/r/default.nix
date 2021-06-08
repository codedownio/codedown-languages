{pkgs
, lib
, callPackage
, symlinkJoin
, R
, rPackages
, rWrapper
}:

let
  common = callPackage ../common.nix {};

  meta = R.meta // {
    baseName = "R";
    displayName = if lib.hasAttr "version" R then "R " + R.version else "R";
    icon = ./logo-64x64.png;
  };

in

with lib;

listToAttrs [{
  name = "R";
  value = rec {
    packageOptions = rPackages;
    packageSearch = common.searcher packageOptions;

    languageServerOptions = {};
    languageServerSearch = common.searcher languageServerOptions;

    build = args@{
      packages ? []
      , languageServers ? []
      , codeDownAttr ? "r"
      , otherLanguageKeys ? []
    }:
      let
        rWithPackages = rWrapper.override {
          packages = [rPackages.IRkernel] ++ (map (x: lib.getAttr x rPackages) packages);
        };
      in symlinkJoin {
        name = "r";
        paths = [
          rWithPackages
          (callPackage ./kernel.nix { inherit rWithPackages; })
          (callPackage ./mode_info.nix {})
        ];
        passthru = {
          args = args // { baseName = "R"; };
          inherit meta packageOptions languageServerOptions;
        };
      };

    inherit meta;
  };
}]
