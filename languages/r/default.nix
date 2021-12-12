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

  allLanguageServerOptions = rPackages: rWrapper: basePackages: {
    languageserver = (callPackage ./language_server.nix { inherit rPackages rWrapper basePackages; });
  };

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

    languageServerOptions = allLanguageServerOptions rPackages rWrapper [];
    languageServerSearch = common.searcher languageServerOptions;

    build = args@{
      packages ? []
      , languageServers ? []
      , attrs ? ["r" "R"]
      , extensions ? ["r"]
      , metaOnly ? false
    }:
      let
        basePackages = [rPackages.IRkernel] ++ (map (x: lib.getAttr x rPackages) packages);

        rWithPackages = rWrapper.override {
          packages = basePackages;
        };
      in symlinkJoin {
        name = "r";

        paths = [
          rWithPackages
          (callPackage ./kernel.nix { inherit rWithPackages attrs extensions; })
          (callPackage ./mode_info.nix { inherit attrs extensions; })
        ]
        ++ (map (x: builtins.getAttr x (allLanguageServerOptions rPackages rWrapper basePackages)) languageServers);

        passthru = {
          args = args // { baseName = "R"; };
          inherit meta packageOptions languageServerOptions;
        };
      };

    inherit meta;
  };
}]
