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

  settingsSchema = [
    {
      target = "lsp.languageserver.enable";
      title = "Enable languageserver";
      type = "boolean";
      defaultValue = true;
    }
  ];

  chooseLanguageServers = settings: rPackages: rWrapper: basePackages: kernelName:
    []
    ++ lib.optionals (common.isTrue settings "lsp.languageserver.enable") [(callPackage ./language_server.nix { inherit rPackages rWrapper basePackages kernelName; })]
  ;

  meta = R.meta // {
    baseName = "R";
    displayName = if lib.hasAttr "version" R then "R " + R.version else "R";
    version = R.version;
    icon = ./logo-64x64.png;
  };

  repls = rWithPackages: version: {
    r = {
      display_name = "R " + version;
      args = ["${rWithPackages}/bin/R"];
      icon = ./logo-64x64.png;
    };
  };

in

with lib;

listToAttrs [{
  name = "R";
  value = rec {
    packageOptions = rPackages;
    packageSearch = common.searcher packageOptions;

    build = args@{
      packages ? []
      , settings ? {}
      , attrs ? ["r" "R"]
      , extensions ? ["r"]
      , metaOnly ? false
    }:
      let
        settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;

        basePackages = [rPackages.IRkernel] ++ (map (x: lib.getAttr x rPackages) packages);

        rWithPackages = rWrapper.override {
          packages = basePackages;
        };
      in
        symlinkJoin {
          name = "r";

          paths = [
            (callPackage ./kernel.nix { inherit rWithPackages attrs extensions; })
            (callPackage ./mode_info.nix { inherit attrs extensions; })
          ]
          ++ (if metaOnly then [] else [rWithPackages])
          ++ (if metaOnly then [] else chooseLanguageServers settingsToUse rPackages rWrapper basePackages "R")
          ;

          passthru = {
            inherit meta packageOptions;
            args = args // { baseName = "R"; };
            repls = repls rWithPackages R.version;
          };
        };

    inherit meta;
  };
}]
