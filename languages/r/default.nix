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
    ++ lib.optionals (common.isTrue settings "lsp.languageserver.enable") [(
      let
        languageserver = callPackage ./language-server-languageserver/languageserver.nix { inherit rPackages rWrapper; };
      in
        (callPackage ./language-server-languageserver.nix {
          inherit rPackages rWrapper basePackages kernelName;
          inherit languageserver;
        })
    )]
  ;

  meta = R.meta // {
    baseName = "R";
    displayName = "R";
    version = R.version;
    icon = ./logo-64x64.png;
  };

  repls = rWithPackages: version: {
    r = {
      display_name = "R";
      attr = "r";
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
    versions = {
      r = R.version;
      languageserver = (callPackage ./language-server-languageserver/languageserver.nix {}).version;
    };

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
            (callPackage ./kernel.nix {
              inherit rWithPackages attrs extensions;
              version = R.version;
            })
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
