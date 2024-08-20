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
        (callPackage ./language-server-languageserver {
          inherit rPackages rWrapper basePackages kernelName;
          inherit languageserver;
        })
    )]
  ;

  meta = R.meta // {
    baseName = "R";
    displayName = "R";
    version = R.version;
    icon = ./r-logo-64x64.png;
  };

  repls = rWithPackages: version: {
    r = {
      display_name = "R";
      attr = "r";
      args = ["${rWithPackages}/bin/R"];
      icon = ./r-logo-64x64.png;
    };
  };

  packageOptions = rPackages;
  packageSearch = common.searcher packageOptions;
  versions = {
    r = R.version;
    languageserver = (callPackage ./language-server-languageserver/languageserver.nix {}).version;
  };

in

with lib;

listToAttrs [{
  name = "R";
  value = lib.makeOverridable ({
    packages ? []
    , settings ? {}
    , attrs ? ["r" "R"]
    , extensions ? ["r"]
  }@args:
    let
      settingsToUse = (common.makeDefaultSettings settingsSchema) // settings;

      basePackages = [rPackages.IRkernel] ++ (map (x: lib.getAttr x rPackages) (map common.packageName packages));

      rWithPackages = rWrapper.override {
        packages = basePackages;
      };

      languageServers = chooseLanguageServers settingsToUse rPackages rWrapper basePackages "R";
    in
      symlinkJoin {
        name = "r";

        paths = [
          (callPackage ./kernel.nix {
            inherit rWithPackages attrs extensions;
            version = R.version;
          })
          rWithPackages
        ]
        ++ languageServers
        ;

        passthru = {
          inherit meta packageOptions packageSearch versions;
          inherit settingsSchema settings;
          args = args // { baseName = "R"; };
          repls = repls rWithPackages R.version;
          modes = {
            inherit attrs extensions;
            code_mirror_mode = "r";
          };
          languageServerNames = map (x: x.languageServerName) languageServers;
        };
      }
  ) {};
}]
