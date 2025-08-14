{pkgs
, lib
, callPackage
, symlinkJoin

, R
, rPackages
, rWrapper

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

with lib;

let
  common = callPackage ../common.nix {};

  basePackages = [rPackages.IRkernel] ++ (map (x: lib.getAttr x rPackages) (map common.packageName packages));

  kernelName = "R";

  languageServers = []
    ++ lib.optionals settings.lsp.languageserver.enable [(
      (callPackage ./language-server-languageserver {
        inherit rPackages rWrapper basePackages kernelName;
        languageserver = callPackage ./language-server-languageserver/languageserver.nix { inherit rPackages rWrapper; };
      })
    )]
  ;

  packageOptions = rPackages;
  packageSearch = common.searcher packageOptions;
  versions = {
    r = R.version;
    languageserver = (callPackage ./language-server-languageserver/languageserver.nix {}).version;
  };

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
    rWithPackages
  ]
  ++ languageServers
  ;

  passthru = {
    meta = R.meta // {
      baseName = "R";
      displayName = "R";
      version = R.version;
      icon = ./r-logo-64x64.png;
      iconSvg = ./r.svg;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch versions;
    inherit settingsSchema settings;
    repls = {
      r = {
        display_name = "R";
        attr = "r";
        args = ["${rWithPackages}/bin/R"];
        icon = ./r-logo-64x64.png;
        iconSvg = ./r.svg;
      };
    };
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "r";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
