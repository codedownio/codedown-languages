{ lib
, callPackage
, symlinkJoin

, R
, rPackages
, rWrapper

, settings
, settingsSchema
}:

with { inherit (settings) packages backend; };
with { inherit (settings.interface) attrs extensions; };

with lib;

let
  common = callPackage ../common.nix {};

  isArk = backend == "ark";

  userPackages = map (x: lib.getAttr x rPackages) (map common.packageName packages);

  basePackages =
    if isArk
    # The variable inspector uses jsonlite, which IRkernel would have brought in.
    then userPackages ++ lib.optionals settings.misc.enableVariableInspector [rPackages.jsonlite]
    else [rPackages.IRkernel] ++ userPackages;

  kernelName = "R";

  languageServers = []
    ++ lib.optionals settings.lsp.languageserver.enable [(
      (callPackage ./language-server-languageserver {
        inherit rWrapper basePackages kernelName;
        languageserver = callPackage ./language-server-languageserver/languageserver.nix { inherit rPackages rWrapper; };
      })
    )]
  ;

  ark = callPackage ./ark.nix { inherit R; };

  packageOptions = rPackages;
  packageSearch = common.searcher packageOptions;
  versions = {
    r = R.version;
    languageserver = (callPackage ./language-server-languageserver/languageserver.nix {}).version;
  } // (if isArk then {
    ark = ark.version;
  } else {
    irkernel = rPackages.IRkernel.version;
  });

  rWithPackages = rWrapper.override {
    packages = basePackages;
  };

  repls = {
    r = {
      display_name = "R";
      attr = "r";
      args = ["${rWithPackages}/bin/R"];
      icon = ./r-logo-64x64.png;
      iconMonochrome = ./r-monochrome.svg;
    };
  };

  kernel =
    if isArk
    then callPackage ./kernel-ark.nix {
      inherit ark R attrs extensions repls;
      # Ark embeds R through libR, so rWrapper's R_LIBS_SITE never reaches it.
      rLibsSite = lib.concatMapStringsSep ":" (p: "${p}/library") basePackages;
      version = R.version;
      enableVariableInspector = settings.misc.enableVariableInspector;
    }
    else callPackage ./kernel-irkernel.nix {
      inherit rWithPackages attrs extensions repls;
      version = R.version;
      enableVariableInspector = settings.misc.enableVariableInspector;
    };

in

symlinkJoin {
  name = "r";

  paths = [
    kernel
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
      iconMonochrome = ./r-monochrome.svg;
      hasPackages = packageOptions != {};
      hasDebugger = isArk;
    };
    inherit packageOptions packageSearch versions;
    inherit settingsSchema settings;
    inherit repls;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "r";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
    formatters = lib.concatMap (x: x.formatters or []) languageServers;
  };
}
