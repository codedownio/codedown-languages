{ lib
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

  basePackages = map (x: lib.getAttr x rPackages) (map common.packageName packages);

  kernelName = "R (Ark)";

  # Ark embeds R via libR rather than running R as a subprocess, so the
  # `rWrapper` indirection (which sets `R_LIBS_SITE` then execs R) doesn't apply
  # to its embedded session. We replicate the one thing the wrapper does —
  # building the library search path — and hand it to the kernel via `env`.
  rLibsSite = lib.concatMapStringsSep ":" (p: "${p}/library") basePackages;

  ark = callPackage ./ark.nix { inherit R; };

  languageServers = [];

  packageOptions = rPackages;
  packageSearch = common.searcher packageOptions;
  versions = {
    r = R.version;
    ark = ark.version;
  };

  rWithPackages = rWrapper.override {
    packages = basePackages;
  };

in

symlinkJoin {
  name = "r-ark";

  paths = [
    (callPackage ./kernel.nix {
      inherit ark R rLibsSite attrs extensions;
      version = R.version;
    })
    rWithPackages
  ]
  ++ languageServers
  ;

  passthru = {
    meta = R.meta // {
      baseName = "R-ark";
      displayName = "R (Ark)";
      version = R.version;
      icon = ./r-logo-64x64.png;
      iconMonochrome = ./r-monochrome.svg;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch versions;
    inherit settingsSchema settings;
    repls = {
      r = {
        display_name = "R (Ark)";
        attr = "r";
        args = ["${rWithPackages}/bin/R"];
        icon = ./r-logo-64x64.png;
        iconMonochrome = ./r-monochrome.svg;
      };
    };
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "r";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
