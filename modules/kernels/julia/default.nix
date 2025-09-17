{ lib
, python3
, callPackage
, symlinkJoin

, julia

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

with lib;

let
  common = callPackage ../common.nix {};

  # juliaWithPackagesBase = julia.withPackages # To use upstream
  juliaWithPackagesBase = callPackage ./julia-modules {};

  juliaWithPackages = juliaWithPackagesBase.override {
    inherit julia;
    inherit (settings) precompile;
    juliaCpuTarget = "generic";
  };

  displayName = "Julia " + julia.version;
  kernelName = "julia";

  packageOptions = {
    packages = lib.listToAttrs (map (x: {
      name = x;
      value = {
        meta = {
          name = x;
          displayName = x;
        };
      };
    }) (import ./julia-modules/package-names.nix));
    packageMustBeDerivation = false;
  };
  packageSearch = common.searcher' packageOptions;

  juliaToUse = juliaWithPackages (
    ["IJulia"]
    ++ packages
    ++ lib.optionals settings.lsp.LanguageServer.enable ["LanguageServer" "SymbolServer"]
  );

  languageServers =
    []
    ++ lib.optionals settings.lsp.LanguageServer.enable [(callPackage ./language-server-LanguageServer.nix {
      inherit attrs;
      juliaWithPackages = juliaToUse;
      inherit kernelName;
      settings = settings.lsp.LanguageServer;
    })]
  ;

in

symlinkJoin {
  name = "julia";

  paths = [
    (callPackage ./kernel.nix {
      inherit attrs extensions displayName;
      julia = juliaToUse;
      python = python3;
    })
    juliaToUse
  ]
  ++ languageServers
  ;

  passthru = {
    meta = julia.meta // {
      baseName = "julia";
      inherit displayName settingsSchema;
      version = julia.version;
      icon = ./julia-logo-64x64.png;
      iconMonochrome = ./julia-monochrome.svg;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch;
    versions = {
      julia = julia.version;
    };
    inherit settingsSchema settings;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "julia";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
