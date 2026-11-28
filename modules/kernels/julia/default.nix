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

  # Vendored copy of nixpkgs' julia.withPackages, carrying a fix that isn't upstream yet
  # (see julia-modules/VENDORED.md). Switch back to "julia.withPackages" once it lands.
  # juliaWithPackagesBase = julia.withPackages # To use upstream
  juliaWithPackagesBase = callPackage ./julia-modules {};

  juliaWithPackages = juliaWithPackagesBase.override {
    inherit julia;
    inherit (settings) precompile;
    juliaCpuTarget = "generic";
  };

  displayName = "Julia " + julia.version;
  kernelName = "julia";

  # SymbolServer.jl, which LanguageServer.jl 5 indexes packages with, fails on Julia 1.13.
  languageServerEnabled = settings.lsp.LanguageServer.enable && lib.versionOlder julia.version "1.13";

  packageOptions = lib.listToAttrs (map (x: {
    name = x;
    value = {
      meta = {
        name = x;
        displayName = x;
      };
    };
  }) (import ./julia-modules/package-names.nix));
  packageSearch = common.searcher' {
    packages = packageOptions;
    packageMustBeDerivation = false;
  };

  juliaToUse = juliaWithPackages (
    ["IJulia"]
    # The variable inspector (variable_inspector.jl) imports JSON to serialize its output.
    ++ lib.optionals settings.misc.enableVariableInspector ["JSON"]
    ++ packages
    ++ lib.optionals languageServerEnabled ["LanguageServer" "SymbolServer"]
  );

  languageServers =
    []
    ++ lib.optionals languageServerEnabled [(callPackage ./language-server-LanguageServer.nix {
      inherit attrs;
      juliaWithPackages = juliaToUse;
      inherit kernelName;
      settings = settings.lsp.LanguageServer;
    })]
  ;

  repls = {
    julia = {
      display_name = "Julia " + julia.version;
      attr = "julia";
      args = ["${juliaToUse}/bin/julia"];
      icon = ./julia-logo-64x64.png;
      iconMonochrome = ./julia-monochrome.svg;
    };
  };

in

symlinkJoin {
  name = "julia";

  paths = [
    (callPackage ./kernel.nix {
      inherit attrs extensions displayName repls;
      julia = juliaToUse;
      python = python3;
      enableVariableInspector = settings.misc.enableVariableInspector;
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
    inherit repls;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "julia";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
    formatters = lib.concatMap (x: x.formatters or []) languageServers;
  };
}
