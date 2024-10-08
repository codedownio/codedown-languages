{ callPackage
, gopls
, lib
, pkgs
, symlinkJoin

, go

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

with lib;

let
  common = callPackage ../common.nix {};

  kernelName = "go";

  languageServers =
    []
    ++ lib.optionals settings.lsp.gopls.enable [(callPackage ./language-server-gopls.nix { inherit go attrs; inherit kernelName; })]
  ;

  packageOptions = {};
  packageSearch = common.searcher packageOptions;

in

symlinkJoin {
  name = "go";
  paths = [
    (callPackage ./kernel.nix {
      inherit attrs extensions;
      version = go.version;
    })
    go
  ]
  ++ languageServers
  ;

  passthru = {
    meta = go.meta // {
      baseName = "go";
      displayName = "Go";
      version = go.version;
      icon = ./go-logo-64x64.png;
      inherit settingsSchema;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch;
    versions = {
      go = go.version;
      gopls = gopls.version;
    };
    inherit settingsSchema settings;
    repls = {};
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "go";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
