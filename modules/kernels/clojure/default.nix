{ lib
, callPackage
, clojure
, clojure-lsp
, symlinkJoin

, settings
, settingsSchema
}:

with { inherit (settings.interface) attrs extensions; };

let
  common = callPackage ../common.nix {};

  repls = clojure: {
    clojure = {
      display_name = "Clojure " + clojure.version;
      attr = "clojure";
      args = ["${clojure}/bin/clojure"];
      icon = ./clojure-logo-64x64.png;
      iconMonochrome = ./clojure-monochrome.svg;
    };
  };

  packageOptions = {};
  packageSearch = common.searcher packageOptions;

  kernelName = "clojure";
  displayName = "Clojure " + clojure.version;

  languageServers = []
    ++ lib.optionals settings.lsp.clojure-lsp.enable [(callPackage ./language-server.nix { inherit kernelName; })]
  ;

in

symlinkJoin {
  name = "clojure";
  paths = [
    (callPackage ./kernel.nix {
      inherit displayName attrs extensions;
      repls = repls clojure;
      version = clojure.version;
      enableVariableInspector = settings.misc.enableVariableInspector;
    })
    clojure
  ]
  ++ languageServers
  ;

  passthru = {
    meta = clojure.meta // {
      baseName = "clojure";
      inherit displayName;
      version = clojure.version;
      icon = ./clojure-logo-64x64.png;
      iconMonochrome = ./clojure-monochrome.svg;
      inherit settingsSchema;
      hasPackages = packageOptions != {};
    };
    inherit packageOptions packageSearch;
    versions = {
      clojure = clojure.version;
      clojure-lsp = clojure-lsp.version;
    };
    inherit settingsSchema settings;
    repls = repls clojure;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "clojure";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
    formatters = lib.concatMap (x: x.formatters or []) languageServers;
  };
}
