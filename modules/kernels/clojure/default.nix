{ lib
, pkgs
, callPackage
, clojure
, stdenv
, writeTextDir
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
    };
  };

  clojure-lsp = callPackage ./clojure-lsp.nix {};

  packageOptions = {};
  packageSearch = common.searcher packageOptions;

  kernelName = "clojure";

  languageServers = []
    ++ lib.optionals settings.lsp.clojure-lsp.enable [(callPackage ./language-server.nix { inherit clojure-lsp kernelName; })];

in

symlinkJoin {
  name = "clojure";
  paths = [
    (callPackage ./kernel.nix { inherit attrs extensions; version = clojure.version; })
    clojure
  ]
  ++ languageServers
  ;

  passthru = {
    meta = clojure.meta // {
      baseName = "clojure";
      displayName = "Clojure";
      version = clojure.version;
      icon = ./clojure-logo-64x64.png;
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
  };
}
