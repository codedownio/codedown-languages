{ lib
, pkgs
, callPackage
, clojure
, stdenv
, writeTextDir
, symlinkJoin

, bash
, attrs
, extensions
, settings
, settingsSchema
}:

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

  clojure-lsp = (builtins.getFlake "github:clojure-lsp/clojure-lsp/5e3584014f2ac9c13a877dfd7984383346d81609").packages.x86_64-linux.default;

  packageOptions = {};
  packageSearch = common.searcher packageOptions;

in

symlinkJoin {
  name = "clojure";
  paths = [
    (callPackage ./kernel.nix { inherit attrs extensions version; })
    clojure
  ]
  ++ lib.optionals settings.lsp.clojure-lsp.enable [(callPackage ./language-server.nix { inherit clojure-lsp kernelName; })]
  ;

  passthru = {
    meta = clojure.meta // {
      baseName = x;
      displayName = "Clojure";
      version = clojure.version;
      icon = ./clojure-logo-64x64.png;
      inherit settingsSchema;
    };
    inherit packageOptions packageSearch;
    versions = {
      clojure = clojure.version;
      clojure-lsp = clojure-lsp.version;
    };
    inherit settingsSchema settings;
    args = args // { baseName = x; };
    repls = repls clojure;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "clojure";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
