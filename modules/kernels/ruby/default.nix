{ lib
, callPackage
, pkgs
, recurseIntoAttrs
, stdenv
, symlinkJoin
, writeTextDir

, ruby

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

with lib;

let
  common = callPackage ../common.nix {};

  kernelName = "ruby";

  rubyPackages = recurseIntoAttrs ruby.gems;

  packageOptions = rubyPackages;
  packageSearch = common.searcher packageOptions;

  languageServers =
    []
    ++ lib.optionals settings.lsp.solargraph.enable [(callPackage ./solargraph.nix { rubyPackages = packageOptions; inherit kernelName; })]
  ;

  iruby = (callPackage ./iruby { inherit ruby; }).iruby;

in

symlinkJoin {
  name = "ruby";
  paths = [
    (callPackage ./kernel.nix {
      inherit iruby;
      inherit attrs extensions version;
    })
    ruby
  ]
  ++ languageServers
  ;
  passthru = {
    meta = ruby.meta // {
      baseName = "ruby";
      displayName = "Ruby";
      version = ruby.version;
      icon = ./iruby-64x64.png;
      iconSvg = ./ruby.svg;
      inherit settingsSchema;
      hasPackages = packageOptions != {};
    };
    versions = {
      ruby = builtins.toString ruby.version;
      solargraph = packageOptions.solargraph.version;
      iruby = iruby.version;
    };
    inherit packageOptions packageSearch;
    inherit settingsSchema settings;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "ruby";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
  };
}
