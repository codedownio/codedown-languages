{ lib
, callPackage
, symlinkJoin

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
  displayName = "Ruby " + ruby.version;

  rubyPackages = lib.recurseIntoAttrs ruby.gems;

  packageOptions = rubyPackages;
  packageSearch = common.searcher packageOptions;

  languageServers =
    []
    ++ lib.optionals settings.lsp.solargraph.enable [(callPackage ./solargraph.nix {
      rubyPackages = packageOptions;
      settings = settings.lsp.solargraph;
      inherit kernelName;
    })]
  ;

  iruby = (callPackage ./iruby { inherit ruby; }).iruby;

  repls = {
    irb = {
      display_name = "IRB " + (builtins.toString ruby.version);
      attr = "irb";
      args = ["${ruby}/bin/irb"];
      icon = ./iruby-64x64.png;
      iconMonochrome = ./ruby-monochrome.svg;
    };
  };

in

symlinkJoin {
  name = "ruby";
  paths = [
    (callPackage ./kernel.nix {
      inherit iruby;
      inherit displayName attrs extensions repls;
      version = ruby.version;
      enableVariableInspector = settings.misc.enableVariableInspector;
    })
    ruby
  ]
  ++ languageServers
  ;
  passthru = {
    meta = ruby.meta // {
      baseName = "ruby";
      inherit displayName;
      version = ruby.version;
      icon = ./iruby-64x64.png;
      iconMonochrome = ./ruby-monochrome.svg;
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
    inherit repls;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "ruby";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
    formatters = lib.concatMap (x: x.formatters or []) languageServers;
  };
}
