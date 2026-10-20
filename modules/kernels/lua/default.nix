{ lib
, callPackage
, symlinkJoin

, lua
, lua-language-server
, xeus-lua

, settings
, settingsSchema
}:

with { inherit (settings) packages; };
with { inherit (settings.interface) attrs extensions; };

with lib;

let
  kernelName = "lua";

  common = callPackage ../common.nix {};

  packageOptions = lua.pkgs;
  packageSearch = common.searcher packageOptions;

  chosenPackages = map (x: getAttr x packageOptions) packages;

  # xeus-lua ships the Lua logo with its kernelspec, so there's no PNG to vendor here.
  icon = "${xeus-lua}/share/jupyter/kernels/xlua/logo-64x64.png";

  # Built even when nothing is selected, so the kernel and the language server can
  # name one path for Lua's module search regardless.
  luaEnv = lua.withPackages (_ps: chosenPackages);

  languageServers = lib.optionals settings.lsp.lua-language-server.enable
    [(callPackage ./language-server-lua {
      inherit attrs icon kernelName lua luaEnv;
      settings = settings.lsp.lua-language-server;
    })];

  repls = {
    lua = {
      display_name = "Lua " + lua.version;
      attr = "lua";
      args = ["${luaEnv}/bin/lua"];
      inherit icon;
      iconMonochrome = ./lua-monochrome.svg;
    };
  };

in

symlinkJoin {
  name = "lua";

  paths = [
    (callPackage ./kernel.nix {
      inherit attrs extensions lua luaEnv repls xeus-lua;
      enableVariableInspector = settings.misc.enableVariableInspector;
    })
    luaEnv
  ]
  ++ languageServers
  ;

  passthru = {
    meta = lua.meta // {
      baseName = kernelName;
      displayName = "Lua " + lua.version;
      version = lua.version;
      inherit icon;
      iconMonochrome = ./lua-monochrome.svg;
      inherit settingsSchema;
      hasPackages = packageOptions != {};
    };
    versions = {
      lua = lua.version;
      xeus-lua = xeus-lua.version;
      lua-language-server = lua-language-server.version;
    };
    inherit packageOptions packageSearch;
    inherit repls;
    inherit settings settingsSchema;
    modes = {
      inherit attrs extensions;
      code_mirror_mode = "lua";
    };
    languageServerNames = map (x: x.languageServerName) languageServers;
    formatters = lib.concatMap (x: x.formatters or []) languageServers;
  };
}
