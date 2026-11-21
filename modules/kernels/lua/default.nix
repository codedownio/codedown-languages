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
  displayName = "Lua " + lua.version;

  common = callPackage ../common.nix {};

  packageOptions = lua.pkgs;
  packageSearch = common.searcher packageOptions;

  chosenPackages = map (x: getAttr x packageOptions) packages;

  icon = "${xeus-lua}/share/jupyter/kernels/xlua/logo-64x64.png";

  luaEnv = lua.withPackages (_ps: chosenPackages);

  languageServers = lib.optionals settings.lsp.lua-language-server.enable
    [(callPackage ./language-server-lua {
      inherit attrs icon kernelName lua luaEnv;
      settings = settings.lsp.lua-language-server;
    })];

  repls = {
    lua = {
      display_name = displayName;
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
      inherit displayName attrs extensions lua luaEnv repls xeus-lua;
      enableVariableInspector = settings.misc.enableVariableInspector;
    })
    luaEnv
  ]
  ++ languageServers
  ;

  passthru = {
    meta = lua.meta // {
      baseName = kernelName;
      inherit displayName;
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
