{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "lua";

  kernels.lua.enable = true;

  # inspect is pure Lua (found via LUA_PATH) and luafilesystem is a C module (found via
  # LUA_CPATH), so between them this covers both halves of the module search path.
  kernels.lua.packages = [
    "inspect"
    "luafilesystem"
  ];
}
