{ codedown
, ...
}:

codedown.makeEnvironment {
  name = "lua";

  kernels.lua.enable = true;

  # One pure-Lua package and one C module, to cover LUA_PATH and LUA_CPATH.
  kernels.lua.packages = [
    "inspect"
    "luafilesystem"
  ];
}
