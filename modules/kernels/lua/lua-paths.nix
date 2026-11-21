{ lua
, luaEnv
}:

# One list of module paths, for both the kernel and the language server.

let
  v = lua.luaversion;

  luaPatterns = [
    "${luaEnv}/share/lua/${v}/?.lua"
    "${luaEnv}/share/lua/${v}/?/init.lua"
  ];

  cPatterns = ["${luaEnv}/lib/lua/${v}/?.so"];

in

{
  # ";;" splices in Lua's default path.
  luaPath = ";;;" + builtins.concatStringsSep ";" luaPatterns;
  luaCPath = ";;;" + builtins.concatStringsSep ";" cPatterns;

  serverRuntimePath = ["?.lua" "?/init.lua"] ++ luaPatterns;

  # Without this, hover on a required module is `unknown`.
  serverLibrary = ["${luaEnv}/share/lua/${v}"];
}
