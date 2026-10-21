{ lua
, luaEnv
}:

# Where the selected subpackages live, in the two formats that need to agree.
#
# The kernel gets `LUA_PATH` / `LUA_CPATH`, which is what luaL_openlibs reads when it
# builds package.path. The language server gets the same directories as the list of
# patterns its `runtime.path` setting wants. If these two drift, `require "foo"` resolves
# in a cell and shows as an unresolved module in the editor, or the other way round --
# so they're derived here from one list rather than written out twice.
#
# ";;" is Lua's own "splice the default path in here" marker, which is what nixpkgs' lua
# wrapper uses. Keeping it means a module that ships with the interpreter still resolves.

let
  v = lua.luaversion;

  luaPatterns = [
    "${luaEnv}/share/lua/${v}/?.lua"
    "${luaEnv}/share/lua/${v}/?/init.lua"
  ];

  cPatterns = ["${luaEnv}/lib/lua/${v}/?.so"];

in

{
  luaPath = ";;;" + builtins.concatStringsSep ";" luaPatterns;
  luaCPath = ";;;" + builtins.concatStringsSep ";" cPatterns;

  # The server resolves a bare `require "foo"` against the workspace too, so its own
  # relative patterns come first.
  serverRuntimePath = ["?.lua" "?/init.lua"] ++ luaPatterns;

  # Loading the directory as a library is what makes the server read the modules
  # themselves, rather than just knowing a path exists -- without it, hover on a
  # required module is `unknown`.
  serverLibrary = ["${luaEnv}/share/lua/${v}"];
}
