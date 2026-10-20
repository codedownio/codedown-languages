{ lua
, luaEnv
}:

# The LUA_PATH / LUA_CPATH that make the selected subpackages importable.
#
# Both the kernel and the language server need these, and they have to agree, or
# `require "foo"` resolves in a notebook and shows as an unresolved module in the
# editor. ";;" is Lua's own "splice the default path in here" marker, which is what
# nixpkgs' own lua wrapper uses -- keeping it means a package that ships with the
# interpreter still resolves.

let
  v = lua.luaversion;

in

{
  luaPath = ";;;${luaEnv}/share/lua/${v}/?.lua;${luaEnv}/share/lua/${v}/?/init.lua";
  luaCPath = ";;;${luaEnv}/lib/lua/${v}/?.so";
}
