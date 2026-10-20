{ lib
, callPackage
, formats
, makeWrapper
, runCommand

, lua
, luaEnv
, lua-language-server

, attrs
, icon
, kernelName
, settings
}:

let
  common = callPackage ../../common.nix {};

  luaPaths = callPackage ../lua-paths.nix { inherit lua luaEnv; };

  v = lua.luaversion;

  # lua-language-server ignores initializationOptions -- it pulls configuration from
  # the client via workspace/configuration, or reads a .luarc.json. codedown doesn't
  # answer configuration requests, so hand it a file with --configpath instead.
  luarc = (formats.json {}).generate "luarc.json" {
    runtime = {
      version = "Lua ${v}";
      # Mirrors LUA_PATH in the kernel, so `require "foo"` resolving in a cell and
      # resolving in the editor are the same question.
      path = [
        "?.lua"
        "?/init.lua"
        "${luaEnv}/share/lua/${v}/?.lua"
        "${luaEnv}/share/lua/${v}/?/init.lua"
      ];
    };
    workspace = {
      library = ["${luaEnv}/share/lua/${v}"];
      # Otherwise the server prompts the user about "third party" libraries it
      # recognizes in the store path, and there's no UI here to answer with.
      checkThirdParty = false;
    };
    # xeus-lua injects these into the global table before any cell runs, so they are
    # defined even though nothing in the document declares them.
    diagnostics.globals = ["ilua" "pprint"];
    telemetry.enable = false;
    format.enable = settings.format;
  };

  # The nixpkgs wrapper points --logpath and --metapath at $XDG_CACHE_HOME or
  # $HOME/.cache. The server writes its generated standard-library metadata to the
  # latter on every start, so it has to be writable; a sandboxed notebook may not
  # have a writable HOME.
  luaLanguageServerWrapped = runCommand "lua-language-server-wrapped" {
    nativeBuildInputs = [makeWrapper];
  } ''
    mkdir -p $out/bin
    makeWrapper ${lua-language-server}/bin/lua-language-server $out/bin/lua-language-server \
      --set-default XDG_CACHE_HOME /tmp/lua-language-server-cache
  '';

  languageServerName = "lua-language-server";

  passthru = {
    inherit languageServerName;
    # EmmyLuaCodeStyle is vendored into lua-language-server, so formatting costs
    # nothing extra in the closure and is on unconditionally.
    formatters = lib.optionals settings.format ["EmmyLuaCodeStyle"];
  };

in

common.writeTextDirWithMetaAndPassthru lua-language-server.meta passthru "lib/codedown/language-servers/lua-${kernelName}-lua-language-server.yaml" (lib.generators.toYAML {} [{
  name = languageServerName;
  version = lua-language-server.version;
  display_name = "lua-language-server";
  description = lua-language-server.meta.description;
  inherit icon;
  icon_monochrome = ../lua-monochrome.svg;
  extensions = ["lua"];
  # A concatenation of Lua cells is a valid Lua chunk -- statements are legal at the
  # top level -- so the server can read the notebook directly, with no proxy.
  notebook_suffix = ".lua";
  kernel_name = kernelName;
  header_lines = [];
  inherit attrs;
  type = "stream";
  primary = true;
  args = [
    "${luaLanguageServerWrapped}/bin/lua-language-server"
    "--configpath=${luarc}"
  ];
  env = {};
  language_id = "lua";
}])
