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

  # `shutdown` must answer with a null result. lua-language-server returns `true`,
  # which is a different type, and a client that decodes the response strictly can't
  # match it to its request -- so the shutdown handshake never completes and the
  # client waits out its timeout before killing the process. (proto.lua turns a
  # handler that returns nothing into `"result": null`, so dropping the `return` is
  # the whole fix.) R's languageserver had the same bug with `[]`; see
  # FEATURE-MATRIX.md.
  #
  # Prepended, not appended: nixpkgs' postPatch ends with `pushd 3rd/luamake`, so
  # anything after it runs from the wrong directory.
  luaLanguageServer = lua-language-server.overrideAttrs (oldAttrs: {
    postPatch = ''
      # Drop the `return true` on the line after the shutdown log call. sed rather
      # than substituteInPlace because a Nix indented string strips the leading
      # whitespace that would have to be part of a multi-line match.
      sed -i "/log.info('Server shutdown.')/{n;/^ *return true$/d}" script/provider/provider.lua
      if grep -A1 "Server shutdown" script/provider/provider.lua | grep -q 'return true'; then
        echo "lua-language-server shutdown patch did not apply" >&2
        exit 1
      fi
    '' + (oldAttrs.postPatch or "");
  });

  # The nixpkgs wrapper points --logpath and --metapath at $XDG_CACHE_HOME or
  # $HOME/.cache. The server writes its generated standard-library metadata to the
  # latter on every start, so it has to be writable; a sandboxed notebook may not
  # have a writable HOME.
  luaLanguageServerWrapped = runCommand "lua-language-server-wrapped" {
    nativeBuildInputs = [makeWrapper];
  } ''
    mkdir -p $out/bin
    makeWrapper ${luaLanguageServer}/bin/lua-language-server $out/bin/lua-language-server \
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

common.writeTextDirWithMetaAndPassthru luaLanguageServer.meta passthru "lib/codedown/language-servers/lua-${kernelName}-lua-language-server.yaml" (lib.generators.toYAML {} [{
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
