{ lib
, callPackage
, formats
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

  # Passed with --configpath, since codedown doesn't answer workspace/configuration.
  luarc = (formats.json {}).generate "luarc.json" {
    runtime = {
      version = "Lua ${v}";
      path = luaPaths.serverRuntimePath;
    };
    workspace = {
      library = luaPaths.serverLibrary;
      # Otherwise it prompts about third-party libraries, with no UI to answer.
      checkThirdParty = false;
    };
    # Defined by xeus-lua before any cell runs.
    diagnostics.globals = ["ilua" "pprint"];
    telemetry.enable = false;
    format.enable = settings.format;
  };

  # The server watches the config file's directory; a bare store path means all of /nix/store.
  luarcDir = runCommand "luarc" {} ''
    mkdir $out
    cp ${luarc} $out/luarc.json
  '';

  # Prepended because nixpkgs' postPatch ends in `pushd 3rd/luamake`.
  luaLanguageServer = lua-language-server.overrideAttrs (oldAttrs: {
    postPatch = ''
      # `shutdown` must answer null, not `true`.
      sed -i "/log.info('Server shutdown.')/{n;/^ *return true$/d}" script/provider/provider.lua
      if grep -A1 "Server shutdown" script/provider/provider.lua | grep -q 'return true'; then
        echo "lua-language-server shutdown patch did not apply" >&2
        exit 1
      fi

      # Stop reading stdin after `exit`, or os.exit waits for the client to close it.
      sed -i "s/^        brave.push('proto', proto)$/&\n        if proto.method == 'exit' then return end/" script/brave/work.lua
      grep -q "if proto.method == 'exit' then return end" script/brave/work.lua || {
        echo "lua-language-server exit patch did not apply" >&2
        exit 1
      }
    '' + (oldAttrs.postPatch or "");
  });

  languageServerName = "lua-language-server";

  passthru = {
    inherit languageServerName;
    # EmmyLuaCodeStyle is built into lua-language-server.
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
  notebook_suffix = ".lua";
  kernel_name = kernelName;
  header_lines = [];
  inherit attrs;
  type = "stream";
  primary = true;
  args = [
    "${luaLanguageServer}/bin/lua-language-server"
    "--configpath=${luarcDir}/luarc.json"
  ];
  env = {};
  language_id = "lua";
}])
