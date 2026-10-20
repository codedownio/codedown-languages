{ callPackage
, lib
, makeWrapper
, runCommand
, writeText

, lua
, luaEnv
, xeus-lua

, enableVariableInspector

, attrs
, extensions
, repls ? {}
}:

let
  common = callPackage ../common.nix {};

  luaPaths = callPackage ./lua-paths.nix { inherit lua luaEnv; };

  variableInspector = {
    initial_code_path = writeText "lua_variable_inspector_init.lua"
      "dofile('${./variable_inspector.lua}')\n";
    list_variables_command = "__codedown_variable_inspector_list()";
    inspect_variable_command = "__codedown_variable_inspector_inspect('{{VARIABLE_NAME}}')";
  };

  # xeus-lua embeds Lua itself rather than shelling out to the interpreter, so the
  # selected packages reach it the way they reach any embedded Lua state: through
  # LUA_PATH / LUA_CPATH, which luaL_openlibs reads when it builds package.path.
  xlua = runCommand "xlua-wrapped" { nativeBuildInputs = [makeWrapper]; } ''
    mkdir -p $out/bin
    makeWrapper ${xeus-lua}/bin/xlua $out/bin/xlua \
      --set LUA_PATH ${lib.escapeShellArg luaPaths.luaPath} \
      --set LUA_CPATH ${lib.escapeShellArg luaPaths.luaCPath}
  '';

in

common.makeJupyterKernel {
  lua = {
    displayName = "Lua";
    argv = [
      "${xlua}/bin/xlua"
      "-f"
      "{connection_file}"
    ];
    language = "lua";
    logo32 = "${xeus-lua}/share/jupyter/kernels/xlua/logo-32x32.png";
    logo64 = "${xeus-lua}/share/jupyter/kernels/xlua/logo-64x64.png";
    metadata = {
      codedown = {
        inherit attrs extensions;

        language_version = lua.version;

        variable_inspector = if enableVariableInspector then variableInspector else null;

        repls = common.replsToMetadata "lua" repls;

        priority = 1;
      };
    };
  };
}
