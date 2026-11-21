{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  pkgsToUse = config.pkgs;

in

{
  options = {
    kernels.lua = {
      enable = mkOption {
        title = "Enable Lua kernel";
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        title = "List of packages";
        type = types.listOf types.str;
        default = [];
        visible = false;
      };

      interface.attrs = mkOption {
        title = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["lua"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["lua"];
      };

      lsp.lua-language-server.enable = mkOption {
        title = "Enable Lua language server";
        type = types.bool;
        default = true;
      };
      lsp.lua-language-server.format = mkOption {
        title = "Enable formatting";
        description = "Format Lua code with EmmyLuaCodeStyle, which is built into lua-language-server.";
        type = types.bool;
        default = true;
      };

      misc.enableVariableInspector = mkOption {
        title = "Enable the variable inspector";
        description = "This will show a summary of the currently defined variables in the UI.";
        type = types.bool;
        default = true;
      };
    };
  };

  config = mkIf config.kernels.lua.enable {
    builtKernels.lua = pkgsToUse.callPackage ./. {
      # sol2 in xeus-lua 0.8.0 supports up to Lua 5.4.
      lua = pkgsToUse.lua5_4;
      xeus-lua = pkgsToUse.callPackage ./xeus-lua.nix { lua = pkgsToUse.lua5_4; };
      settings = config.kernels.lua;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.lua;
    };
  };
}
