{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.lean4 = {
      enable = mkOption {
        title = "Enable Lean 4 kernel";
        description = ''
          Cells run through leanprover-community/repl, which chains each cell's environment
          onto the previous one's. The language server elaborates the notebook separately,
          so the two can disagree if cells are re-run out of order.
        '';
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        title = "List of packages";
        description = ''
          Lean libraries from nixpkgs' `leanPackages`, put on the kernel's and the language
          server's search paths.

          All are prebuilt in the binary cache except `mathlib`, whose 8.7 GB output is
          over Hydra's upload limit (NixOS/nixpkgs#510957), so selecting it means compiling
          Mathlib locally for hours. nixpkgs master has since fixed that; drop this caveat
          on the next bump.
        '';
        type = types.listOf types.str;
        default = [];
        visible = false;
      };

      interface.attrs = mkOption {
        title = boilerplate.attrsTitle;
        description = boilerplate.attrsDescription;
        type = types.listOf types.str;
        default = ["lean4" "lean"];
      };
      interface.extensions = mkOption {
        title = boilerplate.extensionsTitle;
        description = boilerplate.extensionsDescription;
        type = types.listOf types.str;
        default = ["lean"];
      };

      lsp.lean.enable = mkOption {
        title = "Enable Lean language server";
        type = types.bool;
        default = true;
      };
    };
  };

  config = mkIf config.kernels.lean4.enable {
    builtKernels.lean4 = config.pkgs.callPackage ./. {
      settings = config.kernels.lean4;
      settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.lean4;
    };
  };
}
