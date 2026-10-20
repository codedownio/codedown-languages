{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

{
  options = {
    kernels.lean4 = {
      enable = mkOption {
        title = "Enable Lean 4 kernel";
        description = ''
          Cells run through leanprover-community/repl, which chains each cell's environment
          onto the previous one's. The language server elaborates the concatenated notebook
          separately, so the two can disagree if cells are re-run out of order -- a `def`
          you deleted from a cell is still in the repl's environment chain until the kernel
          is restarted.
        '';
        type = types.bool;
        default = false;
        visible = false;
      };

      packages = mkOption {
        title = "List of packages";
        description = ''
          Lean libraries to put on the kernel's and the language server's search paths,
          from nixpkgs' `leanPackages`.

          All of them are prebuilt in the public binary cache except `mathlib`, on this
          nixpkgs. Mathlib is one Lake package of 7871 modules whose build output is
          8.7 GB, over the 4 GiB that Hydra will upload, so it never reached the cache
          (NixOS/nixpkgs#510957). Selecting it here means compiling Mathlib locally,
          which takes hours.

          Fixed upstream after our pin branched off: nixpkgs master builds a
          `mathlib__archive` whose modules are packed into per-module `.ltar`s with
          leantar, which comes to 1.7 GB and does get cached, and makes the user-facing
          derivation an unpack of it. Worth deleting this caveat on the next nixpkgs bump.
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
