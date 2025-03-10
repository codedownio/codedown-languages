{ config, options, lib, pkgs, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  subPackage = types.submodule {
    options = {
      name = mkOption {
        description = "Package name";
        type = types.str;
      };
      outputs = mkOption {
        example = "Package outputs to include";
        type = types.listOf types.str;
      };
    };
  };

  mkOptions = packageOption: {
    enable = mkOption {
      example = "Enable Python 3 kernel";
      type = types.bool;
      default = false;
      visible = false;
    };

    packages = mkOption {
      example = "List of packages";
      type = types.listOf (types.either types.str subPackage);
      default = [];
      visible = false;
    };

    python3Package = packageOption;

    interface.attrs = mkOption {
      example = boilerplate.attrsTitle;
      description = boilerplate.attrsDescription;
      type = types.listOf types.str;
      default = ["python3" "python"];
    };
    interface.extensions = mkOption {
      example = boilerplate.extensionsTitle;
      description = boilerplate.extensionsDescription;
      type = types.listOf types.str;
      default = ["py"];
    };

    lsp.jedi.enable = mkOption {
      example = "Enable Jedi language server";
      type = types.bool;
      default = true;
    };
    lsp.pyright.enable = mkOption {
      example = "Enable Pyright language server";
      type = types.bool;
      default = false;
    };
    lsp.pylint.enable = mkOption {
      example = "Enable Pylint language server";
      type = types.bool;
      default = false;
    };
    lsp.flake8.enable = mkOption {
      example = "Enable Flake8 language server";
      type = types.bool;
      default = false;
    };
    lsp.pycodestyle.enable = mkOption {
      example = "Enable pycodestyle language server";
      type = types.bool;
      default = false;
    };
    lsp.microsoft.enable = mkOption {
      example = "Enable Microsoft Python language server";
      type = types.bool;
      default = false;
    };
    lsp.python-lsp-server.enable = mkOption {
      example = "Enable python-lsp-server language server";
      type = types.bool;
      default = false;
    };
    lsp.python-language-server.enable = mkOption {
      example = "Enable python-language-server language server";
      type = types.bool;
      default = false;
    };

    misc.permitUserSite = mkOption {
      example = "Permit user site-packages";
      description = "Skip setting the PYTHONNOUSERSITE variable. This will allow your Python code to import local packages (e.g. from ~/.local/lib). This is useful if you want to use pip to install Python packages independently of Nix.";
      type = types.bool;
      default = false;
    };
    misc.enableVariableInspector = mkOption {
      example = "Enable the variable inspector";
      description = "This will show a summary of the currently defined variables in the UI.";
      type = types.bool;
      default = true;
    };
  };

in

{
  options = {
    kernels.python3 = mkOptions (mkOption {
      example = "Python 3 version";
      type = types.enum (lib.unique (
        [
          "python3"

          "python310"
          "python311"
          "python312"

          # These fail to build
          # "python313"
          # "python314"
        ]
        # ++ (builtins.filter (n: builtins.match "^python3[0-9]*$" n != null) (builtins.attrNames config.pkgs))
      ));
      default = "python3";
    });

    kernels.pypy3 = mkOptions (mkOption {
      example = "PyPy 3 version";
      type = types.enum (lib.unique (
        [
          "pypy3"
        ]
        # ++ (builtins.filter (n: builtins.match "^pypy3[0-9]*$" n != null) (builtins.attrNames config.pkgs))
      ));
      default = "pypy3";
    });
  };

  config = mkMerge [
    (mkIf config.kernels.python3.enable {
      builtKernels.python3 = let
        # Pythons that don't work with the ipykernel, ipywidgets, etc. of the Nixpkgs Python package set,
        # so we have a special package checked in under ./envs
        specialEnvPythons = {
          # "python38" = config.pkgs.python38;
          # "python39" = config.pkgs.python39;
          # "python312" = config.pkgs.python312;
        };

        x = config.kernels.python3.python3Package;

        basePython = lib.getAttr x config.pkgs;
          # if lib.hasAttr x specialEnvPythons then (config.pkgs.poetry2nix.mkPoetryEnv {
          #   projectDir = ./envs/${x};
          #   python = specialEnvPythons.${x};
          #   overrides = import ./envs/${x}/poetry-overrides.nix { inherit (config.pkgs) poetry2nix; };
          # }).overrideAttrs (_: { version = specialEnvPythons.${x}.version; })
          # else lib.getAttr x config.pkgs;
      in
        config.pkgs.callPackage ./. {
          name = "Python";

          python3 = basePython;

          settings = config.kernels.python3;
          settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.python3;
        };
    })

    (mkIf config.kernels.pypy3.enable {
      builtKernels.pypy3 =
        config.pkgs.callPackage ./. {
          name = "PyPy";

          python3 = lib.getAttr config.kernels.pypy3.python3Package config.pkgs;

          settings = config.kernels.pypy3;
          settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.pypy3;
        };
    })
  ];
}
