{ config, options, lib, nixosOptionsToSettingsSchema, boilerplate, ... }:

with lib;

let
  subPackage = types.submodule {
    options = {
      name = mkOption {
        description = "Package name";
        type = types.str;
      };
      outputs = mkOption {
        title = "Package outputs to include";
        type = types.listOf types.str;
      };
      extras = mkOption {
        title = "Extras (optional-dependencies) to enable for the package";
        type = types.listOf types.str;
        default = [];
      };
    };
  };

  projectConfigNote = "A pyrightconfig.json or pyproject.toml in the project folder replaces both the type checking mode and the rule severities.";

  subPackageEvaluated = lib.evalModules {
    modules = [
      {
        options = {
          subPackage = {
            extras = mkOption {
              title = "Extras (optional-dependencies) to enable for the package";
              type = types.listOf types.str;
              default = [];
            };
          };
        };
      }
    ];
  };

in

{
  options.kernels.python3 = {
    enable = mkOption {
      title = "Enable Python 3 kernel";
      type = types.bool;
      default = false;
      visible = false;
    };

    packages = mkOption {
      title = "List of packages";
      type = types.listOf (types.either types.str subPackage);
      default = [];
      visible = false;
    };

    python3Package = mkOption {
      title = "Python interpreter";
      type = types.enum (lib.unique (
        [
          "python3"

          # "python310"
          "python312"
          "python313"
          "python314"

          "pypy3"
        ]
        # ++ (builtins.filter (n: builtins.match "^python3[0-9]*$" n != null) (builtins.attrNames config.pkgs))
      ));
      default = "python3";
    };

    interface.attrs = mkOption {
      title = boilerplate.attrsTitle;
      description = boilerplate.attrsDescription;
      type = types.listOf types.str;
      default = ["python3" "python"];
    };
    interface.extensions = mkOption {
      title = boilerplate.extensionsTitle;
      description = boilerplate.extensionsDescription;
      type = types.listOf types.str;
      default = ["py"];
    };

    lsp.jedi.enable = mkOption {
      title = "Enable Jedi language server";
      type = types.bool;
      default = false;
    };
    lsp.pyright.enable = mkOption {
      title = "Enable Pyright language server";
      type = types.bool;
      default = false;
    };
    lsp.pyright.disableLanguageServices = mkOption {
      title = "Disable Pyright's language services";
      description = "Use Pyright for diagnostics only, without its completions, hovers, and other features.";
      type = types.bool;
      default = true;
    };
    lsp.pyright.typeCheckingMode = mkOption {
      title = "Pyright type checking mode";
      description = "How strict Pyright is. ${projectConfigNote}";
      type = types.enum ["off" "basic" "standard" "strict"];
      default = "basic";
    };
    lsp.pyright.diagnosticSeverityOverrides = mkOption {
      title = "Pyright rule severities";
      description = "Severity for individual rules, overriding the type checking mode, e.g. reportUnusedExpression = \"none\". ${projectConfigNote}";
      type = types.attrsOf (types.enum ["none" "information" "warning" "error"]);
      default = {};
    };
    lsp.basedpyright.enable = mkOption {
      title = "Enable basedpyright language server";
      type = types.bool;
      default = false;
    };
    lsp.basedpyright.disableLanguageServices = mkOption {
      title = "Disable basedpyright's language services";
      description = "Use basedpyright for diagnostics only, without its completions, hovers, and other features.";
      type = types.bool;
      default = false;
    };
    lsp.basedpyright.typeCheckingMode = mkOption {
      title = "basedpyright type checking mode";
      description = "How strict basedpyright is. ${projectConfigNote}";
      type = types.enum ["off" "basic" "standard" "strict" "recommended" "all"];
      default = "standard";
    };
    lsp.basedpyright.diagnosticSeverityOverrides = mkOption {
      title = "basedpyright rule severities";
      description = "Severity for individual rules, overriding the type checking mode, e.g. reportUnusedExpression = \"none\". ${projectConfigNote}";
      type = types.attrsOf (types.enum ["none" "hint" "information" "warning" "error"]);
      default = {};
    };
    lsp.pyrefly.enable = mkOption {
      title = "Enable Pyrefly language server";
      type = types.bool;
      default = true;
    };
    lsp.pyrefly.disableLanguageServices = mkOption {
      title = "Disable Pyrefly's language services";
      description = "Use Pyrefly for diagnostics only, without its completions, hovers, and other features.";
      type = types.bool;
      default = false;
    };
    lsp.pyrefly.typeCheckingMode = mkOption {
      title = "Pyrefly type checking mode";
      description = "How strict Pyrefly is. \"auto\" migrates a mypy or Pyright config found in the project, and otherwise uses \"basic\". A pyrefly.toml or a [tool.pyrefly] section in pyproject.toml takes precedence.";
      type = types.enum ["auto" "off" "basic" "legacy" "default" "strict" "all"];
      default = "auto";
    };
    lsp.ruff.enable = mkOption {
      title = "Enable Ruff language server";
      description = "Linting, formatting and import sorting. It provides no completions or hovers, so pair it with another language server.";
      type = types.bool;
      default = true;
    };
    lsp.pylint.enable = mkOption {
      title = "Enable Pylint language server";
      type = types.bool;
      default = false;
    };
    lsp.flake8.enable = mkOption {
      title = "Enable Flake8 language server";
      type = types.bool;
      default = false;
    };
    lsp.pycodestyle.enable = mkOption {
      title = "Enable pycodestyle language server";
      type = types.bool;
      default = false;
    };
    lsp.microsoft.enable = mkOption {
      title = "Enable Microsoft Python language server";
      type = types.bool;
      default = false;
    };
    lsp.python-lsp-server.enable = mkOption {
      title = "Enable python-lsp-server language server";
      type = types.bool;
      default = false;
    };
    lsp.python-lsp-server.formatter = mkOption {
      title = "Python formatter";
      description = "Which formatter python-lsp-server uses.";
      type = types.enum ["autopep8" "yapf" "black" "ruff" "none"];
      default = "autopep8";
    };
    lsp.python-language-server.enable = mkOption {
      title = "Enable python-language-server language server";
      type = types.bool;
      default = false;
    };

    misc.permitUserSite = mkOption {
      title = "Permit user site-packages";
      description = "Skip setting the PYTHONNOUSERSITE variable. This will allow your Python code to import local packages (e.g. from ~/.local/lib). This is useful if you want to use pip to install Python packages independently of Nix.";
      type = types.bool;
      default = false;
    };
    misc.enableVariableInspector = mkOption {
      title = "Enable the variable inspector";
      description = "This will show a summary of the currently defined variables in the UI.";
      type = types.bool;
      default = true;
    };
  };

  config = mkIf config.kernels.python3.enable {
    builtKernels.python3 = let
      x = config.kernels.python3.python3Package;

      isPyPy = lib.hasPrefix "pypy" x;

      cpythonOverrides = _pyFinal: pyPrev: {
        # A reply written raw on the shell socket drains its descriptor edge, so a request
        # arriving at that moment is never dispatched and the shell channel wedges for good.
        ipykernel = pyPrev.ipykernel.overridePythonAttrs (old: {
          patches = (old.patches or []) ++ [ ./ipykernel-reply-through-shell-stream.patch ];
        });

        # Every pylint primer test drives one shared PRIMER_DIRECTORY and reads back the
        # comment.txt it writes there, so the pytest-xdist workers overwrite each other's
        # output and one of them reads an empty file. They only run on the single
        # interpreter pylint pins them to (PRIMER_CURRENT_INTERPRETER, currently 3.13), and
        # they cover pylint's own CI tooling rather than anything we ship.
        pylint = pyPrev.pylint.overridePythonAttrs (old: {
          disabledTestPaths = (old.disabledTestPaths or []) ++ [
            "tests/testutils/_primer/test_primer.py"
          ];
        });
      };

      pypyOverrides = pyFinal: pyPrev:
        # Test suites are the main thing standing between PyPy and a working kernel: a
        # package unsupported on PyPy throws when its outPath is evaluated even as a mere
        # check input, and several check inputs are C extensions that don't compile here.
        (lib.mapAttrs (_name: value:
          let isPythonPackage = builtins.tryEval (lib.isDerivation value && value ? overridePythonAttrs);
          in if isPythonPackage.success && isPythonPackage.value
             then value.overridePythonAttrs (_: { doCheck = false; })
             else value
        ) pyPrev)
        // {
          ipykernel = pyPrev.ipykernel.overridePythonAttrs (old: {
            doCheck = false;
            patches = (old.patches or []) ++ [ ./ipykernel-reply-through-shell-stream.patch ];
          });

          # On PyPy, pyzmq builds and runs against cffi rather than Cython, but nixpkgs
          # only wires up the Cython path.
          pyzmq = pyPrev.pyzmq.overridePythonAttrs (old: {
            doCheck = false;
            build-system = (old.build-system or []) ++ [ pyFinal.cffi pyFinal.pycparser ];
            dependencies = (old.dependencies or []) ++ [ pyFinal.cffi pyFinal.pycparser ];
          });
        };
    in
      config.pkgs.callPackage ./. {
        name = if isPyPy then "PyPy" else "Python";

        python3 = (lib.getAttr x config.pkgs).override {
          packageOverrides = if isPyPy then pypyOverrides else cpythonOverrides;
        };

        settings = config.kernels.python3;
        settingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 2; } options.kernels.python3;
        subPackageSettingsSchema = nixosOptionsToSettingsSchema { componentsToDrop = 1; } subPackageEvaluated.options.subPackage;
      };
  };
}
