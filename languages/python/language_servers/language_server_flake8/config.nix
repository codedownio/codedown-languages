{ callPackage
, lib
, stdenv
, pythonWithPackages

, kernelName
}:

let
  common = callPackage ../../common.nix {};

  diagnostic-languageserver = (callPackage ../../../../language_servers/diagnostic-languageserver/default.nix {})
    ."diagnostic-languageserver-git+https://github.com/codedownio/diagnostic-languageserver.git#0171e0867e0c340c287bfd60c348425585e21eeb";

  python = pythonWithPackages (ps: [ps.flake8]);

in

common.writeTextDirWithMeta python.pkgs.flake8.meta "lib/codedown/language-servers/python-${kernelName}-flake8.yaml"
  (lib.generators.toYAML {} [{
    name = "flake8";
    display_name = "Flake8";
    description = python.pkgs.flake8.meta.description;
    extensions = ["py"];
    notebook_suffix = ".py";
    kernel_name = kernelName;
    attrs = ["python"];
    type = "stream";
    args = ["${diagnostic-languageserver}/bin/diagnostic-languageserver" "--stdio" "--log-level" "1"];
    # Not sure whether to do this using an environment variable or initialization option
    env = {
      PYTHONPATH = lib.concatStringsSep ":" [
        "${python}/${python.sitePackages}"
        "/home/user/.local/${python.sitePackages}"
      ];
    };
    initialization_options = {
      linters = {
        flake8 = {
          sourceName = "flake8";
          command = "${python.pkgs.flake8}/bin/flake8";
          args = [
            "%file"
          ];
          formatPattern = [
            # TODO: include message code somehow?
            "^([^:]+):(\\d+):(\\d+): (.)(\\S+) (.*)"
            {
              line = 2;
              column = 3;
              security = 4;
              message = 6;
            }
          ];
          rootPatterns = [".git" "pyproject.toml" "setup.py"];
          securities = {
            E = "error"; # Errors? (typically from pycodestyle)
            W = "warning"; # Warnings? (typically from pycodestyle)
            C = "warning"; # cyclomatic complexity (from mccabe)
            F = "warning"; # flake8's own warnings
          };
          offsetColumn = 0;
          offsetLine = 0;
          formatLines = 1;
        };
      };
      filetypes = {
        py = "flake8";
        md = "flake8";
        ipynb = "flake8";
        flake8 = "flake8";
        python = "flake8";
      };
      formatters = {};
      formatFiletypes = {};
    };
  }])
