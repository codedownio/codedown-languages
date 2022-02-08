{ stdenv
, pkgs
, python
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../../common.nix {};

  diagnostic-languageserver = (callPackage ../../../language_servers/diagnostic-languageserver/default.nix {})."diagnostic-languageserver-git+https://github.com/codedownio/diagnostic-languageserver.git#0171e0867e0c340c287bfd60c348425585e21eeb";

  # Make a special Python environment with all the default packages, so we can get a site-packages
  # path containing them all to pass to the language server
  pythonEnv = python.buildEnv.override {
    extraLibs = [python.pkgs.flake8];
  };

in

common.writeTextDirWithMeta python.pkgs.flake8.meta "lib/codedown/python-flake8-language-servers.yaml"
  (lib.generators.toYAML {} [{
    name = "flake8";
    description = python.pkgs.flake8.meta.description;
    extensions = ["py"];
    attrs = ["python"];
    type = "stream";
    args = ["${diagnostic-languageserver}/bin/diagnostic-languageserver" "--stdio" "--log-level" "1"];
    # Not sure whether to do this using an environment variable or initialization option
    env = {
      PYTHONPATH = lib.concatStringsSep ":" [
        "${pythonEnv}/lib/python3.8/site-packages"
        "/home/user/.local/lib/python3.8/site-packages"
      ];
    };
    initialization_options = {
      linters = {
        flake8 = {
          sourceName = "flake8";
          command = "${pythonEnv.pkgs.flake8}/bin/flake8";
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
