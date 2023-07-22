{ stdenv
, pkgs
, pythonWithPackages
, kernelName
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../../common.nix {};

  diagnostic-languageserver = (callPackage ../../../../language_servers/diagnostic-languageserver/default.nix {})
    ."diagnostic-languageserver-git+https://github.com/codedownio/diagnostic-languageserver.git#0171e0867e0c340c287bfd60c348425585e21eeb";

  python = pythonWithPackages (ps: [ps.pylint]);

in

common.writeTextDirWithMeta python.pkgs.pylint.meta "lib/codedown/language-servers/python-${kernelName}-pylint.yaml"
  (lib.generators.toYAML {} [{
    name = "pylint";
    display_name = "Pylint";
    description = python.pkgs.pylint.meta.description;
    icon = ./logo.png;
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
        pylint = {
          sourceName = "pylint";
          command = "${python.pkgs.pylint}/bin/pylint";
          args = [
            "--output-format"
            "text"
            "--score"
            "no"
            "--msg-template"
            "'{line}:{column}:{category}:{msg} ({msg_id}:{symbol})'"
            "--from-stdin"
            "myfile"
          ];
          formatPattern = [
            "^(\\d+?):(\\d+?):([a-z]+?):(.*)$"
            {
              line = 1;
              column = 2;
              security = 3;
              message = 4;
            }
          ];
          rootPatterns = [".git" "pyproject.toml" "setup.py"];
          securities = {
            informational = "hint";
            refactor = "info";
            convention = "info";
            warning = "warning";
            error = "error";
            fatal = "error";
          };
          offsetColumn = 1;
          formatLines = 1;
        };
      };
      filetypes = {
        py = "pylint";
        md = "pylint";
        ipynb = "pylint";
        pylint = "pylint";
        python = "pylint";
      };
      formatters = {};
      formatFiletypes = {};
    };
  }])
