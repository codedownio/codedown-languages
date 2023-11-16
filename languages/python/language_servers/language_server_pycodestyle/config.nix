{ callPackage
, lib
, stdenv

, pythonWithPackages
, kernelName
, packages ? []
}:

let
  common = callPackage ../../../common.nix {};

  diagnostic-languageserver = (callPackage ../../../../language_servers/diagnostic-languageserver/default.nix {})
    ."diagnostic-languageserver-git+https://github.com/codedownio/diagnostic-languageserver.git#0171e0867e0c340c287bfd60c348425585e21eeb";

  python = pythonWithPackages (ps: [ps.pycodestyle]);

in

common.writeTextDirWithMeta python.pkgs.pycodestyle.meta "lib/codedown/language-servers/python-${kernelName}-pycodestyle.yaml"
  (lib.generators.toYAML {} [{
    name = "pycodestyle";
    version = python.pkgs.pycodestyle.version;
    display_name = "pycodestyle";
    description = python.pkgs.pycodestyle.meta.description;
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
        pycodestyle = {
          sourceName = "pycodestyle";
          command = "${python.pkgs.pycodestyle}/bin/pycodestyle";
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
            E = "error";
            W = "warning";
          };
          offsetColumn = 0;
          offsetLine = 0;
          formatLines = 1;
        };
      };
      filetypes = {
        py = "pycodestyle";
        md = "pycodestyle";
        ipynb = "pycodestyle";
        pycodestyle = "pycodestyle";
        python = "pycodestyle";
      };
      formatters = {};
      formatFiletypes = {};
    };
  }])
