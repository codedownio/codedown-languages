{ stdenv
, pkgs
, python
, kernelName
, packages ? []
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../../common.nix {};

  diagnostic-languageserver = (callPackage ../../../language_servers/diagnostic-languageserver/default.nix {})."diagnostic-languageserver-git+https://github.com/codedownio/diagnostic-languageserver.git#0171e0867e0c340c287bfd60c348425585e21eeb";

  # Make a special Python environment with all the default packages, so we can get a site-packages
  # path containing them all to pass to the language server
  pythonEnv = python.buildEnv.override {
    extraLibs = [python.pkgs.pycodestyle] ++ packages;
  };

in

common.writeTextDirWithMeta python.pkgs.pycodestyle.meta "lib/codedown/python-pycodestyle-language-servers.yaml"
  (lib.generators.toYAML {} [{
    name = "pycodestyle";
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
        "${pythonEnv}/${pythonEnv.sitePackages}"
        "/home/user/.local/${pythonEnv.sitePackages}"
      ];
    };
    initialization_options = {
      linters = {
        pycodestyle = {
          sourceName = "pycodestyle";
          command = "${pythonEnv.pkgs.pycodestyle}/bin/pycodestyle";
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
