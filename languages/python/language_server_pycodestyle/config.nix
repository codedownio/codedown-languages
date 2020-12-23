{stdenv, pkgs, python}:

with pkgs;
with stdenv.lib;

let
  shared = callPackage ../shared.nix { inherit python; pythonPackages = python.pkgs; };

  diagnostic-languageserver = (callPackage ../../../language_servers/diagnostic-languageserver/default.nix {})."diagnostic-languageserver-git+https://github.com/codedownio/diagnostic-languageserver.git#c8aeacf80d3be95581441b9d3e62ce040cfa41f4";

  # Make a special Python environment with all the default packages, so we can get a site-packages
  # path containing them all to pass to the language server
  pythonEnv = python.buildEnv.override {
    extraLibs = [python.pkgs.pycodestyle] ++ shared.defaultPackages python.pkgs;
  };

in

{
  config = {
    name = "pycodestyle";
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
  };
}
