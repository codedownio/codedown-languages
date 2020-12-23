{stdenv, pkgs, python}:

with pkgs;
with stdenv.lib;

let
  shared = callPackage ../shared.nix { inherit python; pythonPackages = python.pkgs; };

  diagnostic-languageserver = (callPackage ../../../language_servers/diagnostic-languageserver/default.nix {})."diagnostic-languageserver-git+https://github.com/codedownio/diagnostic-languageserver.git#c8aeacf80d3be95581441b9d3e62ce040cfa41f4";

  # Make a special Python environment with all the default packages, so we can get a site-packages
  # path containing them all to pass to the language server
  pythonEnv = python.buildEnv.override {
    extraLibs = [python.pkgs.pylint] ++ shared.defaultPackages python.pkgs;
  };

in

{
  config = {
    name = "pylint";
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
        pylint = {
          sourceName = "pylint";
          command = "${pythonEnv.pkgs.pylint}/bin/pylint";
          args = [
            "--output-format"
            "text"
            "--score"
            "no"
            "--msg-template"
            "'{line}:{column}:{category}:{msg} ({msg_id}:{symbol})'"
            "%file"
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
  };
}
