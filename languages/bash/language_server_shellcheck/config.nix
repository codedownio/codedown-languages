{stdenv, pkgs, python}:

with pkgs;
with pkgs.lib;

let
  shared = callPackage ../shared.nix { inherit python; pythonPackages = python.pkgs; };

  diagnostic-languageserver = (callPackage ../../../language_servers/diagnostic-languageserver/default.nix {})."diagnostic-languageserver-git+https://github.com/codedownio/diagnostic-languageserver.git#0171e0867e0c340c287bfd60c348425585e21eeb";

in

{
  config = {
    name = "shellcheck";
    extensions = ["sh" "bash"];
    attrs = ["bash"];
    type = "stream";
    args = ["${diagnostic-languageserver}/bin/diagnostic-languageserver" "--stdio" "--log-level" "1"];
    env = {

    };
    initialization_options = {
      linters = {
        shellcheck = {
          command = "${shellcheck}/bin/shellcheck";
          debounce = 100;
          args = [ "--format=gcc" "-"];
          offsetLine = 0;
          offsetColumn = 0;
          sourceName = "shellcheck";
          formatLines = 1;
          formatPattern = [
            "^[^:]+:(\\d+):(\\d+):\\s+([^:]+):\\s+(.*)$"
            {
              "line" = 1;
              "column" = 2;
              "message" = 4;
              "security" = 3;
            }
          ];
          securities = {
            error = "error";
            warning = "warning";
            note = "info";
          };
        };
      };
      filetypes = {
        py = "shellcheck";
        md = "shellcheck";
        ipynb = "shellcheck";
        shellcheck = "shellcheck";
        python = "shellcheck";
      };
      formatters = {};
      formatFiletypes = {};
    };
  };
}
