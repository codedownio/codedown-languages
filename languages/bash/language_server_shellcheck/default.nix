{ stdenv
, pkgs
, python
}:

with pkgs;
with pkgs.lib;

let
  common = callPackage ../../common.nix {};

  diagnostic-languageserver = (callPackage ../../../language_servers/diagnostic-languageserver/default.nix {
    nodejs = pkgs.nodejs-14_x;
  })."diagnostic-languageserver-git+https://github.com/codedownio/diagnostic-languageserver.git#0171e0867e0c340c287bfd60c348425585e21eeb";

in

common.writeTextDirWithMeta shellcheck "lib/codedown/language-servers/bash-shellcheck.yaml" (lib.generators.toYAML {} [{
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
}])
