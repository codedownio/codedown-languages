let nixpkgs = import (import ../../../nix/pinned-nixpkgs.nix) {}; in

with nixpkgs;
with python3Packages;
with makeWrapper;
with writers;

rec {
  name = "sql";

  kernel = jupyter-kernel.create {
    definitions = {
      sql = {
        displayName = "SQL";
        argv = [
          "${runSqlKernel}/bin/run-sql-kernel.sh"
          "{connection_file}"
        ];
        language = "sql";
        logo32 = ./logo-32x32.png;
        logo64 = ./logo-64x64.png;
        env = {};
      };
    };
  };

  modeInfo = writeText "mode_config.yaml" (lib.generators.toYAML {} [{
    attrName = "sql";
    codeMirrorMode = "sql";
    extensionsToHighlight = ["sql"];
    extensionsToRun = ["sql"];
  }]);
}
