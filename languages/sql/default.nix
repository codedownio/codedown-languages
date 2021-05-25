with import <nixpkgs> {};
with python3Packages;
with writers;

rec {
  name = "sql";

  kernel = jupyter-kernel.create {
    definitions = {
      sql = {
        displayName = "SQL";
        argv = [
          "TODO"
          "{connection_file}"
        ];
        language = "sql";
        logo32 = ./logo-32x32.png;
        logo64 = ./logo-64x64.png;
        env = {};
        metadata = {
          codedown = {
            priority = 10;
          };
        };
      };
    };
  };

  modeInfo = writeTextDir "lib/codedown/sql-modes.yaml" (lib.generators.toYAML {} [{
    attrName = "sql";
    codeMirrorMode = "sql";
    extensionsToHighlight = ["sql"];
    extensionsToRun = ["sql"];
  }]);
}
